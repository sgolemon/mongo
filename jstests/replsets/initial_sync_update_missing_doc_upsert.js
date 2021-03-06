/**
 * Initial sync runs in several phases - the first 3 are as follows:
 * 1) fetches the last oplog entry (op_start1) on the source;
 * 2) copies all non-local databases from the source; and
 * 3) fetches and applies operations from the source after op_start1.
 *
 * This test upserts documents with both the "update" and "applyOps"
 * commands on the source between phases 2 and 3; these should be treated
 * as inserts on the syncing node.
 */

(function() {
load("jstests/replsets/libs/initial_sync_update_missing_doc.js");
load("jstests/libs/check_log.js");

var name = 'initial_sync_update_missing_doc_upsert';
var replSet = new ReplSetTest({
    name: name,
    nodes: 1,
});

replSet.startSet();
replSet.initiate();

const primary = replSet.getPrimary();
const dbName = 'test';

assert.commandWorked(primary.getDB(dbName).createCollection(name));
const coll = primary.getDB(dbName).getCollection(name);

// Add a secondary node with priority: 0 and votes: 0 so that we prevent elections while
// it is syncing from the primary.
const secondaryConfig = {
    rsConfig: {votes: 0, priority: 0}
};
const secondary = reInitiateSetWithSecondary(replSet, secondaryConfig);

jsTestLog("Allow initial sync to finish cloning collections");

assert.commandWorked(secondary.getDB('admin').runCommand(
    {configureFailPoint: 'initialSyncHangAfterDataCloning', mode: 'alwaysOn'}));
assert.commandWorked(secondary.getDB('admin').runCommand(
    {configureFailPoint: 'initialSyncHangBeforeCopyingDatabases', mode: 'off'}));
checkLog.contains(secondary, "initialSyncHangAfterDataCloning fail point enabled");

jsTestLog('Use both "update" and "applyOps" to upsert doc on primary');

let documentIdCounter = 0;
let numDocuments = 0;

assert.commandWorked(coll.update({_id: documentIdCounter}, {x: 1}, {upsert: true}));
documentIdCounter++;
numDocuments++;

function applyOps({documentId, alwaysUpsert, allowAtomic}) {
    let command = {
        applyOps: [{op: "u", ns: coll.getFullName(), o2: {_id: documentId}, o: {$set: {x: 1}}}]
    };

    if (alwaysUpsert !== null) {
        command['alwaysUpsert'] = alwaysUpsert;
    }

    if (allowAtomic !== null) {
        command['allowAtomic'] = allowAtomic;
    }

    assert.commandWorked(primary.getDB(dbName).runCommand(command));
}

/* alwaysUpsert is true by default; test with the default value and an explicit value */
for (let alwaysUpsert of [null, true]) {
    /* If allowAtomic is true (the default), this writes an applyOps oplog entry containing an
     * op: 'u' sub-entry, otherwise it writes a regular op: 'u' entry. The update is treated as
     * an upsert by the primary. Ensure it is treated that way by the secondary when it applies
     * the oplog entry during initial sync.
     */
    for (let allowAtomic of [null, true, false]) {
        applyOps(
            {documentId: documentIdCounter, alwaysUpsert: alwaysUpsert, allowAtomic: allowAtomic});
        documentIdCounter++;
        numDocuments++;
    }
}

/* The interesting scenario for alwaysUpsert: false is if the document is deleted on the primary
 * after updating. When the secondary attempts to apply the oplog entry during initial sync,
 * it will fail to update, and fail to fetch the missing document. Ensure that initial sync
 * proceeds anyway.
 */
for (let allowAtomic of [null, true, false]) {
    coll.insertOne({_id: documentIdCounter});
    applyOps({documentId: documentIdCounter, alwaysUpsert: false, allowAtomic: allowAtomic});
    coll.deleteOne({_id: documentIdCounter});
    // Don't increment numDocuments, since we deleted the document we just inserted.
    documentIdCounter++;
}

jsTestLog("Allow initial sync to finish fetching and replaying oplog");

assert.commandWorked(secondary.getDB('admin').runCommand(
    {configureFailPoint: 'initialSyncHangBeforeGettingMissingDocument', mode: 'off'}));
assert.commandWorked(secondary.getDB('admin').runCommand(
    {configureFailPoint: 'initialSyncHangAfterDataCloning', mode: 'off'}));
assert.commandWorked(secondary.getDB('admin').runCommand(
    {configureFailPoint: 'initialSyncHangBeforeGettingMissingDocument', mode: 'off'}));

checkLog.contains(secondary, 'initial sync done');

const replStatus = assert.commandWorked(secondary.adminCommand({replSetGetStatus: 1}));
const firstOplogEnd = replStatus.initialSyncStatus.initialSyncOplogEnd;

// *No* missing documents were fetched.
finishAndValidate(replSet, name, firstOplogEnd, 0 /* numFetched */, numDocuments);

replSet.stopSet();
})();
