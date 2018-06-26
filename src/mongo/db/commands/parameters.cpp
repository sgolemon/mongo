// parameters.cpp

/**
*    Copyright (C) 2012 10gen Inc.
*
*    This program is free software: you can redistribute it and/or  modify
*    it under the terms of the GNU Affero General Public License, version 3,
*    as published by the Free Software Foundation.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Affero General Public License for more details.
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*    As a special exception, the copyright holders give permission to link the
*    code of portions of this program with the OpenSSL library under certain
*    conditions as described in each individual source file and distribute
*    linked combinations including the program with the OpenSSL library. You
*    must comply with the GNU Affero General Public License in all respects for
*    all of the code used other than as permitted herein. If you modify file(s)
*    with this exception, you may extend this exception to your version of the
*    file(s), but you are not obligated to do so. If you do not wish to do so,
*    delete this exception statement from your version. If you delete this
*    exception statement from all source files in the program, then also delete
*    it in the license file.
*/

#include "mongo/platform/basic.h"

#include "mongo/db/commands/parameters.h"

#include <set>

#include "mongo/bson/json.h"
#include "mongo/client/replica_set_monitor.h"
#include "mongo/config.h"
#include "mongo/db/auth/authorization_manager.h"
#include "mongo/db/auth/internal_user_auth.h"
#include "mongo/db/command_generic_argument.h"
#include "mongo/db/commands.h"
#include "mongo/db/server_parameters.h"
#include "mongo/db/storage/storage_options.h"
#include "mongo/logger/parse_log_component_settings.h"
#include "mongo/util/mongoutils/str.h"

using std::string;
using std::stringstream;

namespace mongo {

namespace {
void appendParameterNames(std::string* help) {
    *help += "supported:\n";
    for (const auto& kv : ServerParameterSet::getGlobal()->getMap()) {
        *help += "  ";
        *help += kv.first;
        *help += '\n';
    }
}
}  // namespace

class CmdGet : public ErrmsgCommandDeprecated {
public:
    CmdGet() : ErrmsgCommandDeprecated("getParameter") {}
    AllowedOnSecondary secondaryAllowed(ServiceContext*) const override {
        return AllowedOnSecondary::kAlways;
    }
    virtual bool adminOnly() const {
        return true;
    }
    virtual bool supportsWriteConcern(const BSONObj& cmd) const override {
        return false;
    }
    virtual void addRequiredPrivileges(const std::string& dbname,
                                       const BSONObj& cmdObj,
                                       std::vector<Privilege>* out) const {
        ActionSet actions;
        actions.addAction(ActionType::getParameter);
        out->push_back(Privilege(ResourcePattern::forClusterResource(), actions));
    }
    std::string help() const override {
        std::string h =
            "get administrative option(s)\nexample:\n"
            "{ getParameter:1, notablescan:1 }\n";
        appendParameterNames(&h);
        h += "{ getParameter:'*' } to get everything\n";
        return h;
    }
    bool errmsgRun(OperationContext* opCtx,
                   const string& dbname,
                   const BSONObj& cmdObj,
                   string& errmsg,
                   BSONObjBuilder& result) {
        bool all = *cmdObj.firstElement().valuestrsafe() == '*';

        int before = result.len();

        const ServerParameter::Map& m = ServerParameterSet::getGlobal()->getMap();
        for (ServerParameter::Map::const_iterator i = m.begin(); i != m.end(); ++i) {
            if (all || cmdObj.hasElement(i->first.c_str())) {
                i->second->append(opCtx, result, i->second->name());
            }
        }

        if (before == result.len()) {
            errmsg = "no option found to get";
            return false;
        }
        return true;
    }
} cmdGet;

class CmdSet : public ErrmsgCommandDeprecated {
public:
    CmdSet() : ErrmsgCommandDeprecated("setParameter") {}
    AllowedOnSecondary secondaryAllowed(ServiceContext*) const override {
        return AllowedOnSecondary::kAlways;
    }
    virtual bool adminOnly() const {
        return true;
    }
    virtual bool supportsWriteConcern(const BSONObj& cmd) const override {
        return false;
    }
    virtual void addRequiredPrivileges(const std::string& dbname,
                                       const BSONObj& cmdObj,
                                       std::vector<Privilege>* out) const {
        ActionSet actions;
        actions.addAction(ActionType::setParameter);
        out->push_back(Privilege(ResourcePattern::forClusterResource(), actions));
    }
    std::string help() const override {
        std::string h =
            "set administrative option(s)\n"
            "{ setParameter:1, <param>:<value> }\n";
        appendParameterNames(&h);
        return h;
    }
    bool errmsgRun(OperationContext* opCtx,
                   const string& dbname,
                   const BSONObj& cmdObj,
                   string& errmsg,
                   BSONObjBuilder& result) {
        int numSet = 0;
        bool found = false;

        const ServerParameter::Map& parameterMap = ServerParameterSet::getGlobal()->getMap();

        // First check that we aren't setting the same parameter twice and that we actually are
        // setting parameters that we have registered and can change at runtime
        BSONObjIterator parameterCheckIterator(cmdObj);

        // We already know that "setParameter" will be the first element in this object, so skip
        // past that
        parameterCheckIterator.next();

        // Set of all the parameters the user is attempting to change
        std::map<std::string, BSONElement> parametersToSet;

        // Iterate all parameters the user passed in to do the initial validation checks,
        // including verifying that we are not setting the same parameter twice.
        while (parameterCheckIterator.more()) {
            BSONElement parameter = parameterCheckIterator.next();
            std::string parameterName = parameter.fieldName();
            if (isGenericArgument(parameterName))
                continue;

            ServerParameter::Map::const_iterator foundParameter = parameterMap.find(parameterName);

            // Check to see if this is actually a valid parameter
            if (foundParameter == parameterMap.end()) {
                errmsg = str::stream() << "attempted to set unrecognized parameter ["
                                       << parameterName << "], use help:true to see options ";
                return false;
            }

            // Make sure we are allowed to change this parameter
            if (!foundParameter->second->allowedToChangeAtRuntime()) {
                errmsg = str::stream() << "not allowed to change [" << parameterName
                                       << "] at runtime";
                return false;
            }

            // Make sure we are only setting this parameter once
            if (parametersToSet.count(parameterName)) {
                errmsg = str::stream()
                    << "attempted to set parameter [" << parameterName
                    << "] twice in the same setParameter command, "
                    << "once to value: [" << parametersToSet[parameterName].toString(false)
                    << "], and once to value: [" << parameter.toString(false) << "]";
                return false;
            }

            parametersToSet[parameterName] = parameter;
        }

        // Iterate the parameters that we have confirmed we are setting and actually set them.
        // Not that if setting any one parameter fails, the command will fail, but the user
        // won't see what has been set and what hasn't.  See SERVER-8552.
        for (std::map<std::string, BSONElement>::iterator it = parametersToSet.begin();
             it != parametersToSet.end();
             ++it) {
            BSONElement parameter = it->second;
            std::string parameterName = it->first;

            ServerParameter::Map::const_iterator foundParameter = parameterMap.find(parameterName);

            if (foundParameter == parameterMap.end()) {
                errmsg = str::stream() << "Parameter: " << parameterName << " that was "
                                       << "avaliable during our first lookup in the registered "
                                       << "parameters map is no longer available.";
                return false;
            }

            if (numSet == 0) {
                foundParameter->second->append(opCtx, result, "was");
            }

            uassertStatusOK(foundParameter->second->set(parameter));
            numSet++;
        }

        if (numSet == 0 && !found) {
            errmsg = "no option found to set, use help:true to see options ";
            return false;
        }

        return true;
    }
} cmdSet;

namespace {
using logger::globalLogDomain;
using logger::LogComponent;
using logger::LogComponentSetting;
using logger::LogSeverity;
using logger::parseLogComponentSettings;

mutablebson::Element _getParentElement(mutablebson::Document& doc, LogComponent component) {
    // Hide LogComponent::kDefault
    if (component == LogComponent::kDefault) {
        return doc.end();
    }
    LogComponent parentComponent = component.parent();

    // Attach LogComponent::kDefault children to root
    if (parentComponent == LogComponent::kDefault) {
        return doc.root();
    }
    mutablebson::Element grandParentElement = _getParentElement(doc, parentComponent);
    return grandParentElement.findFirstChildNamed(parentComponent.getShortName());
}

void _getLogComponentVerbositySetting(BSONObj* output) {
    // The "default" log component is an implementation detail. Don't expose this to users.
    static const std::string defaultLogComponentName =
        LogComponent(LogComponent::kDefault).getShortName();

    mutablebson::Document doc;

    for (int i = 0; i < int(LogComponent::kNumLogComponents); ++i) {
        LogComponent component = static_cast<LogComponent::Value>(i);

        int severity = -1;
        if (globalLogDomain()->hasMinimumLogSeverity(component)) {
            severity = globalLogDomain()->getMinimumLogSeverity(component).toInt();
        }

        // Save LogComponent::kDefault LogSeverity at root
        if (component == LogComponent::kDefault) {
            uassertStatusOK(doc.root().appendInt("verbosity", severity));
            continue;
        }

        mutablebson::Element element = doc.makeElementObject(component.getShortName());
        uassertStatusOK(element.appendInt("verbosity", severity));

        mutablebson::Element parentElement = _getParentElement(doc, component);
        uassertStatusOK(parentElement.pushBack(element));
    }

    BSONObj result = doc.getObject();
    output->swap(result);
    invariant(!output->hasField(defaultLogComponentName));
}

/**
 * Updates component hierarchy log levels.
 *
 * BSON Format:
 * {
 *     verbosity: 4,  <-- maps to 'default' log component.
 *     componentA: {
 *         verbosity: 2,  <-- sets componentA's log level to 2.
 *         componentB: {
 *             verbosity: 1, <-- sets componentA.componentB's log level to 1.
 *         }
 *         componentC: {
 *             verbosity: -1, <-- clears componentA.componentC's log level so that
 *                                its final loglevel will be inherited from componentA.
 *         }
 *     },
 *     componentD : 3  <-- sets componentD's log level to 3 (alternative to
 *                         subdocument with 'verbosity' field).
 * }
 *
 * For the default component, the log level is read from the top-level
 * "verbosity" field.
 * For non-default components, we look up the element using the component's
 * dotted name. If the "<dotted component name>" field is a number, the log
 * level will be read from the field's value.
 * Otherwise, we assume that the "<dotted component name>" field is an
 * object with a "verbosity" field that holds the log level for the component.
 * The more verbose format with the "verbosity" field is intended to support
 * setting of log levels of both parent and child log components in the same
 * BSON document.
 *
 * Ignore elements in BSON object that do not map to a log component's dotted
 * name.
 */
Status _setLogComponentVerbositySetting(const BSONObj& bsonSettings) {
    auto swSettings = parseLogComponentSettings(bsonSettings);
    if (!swSettings.isOK()) {
        return swSettings.getStatus();
    }

    auto settings = std::move(swSettings.getValue());
    for (const auto& newSetting : settings) {
        // Negative value means to clear log level of component.
        if (newSetting.level < 0) {
            globalLogDomain()->clearMinimumLoggedSeverity(newSetting.component);
            continue;
        }
        // Convert non-negative value to Log()/Debug(N).
        LogSeverity newSeverity =
            (newSetting.level > 0) ? LogSeverity::Debug(newSetting.level) : LogSeverity::Log();
        globalLogDomain()->setMinimumLoggedSeverity(newSetting.component, newSeverity);
    }

    return Status::OK();
}

}  // namespace
}  // namespace mongo

void mongo::logComponentVerbositySettingAppendBSON(OperationContext* opCtx,
                                                   BSONObjBuilder& b,
                                                   const std::string& name) {
    BSONObj currentSettings;
    _getLogComponentVerbositySetting(&currentSettings);
    b << name << currentSettings;
}

mongo::Status mongo::logComponentVerbositySettingFromBSON(const BSONElement& newValueElement) {
    if (!newValueElement.isABSONObj()) {
        return {ErrorCodes::TypeMismatch,
                str::stream() << "log component verbosity is not a BSON object: "
                              << newValueElement};
    }
    return _setLogComponentVerbositySetting(newValueElement.Obj());
}

mongo::Status mongo::logComponentVerbositySettingFromString(const std::string& str) {
    try {
        return _setLogComponentVerbositySetting(mongo::fromjson(str));
    } catch (const DBException& ex) {
        return ex.toStatus();
    }
}

boost::synchronized_value<std::string> mongo::automationServiceDescriptor;
mongo::Status mongo::validateAutomationServiceDescriptor(const std::string& newValue) {
    if (newValue.size() > 64) {
        return {ErrorCodes::Overflow,
                "Value for parameter automationServiceDescription "
                "must be no more than 64 bytes"};
    }

    return Status::OK();
}

void mongo::logLevelAppendBSON(OperationContext* opCtx,
                               BSONObjBuilder& b,
                               const std::string& name) {
    b << name << globalLogDomain()->getMinimumLogSeverity().toInt();
}

mongo::Status mongo::logLevelFromBSON(const BSONElement& newValueElement) {
    int newValue;
    if (!newValueElement.coerce(&newValue) || newValue < 0) {
        return {ErrorCodes::BadValue,
                str::stream() << "Invalid value for logLevel: " << newValueElement};
    }
    LogSeverity newSeverity = (newValue > 0) ? LogSeverity::Debug(newValue) : LogSeverity::Log();
    globalLogDomain()->setMinimumLoggedSeverity(newSeverity);
    return Status::OK();
}

mongo::Status mongo::logLevelFromString(const std::string& str) {
    int newValue;
    Status status = parseNumberFromString(str, &newValue);
    if (!status.isOK()) {
        return status;
    }

    if (newValue < 0) {
        return {ErrorCodes::BadValue, str::stream() << "Invalid value for logLevel: " << newValue};
    }

    LogSeverity newSeverity = (newValue > 0) ? LogSeverity::Debug(newValue) : LogSeverity::Log();
    globalLogDomain()->setMinimumLoggedSeverity(newSeverity);
    return Status::OK();
}
