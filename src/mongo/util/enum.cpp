/**
 *    Copyright (C) 2019-present MongoDB, Inc.
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the Server Side Public License, version 1,
 *    as published by MongoDB, Inc.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    Server Side Public License for more details.
 *
 *    You should have received a copy of the Server Side Public License
 *    along with this program. If not, see
 *    <http://www.mongodb.com/licensing/server-side-public-license>.
 *
 *    As a special exception, the copyright holders give permission to link the
 *    code of portions of this program with the OpenSSL library under certain
 *    conditions as described in each individual source file and distribute
 *    linked combinations including the program with the OpenSSL library. You
 *    must comply with the Server Side Public License in all respects for
 *    all of the code used other than as permitted herein. If you modify file(s)
 *    with this exception, you may extend this exception to your version of the
 *    file(s), but you are not obligated to do so. If you do not wish to do so,
 *    delete this exception statement from your version. If you delete this
 *    exception statement from all source files in the program, then also delete
 *    it in the license file.
 */

#include "mongo/platform/basic.h"

#include "mongo/util/enum.h"

#define MONGO_LOG_DEFAULT_COMPONENT ::mongo::logger::LogComponent::kDefault

#include "mongo/base/status.h"
#include "mongo/util/log.h"
#include "mongo/util/options_parser/startup_option_init.h"
#include "mongo/util/options_parser/startup_options.h"
#include "mongo/util/quick_exit.h"
#include "mongo/util/string_map.h"

namespace mongo {
namespace {
StringMap<Enumerator*> enumerators;

class ListEnumerators : public Enumerator {
public:
    ListEnumerators() : Enumerator("list") {}

    BSONObj getEnumeration() final {
        BSONArrayBuilder b;
        for (const auto it : enumerators) {
            b.append(it.first);
        }
        return b.arr();
    }
} listEnumerators;

MONGO_STARTUP_OPTIONS_POST(Enum)(InitializerContext*) {
    const auto& params = optionenvironment::startupOptionsParsed;

    if (params.count("enum")) {
        const auto type = params["enum"].as<std::string>();

        auto it = enumerators.find(type);
        if (it == enumerators.end()) {
            error() << "Unknown enumeration '" << type << "'";
            quickExit(1);
        }

        auto data = it->second->getEnumeration();
        std::cout << data.jsonString(JsonStringFormat::Strict, 1, data.couldBeArray()) << std::endl;
        quickExit(0);
    }

    return Status::OK();
}
}  // namespace

Enumerator::Enumerator(StringData strName) {
    auto name = strName.toString();

    uassert(ErrorCodes::InternalError,
            str::stream() << "Duplicate enumerator '" << name << "'",
            enumerators.count(name) == 0);

    enumerators[std::move(name)] = this;
}

}  // namespace mongo
