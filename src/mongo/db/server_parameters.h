// server_parameters.h

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

#pragma once

#include <boost/thread/synchronized_value.hpp>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "mongo/base/static_assert.h"
#include "mongo/base/status.h"
#include "mongo/db/jsobj.h"
#include "mongo/platform/atomic_proxy.h"
#include "mongo/platform/atomic_word.h"
#include "mongo/platform/compiler.h"
#include "mongo/stdx/functional.h"
#include "mongo/stdx/mutex.h"
#include "mongo/util/stringutils.h"

namespace mongo {

class ServerParameterSet;
class OperationContext;

namespace server_parameter_detail {
template <typename T>
inline StatusWith<T> coerceFromString(const std::string& str) {
    T value;
    Status status = parseNumberFromString(str, &value);
    if (!status.isOK()) {
        return status;
    }
    return value;
}

template <>
inline StatusWith<bool> coerceFromString<bool>(const std::string& str) {
    if ((str == "1") || (str == "true")) {
        return true;
    }
    if ((str == "0") || (str == "false")) {
        return false;
    }
    return Status(ErrorCodes::BadValue, "Value is not a valid boolean");
}

template <>
inline StatusWith<std::string> coerceFromString<std::string>(const std::string& str) {
    return str;
}

template <>
inline StatusWith<std::vector<std::string>> coerceFromString<std::vector<std::string>>(
    const std::string& str) {
    std::vector<std::string> v;
    splitStringDelim(str, &v, ',');
    return v;
}

template <typename T>
inline std::string coerceToString(const T& val) {
    return str::stream() << val;
}

template <>
inline std::string coerceToString<bool>(const bool& val) {
    return val ? "true" : "false";
}

template <>
inline std::string coerceToString<std::string>(const std::string& val) {
    return val;
}

template <>
inline std::string coerceToString<std::vector<std::string>>(const std::vector<std::string>& val) {
    auto stream = str::stream();
    const char* delim = "";
    for (const auto& v : val) {
        stream << delim << v;
        delim = ",";
    }
    return stream;
}

}  // namespace server_parameter_detail

/**
 * Lets you make server level settings easily configurable.
 * Hooks into (set|get)Parameter, as well as command line processing
 *
 * NOTE: ServerParameters set at runtime can be read or written to at anytime, and are not
 * thread-safe without atomic types or other concurrency techniques.
 */
class ServerParameter {
public:
    typedef std::map<std::string, ServerParameter*> Map;

    ServerParameter(ServerParameterSet* sps,
                    const std::string& name,
                    bool allowedToChangeAtStartup,
                    bool allowedToChangeAtRuntime);
    ServerParameter(ServerParameterSet* sps, const std::string& name);
    virtual ~ServerParameter();

    std::string name() const {
        return _name;
    }

    /**
     * @return if you can set on command line or config file
     */
    bool allowedToChangeAtStartup() const {
        return _allowedToChangeAtStartup;
    }

    /**
     * @param if you can use (get|set)Parameter
     */
    bool allowedToChangeAtRuntime() const {
        return _allowedToChangeAtRuntime;
    }


    virtual void append(OperationContext* opCtx, BSONObjBuilder& b, const std::string& name) = 0;

    virtual Status set(const BSONElement& newValueElement) = 0;

    virtual Status setFromString(const std::string& str) = 0;

private:
    std::string _name;
    bool _allowedToChangeAtStartup;
    bool _allowedToChangeAtRuntime;
};

class ServerParameterSet {
public:
    typedef std::map<std::string, ServerParameter*> Map;

    void add(ServerParameter* sp);

    const Map& getMap() const {
        return _map;
    }

    static ServerParameterSet* getGlobal();

private:
    Map _map;
};

/**
 * Server Parameters can be set startup up and/or runtime.
 *
 * At startup, --setParameter ... or config file is used.
 * At runtime, { setParameter : 1, ...} is used.
 */
enum class ServerParameterType {

    /**
     * Parameter can only be set via runCommand.
     */
    kRuntimeOnly,

    /**
     * Parameter can only be set via --setParameter, and is only read at startup after command-line
     * parameters, and the config file are processed.
     */
    kStartupOnly,

    /**
     * Parameter can be set at both startup and runtime.
     */
    kStartupAndRuntime,
};

/**
 * Specialization of ServerParameter used by IDL generator.
 */
template <ServerParameterType paramType>
class IDLServerParameter : public ServerParameter {
private:
    using SPT = ServerParameterType;

public:
    IDLServerParameter(const std::string& name)
        : ServerParameter(ServerParameterSet::getGlobal(),
                          name,
                          paramType == SPT::kStartupOnly || paramType == SPT::kStartupAndRuntime,
                          paramType == SPT::kRuntimeOnly || paramType == SPT::kStartupAndRuntime) {}

    /**
     * Define a callback for populating a BSONObj with the current setting.
     */
    using appendBSON_t = void(OperationContext*, BSONObjBuilder&, const std::string&);
    void setAppendBSON(std::function<appendBSON_t> appendBSON) {
        _appendBSON = std::move(appendBSON);
    }

    /**
     * Encode the setting into BSON object.
     *
     * Typically invoked by {getParameter:...} to produce a dictionary
     * of SCP settings.
     */
    void append(OperationContext* opCtx, BSONObjBuilder& b, const std::string& name) override {
        invariant(_appendBSON);
        _appendBSON(opCtx, b, name);
    }

    /**
     * Define a callback for setting the value from a BSONElement.
     */
    using fromBSON_t = Status(const BSONElement&);
    void setFromBSON(std::function<fromBSON_t> fromBSON) {
        _fromBSON = std::move(fromBSON);
    }

    /**
     * Update the underlying value using a BSONElement
     *
     * Allows setting non-basic values (e.g. vector<string>)
     * via the {setParameter: ...} call.
     */
    Status set(const BSONElement& newValueElement) override {
        invariant(_fromBSON);
        return _fromBSON(newValueElement);
    }

    /**
     * Define a callback for setting the value from a string.
     */
    using fromString_t = Status(const std::string&);
    void setFromString(std::function<fromString_t> fromString) {
        _fromString = std::move(fromString);
    }

    /**
     * Update the underlying value from a string.
     *
     * Typically invoked from commandline --setParameter usage.
     */
    Status setFromString(const std::string& str) override {
        invariant(_fromString);
        return _fromString(str);
    }

protected:
    std::function<appendBSON_t> _appendBSON;
    std::function<fromBSON_t> _fromBSON;
    std::function<fromString_t> _fromString;
};

template <typename T, ServerParameterType paramType>
class IDLServerParameterWithStorage : public IDLServerParameter<paramType> {
private:
    template <typename U>
    struct storage_wrapper;

    template <typename U>
    struct storage_wrapper<std::atomic<U>> {
        static constexpr bool thread_safe = true;
        using type = U;
        static void store(std::atomic<U>& storage, const U& value) {
            storage.store(value);
        }
        static U load(std::atomic<U>& storage) {
            return storage.load();
        }
    };

    template <typename U>
    struct storage_wrapper<AtomicWord<U>> {
        static constexpr bool thread_safe = true;
        using type = U;
        static void store(AtomicWord<U>& storage, const U& value) {
            storage.store(value);
        }
        static U load(AtomicWord<U>& storage) {
            return storage.load();
        }
    };

    // Covers AtomicDouble
    template <typename U, typename P>
    struct storage_wrapper<AtomicProxy<U, P>> {
        static constexpr bool thread_safe = true;
        using type = U;
        static void store(AtomicProxy<U, P>& storage, const U& value) {
            storage.store(value);
        }
        static U load(AtomicProxy<U, P>& storage) {
            return storage.load();
        }
    };

    template <typename U>
    struct storage_wrapper<boost::synchronized_value<U>> {
        static constexpr bool thread_safe = true;
        using type = U;
        static void store(boost::synchronized_value<U>& storage, const U& value) {
            *storage = value;
        }
        static U load(boost::synchronized_value<U>& storage) {
            return *storage;
        }
    };

    // All other types
    template <typename U>
    struct storage_wrapper {
        static constexpr bool thread_safe = false;
        using type = U;
        static void store(U& storage, const U& value) {
            storage = value;
        }
        static U load(U& storage) {
            return storage;
        }
    };

    static constexpr bool thread_safe = storage_wrapper<T>::thread_safe;
    using element_type = typename storage_wrapper<T>::type;
    using SPT = ServerParameterType;

public:
    IDLServerParameterWithStorage(const std::string& name, T& storage)
        : IDLServerParameter<paramType>(name), _storage(storage) {
        invariant(thread_safe || paramType == SPT::kStartupOnly,
                  "Runtime server parameters must be thread safe");
    }

    /**
     * Encode the setting into BSON object.
     *
     * Typically invoked by {getParameter:...} to produce a dictionary
     * of SCP settings.
     */
    void append(OperationContext* opCtx, BSONObjBuilder& b, const std::string& name) final {
        if (this->_appendBSON) {
            this->_appendBSON(opCtx, b, name);
        } else {
            b.append(name, getValue());
        }
    }

    /**
     * Update the underlying value using a BSONElement
     *
     * Allows setting non-basic values (e.g. vector<string>)
     * via the {setParameter: ...} call.
     */
    Status set(const BSONElement& newValueElement) final {
        element_type newValue;

        if (this->_fromBSON) {
            return this->_fromBSON(newValueElement);
        } else if (newValueElement.coerce(&newValue)) {
            return setValue(newValue);
        } else {
            return Status(ErrorCodes::BadValue, "Can't coerce value");
        }
    }

    /**
     * Update the underlying value from a string.
     *
     * Typically invoked from commandline --setParameter usage.
     */
    Status setFromString(const std::string& str) final {
        if (this->_fromString) {
            return this->_fromString(str);
        }

        auto swNewValue = server_parameter_detail::coerceFromString<element_type>(str);
        if (!swNewValue.isOK()) {
            return swNewValue.getStatus();
        }

        return setValue(swNewValue.getValue());
    }

    /**
     * Set the value by natice type.
     * Also used for setting initial/default value.
     */
    Status setValue(const element_type& newValue) {
        for (const auto& validator : _validators) {
            const auto status = validator(newValue);
            if (!status.isOK()) {
                return status;
            }
        }
        storage_wrapper<T>::store(_storage, newValue);

        if (_onUpdate) {
            return _onUpdate(newValue);
        }

        return Status::OK();
    }

    element_type getValue() const {
        return storage_wrapper<T>::load(_storage);
    }

    /**
     * Called *after* updating the underlying storage to its new value.
     */
    using onUpdate_t = Status(const element_type&);
    void setOnUpdate(std::function<onUpdate_t> onUpdate) {
        _onUpdate = std::move(onUpdate);
    }

    // Validators.

    /**
     * Add a callback validator to be invoked when this setting is updated.
     *
     * Callback should return Status::OK() or ErrorCodes::BadValue.
     */
    using validator_t = Status(const element_type&);
    void addValidator(std::function<validator_t> validator) {
        _validators.push_back(std::move(validator));
    }

    /**
     * Specific (typically numeric) bounds on acceptable values.
     * Example range of (5,10]:
     *   scp.addBound<predicate::GT>(5).addBound<predicate::LTE>(10);
     */
    template <typename Predicate>
    void addBound(const element_type& bound) {
        addValidator([this, bound](const element_type& newValue) -> Status {
            if (!Predicate::evaluate(newValue, bound)) {
                return {ErrorCodes::BadValue, str::stream() << newValue};
            }
            return Status::OK();
        });
    }

    /**
     * Limit setting to a specific set of possible values.
     *
     * Accepts unnormalized string form from IDL generator
     * and promotes to specific element_type at startup.
     */
    template <typename element_type_t = element_type,
              std::enable_if_t<std::is_same<std::vector<std::string>, element_type_t>::value>>
    void setEnumValues(const std::set<std::string>& values) {
        // TODO: Use `if constexpr` instead of this enable_if SFINAE crap.
        addValidator([&values](const std::vector<std::string>& newValue) -> Status {
            for (const auto& value : newValue) {
                if (!values.count(value)) {
                    return {ErrorCodes::BadValue, str::stream() << newValue};
                }
            }
            return Status::OK();
        });
    }

    template <typename element_type_t = element_type,
              std::enable_if_t<!std::is_same<std::vector<std::string>, element_type_t>::value>>
    void setEnumValues(const std::set<std::string>& values) {
        std::set<element_type> coercedValues;
        if (std::is_same<element_type, std::string>::value) {
            coercedValues = values;
        } else {
            for (const auto& value : values) {
                auto swValue = server_parameter_detail::coerceFromString<element_type>(value);
                invariant(swValue.isOK());
                coercedValues.emplace(swValue.getValue());
            }
        }

        addValidator([&coercedValues](const element_type& newValue) -> Status {
            if (coercedValues.count(newValue)) {
                return Status::OK();
            }
            return {ErrorCodes::BadValue, str::stream() << newValue};
        });
    }

private:
    T& _storage;

    std::vector<std::function<validator_t>> _validators;
    std::function<onUpdate_t> _onUpdate;
};

class ServerParameterAlias : ServerParameter {
public:
    ServerParameterAlias(StringData name, ServerParameter* primary, bool deprecated = false)
        : ServerParameter(ServerParameterSet::getGlobal(),
                          primary->name(),
                          primary->allowedToChangeAtStartup(),
                          primary->allowedToChangeAtRuntime()),
          _primary(primary),
          _deprecated(deprecated) {
        invariant(_primary);
    }

    void append(OperationContext* opCtx, BSONObjBuilder& b, const std::string& name) final {
        _primary->append(opCtx, b, name);
    }

    Status set(const BSONElement& newValueElement) final;
    Status setFromString(const std::string& str) final;

private:
    ServerParameter* _primary;
    bool _deprecated;
};

/**
 * Lets you make server level settings easily configurable.
 * Hooks into (set|get)Parameter, as well as command line processing
 */
template <typename T>
class BoundServerParameter : public ServerParameter {
private:
    using setter = stdx::function<Status(const T&)>;
    using getter = stdx::function<T()>;
    using SPT = ServerParameterType;

public:
    BoundServerParameter(const std::string& name,
                         const setter set,
                         const getter get,
                         SPT paramType = SPT::kStartupOnly)
        : BoundServerParameter(ServerParameterSet::getGlobal(), name, set, get, paramType) {}

    BoundServerParameter(ServerParameterSet* sps,
                         const std::string& name,
                         const setter set,
                         const getter get,
                         SPT paramType = SPT::kStartupOnly)
        : ServerParameter(sps,
                          name,
                          paramType == SPT::kStartupOnly || paramType == SPT::kStartupAndRuntime,
                          paramType == SPT::kRuntimeOnly || paramType == SPT::kStartupAndRuntime),
          _setter(set),
          _getter(get) {}
    ~BoundServerParameter() override = default;

    void append(OperationContext* opCtx, BSONObjBuilder& b, const std::string& name) override {
        b.append(name, _getter());
    }

    Status set(const BSONElement& newValueElement) override {
        T newValue;

        if (!newValueElement.coerce(&newValue)) {
            return Status(ErrorCodes::BadValue, "Can't coerce value");
        }

        return _setter(newValue);
    }

    Status setFromString(const std::string& str) override {
        auto swVal = server_parameter_detail::coerceFromString<T>(str);
        if (!swVal.isOK()) {
            return swVal.getStatus();
        }
        return _setter(swVal.getValue());
    }

private:
    const setter _setter;
    const getter _getter;
};

template <typename T>
class LockedServerParameter : public BoundServerParameter<T> {
private:
    using SPT = ServerParameterType;

public:
    LockedServerParameter(const std::string& name,
                          const T& initval,
                          SPT paramType = SPT::kStartupAndRuntime)
        : LockedServerParameter(ServerParameterSet::getGlobal(), name, initval, paramType) {}

    LockedServerParameter(ServerParameterSet* sps,
                          const std::string& name,
                          const T& initval,
                          SPT paramType = SPT::kStartupAndRuntime)
        : BoundServerParameter<T>(sps,
                                  name,
                                  [this](const T& v) { return setLocked(v); },
                                  [this]() { return getLocked(); },
                                  paramType),
          _value(initval) {}
    ~LockedServerParameter() override = default;

    Status setLocked(const T& value) {
        stdx::unique_lock<stdx::mutex> lk(_mutex);
        _value = value;
        return Status::OK();
    }

    T getLocked() const {
        stdx::unique_lock<stdx::mutex> lk(_mutex);
        return _value;
    }

private:
    mutable stdx::mutex _mutex;
    T _value;
};

namespace server_parameter_detail {

template <typename T, typename...>
struct IsOneOf : std::false_type {};

template <typename T, typename U0, typename... Us>
struct IsOneOf<T, U0, Us...>
    : std::conditional_t<std::is_same<T, U0>::value, std::true_type, IsOneOf<T, Us...>> {};

/**
 * Type trait for ServerParameterType to identify which types are safe to use at runtime because
 * they have std::atomic or equivalent types.
 */
template <typename T>
struct IsSafeRuntimeType : IsOneOf<T, bool, int, long long, double> {};

/**
 * Get the type of storage to use for a given tuple of <type, ServerParameterType>.
 *
 * By default, we want std::atomic or equivalent types because they are thread-safe.
 * If the parameter is a startup only type, then there are no concurrency concerns since
 * server parameters are processed on the main thread while it is single-threaded during startup.
 */
template <typename T, ServerParameterType paramType>
struct StorageTraits {
    /**
     * For kStartupOnly parameters, we can use the type T as storage directly.
     * Otherwise if T is double, use AtomicDouble. Otherwise use AtomicWord<T>.
     */
    using value_type = std::conditional_t<
        paramType == ServerParameterType::kStartupOnly,
        T,
        std::conditional_t<std::is_same<T, double>::value, AtomicDouble, AtomicWord<T>>>;

    static T get(value_type* v) {
        return _get(v);
    }

    static void set(value_type* v, const T& newValue) {
        _set(v, newValue);
    }

private:
    static T _get(AtomicDouble* v) {
        return v->load();
    }
    template <typename U>
    static T _get(AtomicWord<U>* v) {
        return v->load();
    }
    template <typename U>
    static T _get(U* v) {
        return *v;
    }

    static void _set(AtomicDouble* v, const T& newValue) {
        v->store(newValue);
    }
    template <typename U>
    static void _set(AtomicWord<U>* v, const T& newValue) {
        v->store(newValue);
    }
    template <typename U>
    static void _set(U* v, const T& newValue) {
        *v = newValue;
    }
};

}  // namespace server_parameter_detail

/**
 * Implementation of BoundServerParameter for reading and writing a server parameter with a given
 * name and type into a specific C++ variable.
 *
 * NOTE: ServerParameters set at runtime can be read or written to at anytime, and are not
 * thread-safe without atomic types or other concurrency techniques.
 */
template <typename T, ServerParameterType paramType>
class ExportedServerParameter : public BoundServerParameter<T> {
public:
    MONGO_STATIC_ASSERT_MSG(paramType == ServerParameterType::kStartupOnly ||
                                server_parameter_detail::IsSafeRuntimeType<T>::value,
                            "This type is not supported as a runtime server parameter.");

    using storage_traits = server_parameter_detail::StorageTraits<T, paramType>;
    using storage_type = typename storage_traits::value_type;
    using validator_function = stdx::function<Status(const T&)>;

    /**
     * Construct an ExportedServerParameter in parameter set "sps", named "name", whose storage
     * is at "value".
     *
     * If allowedToChangeAtStartup is true, the parameter may be set at the command line,
     * e.g. via the --setParameter switch.  If allowedToChangeAtRuntime is true, the parameter
     * may be set at runtime, e.g.  via the setParameter command.
     */
    ExportedServerParameter(ServerParameterSet* sps, const std::string& name, storage_type* value)
        : BoundServerParameter<T>(sps,
                                  name,
                                  [this](const T& v) { return set(v); },
                                  [this] { return storage_traits::get(_value); },
                                  paramType),
          _value(value) {}

    // Don't let the template method hide our inherited method
    using BoundServerParameter<T>::set;

    virtual Status set(const T& newValue) {
        auto const status = validate(newValue);
        if (!status.isOK()) {
            return status;
        }
        storage_traits::set(_value, newValue);
        return Status::OK();
    }

    ExportedServerParameter* withValidator(validator_function validator) {
        invariant(!_validator);
        _validator = std::move(validator);
        return this;
    }

protected:
    /**
     * Note that if a subclass overrides the validate member function, the validator provided via
     * withValidate will not be used.
     **/
    virtual Status validate(const T& potentialNewValue) {
        if (_validator) {
            return _validator(potentialNewValue);
        }
        return Status::OK();
    }

    storage_type* const _value;  // owned elsewhere
    validator_function _validator;
};

}  // namespace mongo

#define MONGO_EXPORT_SERVER_PARAMETER_IMPL_(NAME, TYPE, INITIAL_VALUE, PARAM_TYPE) \
    ExportedServerParameter<TYPE, PARAM_TYPE>::storage_type NAME(INITIAL_VALUE);   \
    MONGO_COMPILER_VARIABLE_UNUSED auto _exportedParameter_##NAME =                \
        (new ExportedServerParameter<TYPE, PARAM_TYPE>(                            \
            ServerParameterSet::getGlobal(), #NAME, &NAME))

/**
 * Create a global variable of type "TYPE" named "NAME" with the given INITIAL_VALUE.  The
 * value may be set at startup or at runtime.
 */
#define MONGO_EXPORT_SERVER_PARAMETER(NAME, TYPE, INITIAL_VALUE) \
    MONGO_EXPORT_SERVER_PARAMETER_IMPL_(                         \
        NAME, TYPE, INITIAL_VALUE, ServerParameterType::kStartupAndRuntime)

/**
 * Like MONGO_EXPORT_SERVER_PARAMETER, but the value may only be set at startup.
 */
#define MONGO_EXPORT_STARTUP_SERVER_PARAMETER(NAME, TYPE, INITIAL_VALUE) \
    MONGO_EXPORT_SERVER_PARAMETER_IMPL_(                                 \
        NAME, TYPE, INITIAL_VALUE, ServerParameterType::kStartupOnly)

/**
 * Like MONGO_EXPORT_SERVER_PARAMETER, but the value may only be set at runtime.
 */
#define MONGO_EXPORT_RUNTIME_SERVER_PARAMETER(NAME, TYPE, INITIAL_VALUE) \
    MONGO_EXPORT_SERVER_PARAMETER_IMPL_(                                 \
        NAME, TYPE, INITIAL_VALUE, ServerParameterType::kRuntimeOnly)
