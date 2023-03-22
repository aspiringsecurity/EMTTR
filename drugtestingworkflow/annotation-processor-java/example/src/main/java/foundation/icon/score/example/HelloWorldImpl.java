/*
 * Copyright 2021 ICON Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package foundation.icon.score.example;

import foundation.icon.score.data.EnumerableDictDB;
import foundation.icon.score.example.model.*;
import score.Address;
import score.Context;
import score.DictDB;
import score.VarDB;
import score.annotation.EventLog;
import score.annotation.External;
import score.annotation.Payable;

import java.math.BigInteger;
import java.util.Map;

public class HelloWorldImpl implements HelloWorld, IcxTransfer {
    public static final String DEFAULT_GREETING = "Hello";
    private final String name;
    private final VarDB<String> greeting = Context.newVarDB("greeting", String.class);
    private final DictDB<Address, BigInteger> transferred =
            Context.newDictDB("transferred", BigInteger.class);

    //DBAcceptableSdo
    private final VarDB<DBAcceptableSdo> dbAcceptableVarDB =
            Context.newVarDB("dbAcceptableVarDB", DBAcceptableSdo.class);

    //DBAcceptableArraySdo
    private final VarDB<DBAcceptableArraySdo> dbAcceptableArrayVarDB =
            Context.newVarDB("dbAcceptableArrayVarDB", DBAcceptableArraySdo.class);

    //DBAcceptableCollectionSdo
    private final VarDB<DBAcceptableCollectionSdo> dbAcceptableCollectionVarDB =
            Context.newVarDB("dbAcceptableCollectionVarDB", DBAcceptableCollectionSdo.class);

    //ParameterAcceptable
    private final VarDB<ParameterAcceptable> parameterAcceptableVarDB =
            Context.newVarDB("parameterAcceptableVarDB", ParameterAcceptable.class);

    //BackwardCompatible
    private final VarDB<BackwardCompatibleSdo> backwardCompatibleVarDB =
            Context.newVarDB("parameterAcceptableVarDB", BackwardCompatibleSdo.class);

    //EnumerableDictDB
    private final EnumerableDictDB<String, String> enumerableDictDB =
            new EnumerableDictDB<>("enumerableDictDB", String.class, String.class);

    public HelloWorldImpl(String _name) {
        this.name = _name;
    }

    @External(readonly = true)
    public String name() {
        return name;
    }

    public static String greeting(String name, String greeting) {
        return "[" + name + "] " + greeting + "!";
    }

    @External(readonly = true)
    public String greeting() {
        return greeting(name, greeting.getOrDefault(DEFAULT_GREETING));
    }

    @External
    public void setGreeting(String _greeting) {
        this.greeting.set(_greeting);
    }

    @External
    public void setDBAcceptable(String _json) {
        dbAcceptableVarDB.set(new DBAcceptableSdo(DBAcceptableJson.parse(_json)));
    }

    @External(readonly = true)
    public String getDBAcceptable() {
        DBAcceptableSdo dbAcceptable = dbAcceptableVarDB.get();
        return DBAcceptableJson.toJson(dbAcceptable).toString();
    }

    @External
    public void setDBAcceptableArray(String _json) {
        dbAcceptableArrayVarDB.set(new DBAcceptableArraySdo(DBAcceptableArrayJson.parse(_json)));
    }

    @External(readonly = true)
    public String getDBAcceptableArray() {
        DBAcceptableArraySdo dbAcceptableArray = dbAcceptableArrayVarDB.get();
        return DBAcceptableArrayJson.toJson(dbAcceptableArray).toString();
    }

    @External
    public void setDBAcceptableCollection(String _json) {
        dbAcceptableCollectionVarDB.set(new DBAcceptableCollectionSdo(DBAcceptableCollectionJson.parse(_json)));
    }

    @External(readonly = true)
    public String getDBAcceptableCollection() {
        DBAcceptableCollectionSdo dbAcceptableCollection = dbAcceptableCollectionVarDB.get();
        return DBAcceptableCollectionJson.toJson(dbAcceptableCollection).toString();
    }

    @External
    public void setParameterAcceptable(ParameterAcceptable parameterAcceptable) {
        parameterAcceptableVarDB.set(parameterAcceptable);
    }

    @External(readonly = true)
    public ParameterAcceptable getParameterAcceptable() {
        return parameterAcceptableVarDB.get();
    }

    @External
    public void setBackwardCompatible(BackwardCompatible backwardCompatible) {
        backwardCompatibleVarDB.set(new BackwardCompatibleSdo(backwardCompatible));
    }

    @External(readonly = true)
    public BackwardCompatible getBackwardCompatible() {
        return backwardCompatibleVarDB.get();
    }

    @External
    public void putEnumerable(String _key, String _value) {
        enumerableDictDB.put(_key, _value);
    }

    @External
    public void removeEnumerable(String _key) {
        enumerableDictDB.remove(_key);
    }

    @External(readonly = true)
    public String getEnumerable(String _key) {
        return enumerableDictDB.get(_key);
    }

    @External(readonly = true)
    public Map<String, String> getEnumerableMap() {
        return enumerableDictDB.toMap();
    }

    @Payable
    public void fallback() {
        // just receive incoming funds
    }

    //Implements of IcxTransfer
    @Payable
    @External
    public void transfer(Address _address) {
        BigInteger value = Context.getValue();
        if (Context.getAddress().equals(_address)) {
            Address caller = Context.getCaller();
            BigInteger totalTransferred = transferred.getOrDefault(caller, BigInteger.ZERO);
            transferred.set(caller, totalTransferred.add(value));
            Transferred(caller, value);
        } else {
            IcxTransferScoreInterface icxTransfer = new IcxTransferScoreInterface(_address);
            icxTransfer.transfer(value, _address);
        }
    }

    @External(readonly = true)
    public BigInteger getTransferred(Address _address) {
        return transferred.get(_address);
    }

    @EventLog(indexed = 1)
    public void Transferred(Address _from, BigInteger icx) { }

}
