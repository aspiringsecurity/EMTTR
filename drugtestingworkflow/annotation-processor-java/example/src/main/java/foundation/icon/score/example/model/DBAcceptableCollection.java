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

package foundation.icon.score.example.model;

import foundation.icon.score.data.ScoreDataObject;
import foundation.icon.score.data.ScoreDataProperty;
import foundation.icon.score.json.JsonObject;
import score.Address;

import java.math.BigInteger;
import java.util.List;

@JsonObject
@ScoreDataObject
public class DBAcceptableCollection {
    //wrap of primitive
    private List<Boolean> booleanWrap;
    private List<Byte> byteWrap;
    private List<Character> charWrap;
    private List<Short> shortWrap;
    private List<Integer> integerWrap;
    private List<Long> longWrap;

    //nullable
    private List<String> stringVal;
    private List<BigInteger> bigIntegerVal;
    private List<Address> addressVal;

    //struct
    private List<Struct> struct;

    //
    private List<Float> floatWrap;
    private List<Double> doubleWrap;

    @ScoreDataProperty(nullable = false)
    private List<String> stringNotNull;

    public List<Boolean> getBooleanWrap() {
        return booleanWrap;
    }

    public void setBooleanWrap(List<Boolean> booleanWrap) {
        this.booleanWrap = booleanWrap;
    }

    public List<Byte> getByteWrap() {
        return byteWrap;
    }

    public void setByteWrap(List<Byte> byteWrap) {
        this.byteWrap = byteWrap;
    }

    public List<Character> getCharWrap() {
        return charWrap;
    }

    public void setCharWrap(List<Character> charWrap) {
        this.charWrap = charWrap;
    }

    public List<Short> getShortWrap() {
        return shortWrap;
    }

    public void setShortWrap(List<Short> shortWrap) {
        this.shortWrap = shortWrap;
    }

    public List<Integer> getIntegerWrap() {
        return integerWrap;
    }

    public void setIntegerWrap(List<Integer> integerWrap) {
        this.integerWrap = integerWrap;
    }

    public List<Long> getLongWrap() {
        return longWrap;
    }

    public void setLongWrap(List<Long> longWrap) {
        this.longWrap = longWrap;
    }

    public List<String> getStringVal() {
        return stringVal;
    }

    public void setStringVal(List<String> stringVal) {
        this.stringVal = stringVal;
    }

    public List<BigInteger> getBigIntegerVal() {
        return bigIntegerVal;
    }

    public void setBigIntegerVal(List<BigInteger> bigIntegerVal) {
        this.bigIntegerVal = bigIntegerVal;
    }

    public List<Address> getAddressVal() {
        return addressVal;
    }

    public void setAddressVal(List<Address> addressVal) {
        this.addressVal = addressVal;
    }

    public List<Struct> getStruct() {
        return struct;
    }

    public void setStruct(List<Struct> struct) {
        this.struct = struct;
    }

    public List<Float> getFloatWrap() {
        return floatWrap;
    }

    public void setFloatWrap(List<Float> floatWrap) {
        this.floatWrap = floatWrap;
    }

    public List<Double> getDoubleWrap() {
        return doubleWrap;
    }

    public void setDoubleWrap(List<Double> doubleWrap) {
        this.doubleWrap = doubleWrap;
    }

    public List<String> getStringNotNull() {
        return stringNotNull;
    }

    public void setStringNotNull(List<String> stringNotNull) {
        this.stringNotNull = stringNotNull;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("DBAcceptableCollection{");
        sb.append("booleanWrap=").append(booleanWrap);
        sb.append(", byteWrap=").append(byteWrap);
        sb.append(", charWrap=").append(charWrap);
        sb.append(", shortWrap=").append(shortWrap);
        sb.append(", integerWrap=").append(integerWrap);
        sb.append(", longWrap=").append(longWrap);
        sb.append(", stringVal=").append(stringVal);
        sb.append(", bigIntegerVal=").append(bigIntegerVal);
        sb.append(", addressVal=").append(addressVal);
        sb.append(", struct=").append(struct);
        sb.append(", floatWrap=").append(floatWrap);
        sb.append(", doubleWrap=").append(doubleWrap);
        sb.append(", stringNotNull=").append(stringNotNull);
        sb.append('}');
        return sb.toString();
    }
}
