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

import score.Address;

import java.math.BigInteger;

public class ParameterAcceptableArray {
    //primitive
    private boolean[] booleanVal;
    private byte[] byteVal;
    private char[] charVal;
    private short[] shortVal;
    private int[] intVal;
    private long[] longVal;

    //wrap of primitive
    private Boolean[] booleanWrap;
    private Byte[] byteWrap;
    private Character[] charWrap;
    private Short[] shortWrap;
    private Integer[] integerWrap;
    private Long[] longWrap;

    //array of nullable
    private String[] stringVal;
    private BigInteger[] bigIntegerVal;
    private Address[] addressVal;

    //struct
    private Struct[] struct;

    //n-depth array
    private byte[][] byteMultiDepth;

    public ParameterAcceptableArray() {
        super();
    }

    public ParameterAcceptableArray(ParameterAcceptableArray obj) {
        super();
        this.setBooleanVal(obj.getBooleanVal());
        this.setByteVal(obj.getByteVal());
        this.setCharVal(obj.getCharVal());
        this.setShortVal(obj.getShortVal());
        this.setIntVal(obj.getIntVal());
        this.setLongVal(obj.getLongVal());
        this.setBooleanWrap(obj.getBooleanWrap());
        this.setByteWrap(obj.getByteWrap());
        this.setCharWrap(obj.getCharWrap());
        this.setShortWrap(obj.getShortWrap());
        this.setIntegerWrap(obj.getIntegerWrap());
        this.setLongWrap(obj.getLongWrap());
        this.setStringVal(obj.getStringVal());
        this.setBigIntegerVal(obj.getBigIntegerVal());
        this.setAddressVal(obj.getAddressVal());
        this.setStruct(obj.getStruct());
        this.setByteMultiDepth(obj.getByteMultiDepth());
    }

    public boolean[] getBooleanVal() {
        return booleanVal;
    }

    public void setBooleanVal(boolean[] booleanVal) {
        this.booleanVal = booleanVal;
    }

    public byte[] getByteVal() {
        return byteVal;
    }

    public void setByteVal(byte[] byteVal) {
        this.byteVal = byteVal;
    }

    public char[] getCharVal() {
        return charVal;
    }

    public void setCharVal(char[] charVal) {
        this.charVal = charVal;
    }

    public short[] getShortVal() {
        return shortVal;
    }

    public void setShortVal(short[] shortVal) {
        this.shortVal = shortVal;
    }

    public int[] getIntVal() {
        return intVal;
    }

    public void setIntVal(int[] intVal) {
        this.intVal = intVal;
    }

    public long[] getLongVal() {
        return longVal;
    }

    public void setLongVal(long[] longVal) {
        this.longVal = longVal;
    }

    public Boolean[] getBooleanWrap() {
        return booleanWrap;
    }

    public void setBooleanWrap(Boolean[] booleanWrap) {
        this.booleanWrap = booleanWrap;
    }

    public Byte[] getByteWrap() {
        return byteWrap;
    }

    public void setByteWrap(Byte[] byteWrap) {
        this.byteWrap = byteWrap;
    }

    public Character[] getCharWrap() {
        return charWrap;
    }

    public void setCharWrap(Character[] charWrap) {
        this.charWrap = charWrap;
    }

    public Short[] getShortWrap() {
        return shortWrap;
    }

    public void setShortWrap(Short[] shortWrap) {
        this.shortWrap = shortWrap;
    }

    public Integer[] getIntegerWrap() {
        return integerWrap;
    }

    public void setIntegerWrap(Integer[] integerWrap) {
        this.integerWrap = integerWrap;
    }

    public Long[] getLongWrap() {
        return longWrap;
    }

    public void setLongWrap(Long[] longWrap) {
        this.longWrap = longWrap;
    }

    public String[] getStringVal() {
        return stringVal;
    }

    public void setStringVal(String[] stringVal) {
        this.stringVal = stringVal;
    }

    public BigInteger[] getBigIntegerVal() {
        return bigIntegerVal;
    }

    public void setBigIntegerVal(BigInteger[] bigIntegerVal) {
        this.bigIntegerVal = bigIntegerVal;
    }

    public Address[] getAddressVal() {
        return addressVal;
    }

    public void setAddressVal(Address[] addressVal) {
        this.addressVal = addressVal;
    }

    public Struct[] getStruct() {
        return struct;
    }

    public void setStruct(Struct[] struct) {
        this.struct = struct;
    }

    public byte[][] getByteMultiDepth() {
        return byteMultiDepth;
    }

    public void setByteMultiDepth(byte[][] byteMultiDepth) {
        this.byteMultiDepth = byteMultiDepth;
    }

}
