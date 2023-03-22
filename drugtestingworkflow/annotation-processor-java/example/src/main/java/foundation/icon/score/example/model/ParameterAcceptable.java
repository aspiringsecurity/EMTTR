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

import score.*;

import java.math.BigInteger;

public class ParameterAcceptable {
    //primitive
    private boolean booleanVal;
    private byte byteVal;
    private char charVal;
    private short shortVal;
    private int intVal;
    private long longVal;

    //wrap of primitive
    private Boolean booleanWrap;
    private Byte byteWrap;
    private Character charWrap;
    private Short shortWrap;
    private Integer integerWrap;
    private Long longWrap;

    //nullable
    private String stringVal;
    private BigInteger bigIntegerVal;
    private Address addressVal;

    //struct
    private Struct struct;

    public ParameterAcceptable() {
        super();
    }

    public ParameterAcceptable(ParameterAcceptable obj) {
        super();
        this.setBooleanVal(obj.isBooleanVal());
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
    }

    public boolean isBooleanVal() {
        return booleanVal;
    }

    public void setBooleanVal(boolean booleanVal) {
        this.booleanVal = booleanVal;
    }

    public byte getByteVal() {
        return byteVal;
    }

    public void setByteVal(byte byteVal) {
        this.byteVal = byteVal;
    }

    public char getCharVal() {
        return charVal;
    }

    public void setCharVal(char charVal) {
        this.charVal = charVal;
    }

    public short getShortVal() {
        return shortVal;
    }

    public void setShortVal(short shortVal) {
        this.shortVal = shortVal;
    }

    public int getIntVal() {
        return intVal;
    }

    public void setIntVal(int intVal) {
        this.intVal = intVal;
    }

    public long getLongVal() {
        return longVal;
    }

    public void setLongVal(long longVal) {
        this.longVal = longVal;
    }

    public Boolean getBooleanWrap() {
        return booleanWrap;
    }

    public void setBooleanWrap(Boolean booleanWrap) {
        this.booleanWrap = booleanWrap;
    }

    public Byte getByteWrap() {
        return byteWrap;
    }

    public void setByteWrap(Byte byteWrap) {
        this.byteWrap = byteWrap;
    }

    public Character getCharWrap() {
        return charWrap;
    }

    public void setCharWrap(Character charWrap) {
        this.charWrap = charWrap;
    }

    public Short getShortWrap() {
        return shortWrap;
    }

    public void setShortWrap(Short shortWrap) {
        this.shortWrap = shortWrap;
    }

    public Integer getIntegerWrap() {
        return integerWrap;
    }

    public void setIntegerWrap(Integer integerWrap) {
        this.integerWrap = integerWrap;
    }

    public Long getLongWrap() {
        return longWrap;
    }

    public void setLongWrap(Long longWrap) {
        this.longWrap = longWrap;
    }

    public String getStringVal() {
        return stringVal;
    }

    public void setStringVal(String stringVal) {
        this.stringVal = stringVal;
    }

    public BigInteger getBigIntegerVal() {
        return bigIntegerVal;
    }

    public void setBigIntegerVal(BigInteger bigIntegerVal) {
        this.bigIntegerVal = bigIntegerVal;
    }

    public Address getAddressVal() {
        return addressVal;
    }

    public void setAddressVal(Address addressVal) {
        this.addressVal = addressVal;
    }

    public Struct getStruct() {
        return struct;
    }

    public void setStruct(Struct struct) {
        this.struct = struct;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("ParameterAcceptable{");
        sb.append("booleanVal=").append(booleanVal);
        sb.append(", byteVal=").append(byteVal);
        sb.append(", charVal=").append(charVal);
        sb.append(", shortVal=").append(shortVal);
        sb.append(", intVal=").append(intVal);
        sb.append(", longVal=").append(longVal);
        sb.append(", booleanWrap=").append(booleanWrap);
        sb.append(", byteWrap=").append(byteWrap);
        sb.append(", charWrap=").append(charWrap);
        sb.append(", shortWrap=").append(shortWrap);
        sb.append(", integerWrap=").append(integerWrap);
        sb.append(", longWrap=").append(longWrap);
        sb.append(", stringVal='").append(stringVal).append('\'');
        sb.append(", bigIntegerVal=").append(bigIntegerVal);
        sb.append(", addressVal=").append(addressVal);
        sb.append(", struct=").append(struct);
        sb.append('}');
        return sb.toString();
    }

    public static void writeObject(ObjectWriter writer, ParameterAcceptable obj) {
        obj.writeObject(writer);
    }

    public static ParameterAcceptable readObject(ObjectReader reader) {
        ParameterAcceptable obj = new ParameterAcceptable();
        reader.beginList();
        obj.setBooleanVal(reader.readBoolean());
        obj.setByteVal(reader.readByte());
        obj.setCharVal(reader.readChar());
        obj.setShortVal(reader.readShort());
        obj.setIntVal(reader.readInt());
        obj.setLongVal(reader.readLong());
        obj.setBooleanWrap(reader.readNullable(Boolean.class));
        obj.setByteWrap(reader.readNullable(Byte.class));
        obj.setCharWrap(reader.readNullable(Character.class));
        obj.setShortWrap(reader.readNullable(Short.class));
        obj.setIntegerWrap(reader.readNullable(Integer.class));
        obj.setLongWrap(reader.readNullable(Long.class));
        obj.setStringVal(reader.readNullable(String.class));
        obj.setBigIntegerVal(reader.readNullable(BigInteger.class));
        obj.setAddressVal(reader.readNullable(Address.class));
        obj.setStruct(reader.readNullable(Struct.class));
        reader.end();
        return obj;
    }

    public void writeObject(ObjectWriter writer) {
        writer.beginList(16);
        writer.write(this.isBooleanVal());
        writer.write(this.getByteVal());
        writer.write(this.getCharVal());
        writer.write(this.getShortVal());
        writer.write(this.getIntVal());
        writer.write(this.getLongVal());
        writer.writeNullable(this.getBooleanWrap());
        writer.writeNullable(this.getByteWrap());
        writer.writeNullable(this.getCharWrap());
        writer.writeNullable(this.getShortWrap());
        writer.writeNullable(this.getIntegerWrap());
        writer.writeNullable(this.getLongWrap());
        writer.writeNullable(this.getStringVal());
        writer.writeNullable(this.getBigIntegerVal());
        writer.writeNullable(this.getAddressVal());
        writer.writeNullable(this.getStruct());
        writer.end();
    }

    public static ParameterAcceptable fromBytes(byte[] bytes) {
        ObjectReader reader = Context.newByteArrayObjectReader("RLPn", bytes);
        return ParameterAcceptable.readObject(reader);
    }

    public byte[] toBytes() {
        ByteArrayObjectWriter writer = Context.newByteArrayObjectWriter("RLPn");
        ParameterAcceptable.writeObject(writer, this);
        return writer.toByteArray();
    }
}
