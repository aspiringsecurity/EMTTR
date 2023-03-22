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
import foundation.icon.score.json.JsonProperty;
import score.VarDB;

//if number of fields is over 31 then may throws RejectedClassException.tooManyInstanceVariables
//refer to org.aion.avm.core.rejection.ConsensusLimitConstants#MAX_TOTAL_INSTANCE_VARIABLES = 31
@JsonObject
@ScoreDataObject
public class DBAcceptable extends ParameterAcceptable {
    //not allowed types for parameter or return type of external method
    private float floatVal;
    private double doubleVal;
    private Float floatWrap;
    private Double doubleWrap;

    //notNull
    @ScoreDataProperty(nullable = false)
    private String stringNotNull;

    //direct
    //this field will be ignored when using at parameter or return type, because it doesn't have getter and setter
    @ScoreDataProperty(direct = true)
    @JsonProperty(direct = true)
    protected String stringDirect;

    //wrapped
    @ScoreDataProperty(wrapped = true)
    private Struct bytesWrappedStruct;

    //custom
    @ScoreDataProperty(
            readObject = "CustomConverter.readCustom",
            writeObject = "CustomConverter.writeCustom"
    )
    @JsonProperty(
            parser = "CustomConverter.parseCustom",
            toJson = "CustomConverter.toJsonCustom"
    )
    private Custom custom;

    //ignore
    @ScoreDataProperty(ignore = true)
    private VarDB<String> varDB;

    public float getFloatVal() {
        return floatVal;
    }

    public void setFloatVal(float floatVal) {
        this.floatVal = floatVal;
    }

    public double getDoubleVal() {
        return doubleVal;
    }

    public void setDoubleVal(double doubleVal) {
        this.doubleVal = doubleVal;
    }

    public Float getFloatWrap() {
        return floatWrap;
    }

    public void setFloatWrap(Float floatWrap) {
        this.floatWrap = floatWrap;
    }

    public Double getDoubleWrap() {
        return doubleWrap;
    }

    public void setDoubleWrap(Double doubleWrap) {
        this.doubleWrap = doubleWrap;
    }

    public String getStringNotNull() {
        return stringNotNull;
    }

    public void setStringNotNull(String stringNotNull) {
        this.stringNotNull = stringNotNull;
    }

    public Struct getBytesWrappedStruct() {
        return bytesWrappedStruct;
    }

    public void setBytesWrappedStruct(Struct bytesWrappedStruct) {
        this.bytesWrappedStruct = bytesWrappedStruct;
    }

    public Custom getCustom() {
        return custom;
    }

    public void setCustom(Custom custom) {
        this.custom = custom;
    }

    public VarDB<String> getVarDB() {
        return varDB;
    }

    public void setVarDB(VarDB<String> varDB) {
        this.varDB = varDB;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("DBAcceptable{");
        sb.append("floatVal=").append(floatVal);
        sb.append(", doubleVal=").append(doubleVal);
        sb.append(", floatWrap=").append(floatWrap);
        sb.append(", doubleWrap=").append(doubleWrap);
        sb.append(", stringNotNull='").append(stringNotNull).append('\'');
        sb.append(", stringDirect='").append(stringDirect).append('\'');
        sb.append(", bytesWrappedStruct=").append(bytesWrappedStruct);
        sb.append(", custom=").append(custom);
        sb.append(", varDB=").append(varDB);
        sb.append('}').append(super.toString());
        return sb.toString();
    }
}
