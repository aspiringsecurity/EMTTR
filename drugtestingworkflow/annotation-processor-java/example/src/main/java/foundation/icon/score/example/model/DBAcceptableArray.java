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
import score.ArrayDB;

@JsonObject
@ScoreDataObject
public class DBAcceptableArray extends ParameterAcceptableArray {
    //not allowed types for parameter or return type of external method
    private float[] floatVal;
    private double[] doubleVal;
    private Float[] floatWrap;
    private Double[] doubleWrap;

    //notNull
    @ScoreDataProperty(nullable = false)
    private String[] stringNotNull;

    //direct
    //this field will be ignored when using at parameter or return type, because it doesn't have getter and setter
    @ScoreDataProperty(direct = true)
    @JsonProperty(direct = true)
    protected String[] stringDirect;

    //wrapped
    @ScoreDataProperty(wrapped = true)
    private Struct[] bytesWrappedStruct;

    @ScoreDataProperty(
            readObject = "CustomConverter.readCustom",
            writeObject = "CustomConverter.writeCustom"
    )
    @JsonProperty(
            parser = "CustomConverter.parseCustom",
            toJson = "CustomConverter.toJsonCustom"
    )
    private Custom[] custom;

    //ignore
    @ScoreDataProperty(ignore = true)
    private ArrayDB<String> arrayDB;

    public float[] getFloatVal() {
        return floatVal;
    }

    public void setFloatVal(float[] floatVal) {
        this.floatVal = floatVal;
    }

    public double[] getDoubleVal() {
        return doubleVal;
    }

    public void setDoubleVal(double[] doubleVal) {
        this.doubleVal = doubleVal;
    }

    public Float[] getFloatWrap() {
        return floatWrap;
    }

    public void setFloatWrap(Float[] floatWrap) {
        this.floatWrap = floatWrap;
    }

    public Double[] getDoubleWrap() {
        return doubleWrap;
    }

    public void setDoubleWrap(Double[] doubleWrap) {
        this.doubleWrap = doubleWrap;
    }

    public String[] getStringNotNull() {
        return stringNotNull;
    }

    public void setStringNotNull(String[] stringNotNull) {
        this.stringNotNull = stringNotNull;
    }

    public String[] getStringDirect() {
        return stringDirect;
    }

    public void setStringDirect(String[] stringDirect) {
        this.stringDirect = stringDirect;
    }

    public Struct[] getBytesWrappedStruct() {
        return bytesWrappedStruct;
    }

    public void setBytesWrappedStruct(Struct[] bytesWrappedStruct) {
        this.bytesWrappedStruct = bytesWrappedStruct;
    }

    public Custom[] getCustom() {
        return custom;
    }

    public void setCustom(Custom[] custom) {
        this.custom = custom;
    }

    public ArrayDB<String> getArrayDB() {
        return arrayDB;
    }

    public void setArrayDB(ArrayDB<String> arrayDB) {
        this.arrayDB = arrayDB;
    }

}
