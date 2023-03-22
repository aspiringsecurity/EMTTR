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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class AssertEquals {
    public static void assertEqualsParameterAcceptable(ParameterAcceptable o1, ParameterAcceptable o2) {
        assertEquals(o1.isBooleanVal(), o2.isBooleanVal());
        assertEquals(o1.getByteVal(), o2.getByteVal());
        assertEquals(o1.getCharVal(), o2.getCharVal());
        assertEquals(o1.getShortVal(), o2.getShortVal());
        assertEquals(o1.getIntVal(), o2.getIntVal());
        assertEquals(o1.getLongVal(), o2.getLongVal());
        assertEquals(o1.getBooleanWrap(), o2.getBooleanWrap());
        assertEquals(o1.getByteWrap(), o2.getByteWrap());
        assertEquals(o1.getCharWrap(), o2.getCharWrap());
        assertEquals(o1.getShortWrap(), o2.getShortWrap());
        assertEquals(o1.getIntegerWrap(), o2.getIntegerWrap());
        assertEquals(o1.getLongWrap(), o2.getLongWrap());
        assertEquals(o1.getStringVal(), o2.getStringVal());
        assertEquals(o1.getBigIntegerVal(), o2.getBigIntegerVal());
        assertEquals(o1.getAddressVal(), o2.getAddressVal());
        assertEquals(o1.getStruct(), o2.getStruct());
    }

    public static void assertEqualsDBAcceptable(DBAcceptable o1, DBAcceptable o2) {
        assertEqualsParameterAcceptable(o1, o2);
        //
        assertEquals(o1.getFloatVal(), o2.getFloatVal());
        assertEquals(o1.getDoubleVal(), o2.getDoubleVal());
        assertEquals(o1.getFloatWrap(), o2.getFloatWrap());
        assertEquals(o1.getDoubleWrap(), o2.getDoubleWrap());
        assertEquals(o1.getStringNotNull(), o2.getStringNotNull());
        assertEquals(o1.stringDirect, o2.stringDirect);
        assertEquals(o1.getBytesWrappedStruct(), o2.getBytesWrappedStruct());
        assertEquals(o1.getCustom(), o2.getCustom());
    }

    public static void assertEqualsBackwardCompatible(BackwardCompatible o1, BackwardCompatible o2) {
        assertEqualsParameterAcceptable(o1, o2);
        //
        assertEquals(o1.getAdded(), o2.getAdded());
    }

    public static void assertEqualsParameterAcceptableArray(ParameterAcceptableArray o1, ParameterAcceptableArray o2) {
        assertArrayEquals(o1.getBooleanVal(), o2.getBooleanVal());
        assertArrayEquals(o1.getByteVal(), o2.getByteVal());
        assertArrayEquals(o1.getCharVal(), o2.getCharVal());
        assertArrayEquals(o1.getShortVal(), o2.getShortVal());
        assertArrayEquals(o1.getIntVal(), o2.getIntVal());
        assertArrayEquals(o1.getLongVal(), o2.getLongVal());
        assertArrayEquals(o1.getBooleanWrap(), o2.getBooleanWrap());
        assertArrayEquals(o1.getByteWrap(), o2.getByteWrap());
        assertArrayEquals(o1.getCharWrap(), o2.getCharWrap());
        assertArrayEquals(o1.getShortWrap(), o2.getShortWrap());
        assertArrayEquals(o1.getIntegerWrap(), o2.getIntegerWrap());
        assertArrayEquals(o1.getLongWrap(), o2.getLongWrap());
        assertArrayEquals(o1.getStringVal(), o2.getStringVal());
        assertArrayEquals(o1.getBigIntegerVal(), o2.getBigIntegerVal());
        assertArrayEquals(o1.getAddressVal(), o2.getAddressVal());
        assertArrayEquals(o1.getStruct(), o2.getStruct());
        assertArrayEquals(o1.getByteMultiDepth(), o2.getByteMultiDepth());
    }

    public static void assertEqualsDBAcceptableArray(DBAcceptableArray o1, DBAcceptableArray o2) {
        assertEqualsParameterAcceptableArray(o1, o2);
        //
        assertArrayEquals(o1.getFloatVal(), o2.getFloatVal());
        assertArrayEquals(o1.getDoubleVal(), o2.getDoubleVal());
        assertArrayEquals(o1.getFloatWrap(), o2.getFloatWrap());
        assertArrayEquals(o1.getDoubleWrap(), o2.getDoubleWrap());
        assertArrayEquals(o1.getStringNotNull(), o2.getStringNotNull());
        assertArrayEquals(o1.stringDirect, o2.stringDirect);
        assertArrayEquals(o1.getBytesWrappedStruct(), o2.getBytesWrappedStruct());
        assertArrayEquals(o1.getCustom(), o2.getCustom());
    }
}
