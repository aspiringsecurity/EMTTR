/*
 * Copyright 2023 ICON Foundation
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

package foundation.icon.jsonrpc;

import java.math.BigInteger;

public class IconStringConverter {
    public static final char[] HEX_CODES = "0123456789abcdef".toCharArray();
    public static final String BOOLEAN_TRUE = "0x1";
    public static final String BOOLEAN_FALSE = "0x0";
    public static final String HEX_PREFIX = "0x";
    public static final String NEG_HEX_PREFIX = "-0x";

    public static String fromBytes(byte[] bytes) {
        if (bytes == null) {
            return HEX_PREFIX;
        }
        StringBuilder r = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            r.append(HEX_CODES[(b >> 4) & 0xF]);
            r.append(HEX_CODES[(b & 0xF)]);
        }
        return HEX_PREFIX + r;
    }

    public static byte[] toBytes(String s) {
        if (s == null) {
            return null;
        }
        if (s.length() % 2 > 0) {
            throw new IllegalArgumentException("hex string length must be even");
        }
        if (s.startsWith(HEX_PREFIX)) {
            s = s.substring(2);
        }
        int l = s.length()/2;
        int j = 0;
        byte[] bytes = new byte[l];
        for (int i = 0; i < l; i++) {
            bytes[i] = (byte)((Character.digit(s.charAt(j++), 16) << 4) |
                    Character.digit(s.charAt(j++), 16) & 0xFF);
        }
        return bytes;
    }

    public static String fromBigInteger(BigInteger bi) {
        String prefix = (bi.signum() == -1) ? NEG_HEX_PREFIX : HEX_PREFIX;
        return prefix + bi.abs().toString(16);
    }

    public static BigInteger toBigInteger(String s) {
        if (s.startsWith(HEX_PREFIX)) {
            return new BigInteger(s.substring(2), 16);
        } else if (s.startsWith(NEG_HEX_PREFIX)) {
            return new BigInteger(s.substring(3), 16).negate();
        } else {
            return new BigInteger(s, 16);
        }
    }

    public static String fromBoolean(Boolean b) {
        return b ? BOOLEAN_TRUE : BOOLEAN_FALSE;
    }

    public static Boolean toBoolean(String s) {
        if (BOOLEAN_TRUE.equals(s)) {
            return Boolean.TRUE;
        } else if (BOOLEAN_FALSE.equals(s)) {
            return Boolean.FALSE;
        }
        throw new IllegalArgumentException("invalid value:"+s);
    }

    public static String fromAddress(Address a) {
        return a.toString();
    }

    public static Address toAddress(String s) {
        return new Address(s);
    }
}
