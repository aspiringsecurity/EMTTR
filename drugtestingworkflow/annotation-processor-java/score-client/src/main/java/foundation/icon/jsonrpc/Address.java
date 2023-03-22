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

package foundation.icon.jsonrpc;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import foundation.icon.icx.Wallet;

import java.util.Arrays;

public class Address extends score.Address {
    public enum Type {
        EOA("hx", 0x0),
        CONTRACT("cx", 0x1);
        String str;
        byte value;
        Type(String str, int value) {
            this.str = str;
            this.value = (byte)value;
        }
        String str() {
            return str;
        }
        byte value() {
            return value;
        }
        static Type of(byte value) {
            for(Type type : values()) {
                if (type.value == value) {
                    return type;
                }
            }
            throw new IllegalArgumentException();
        }
        static Type of(String str) {
            for(Type type : values()) {
                if (type.str.equals(str)) {
                    return type;
                }
            }
            throw new IllegalArgumentException();
        }
    }
    public static final int LENGTH = 21;
    public static final int BODY_LENGTH = LENGTH - 1;
    private final Type type;
    private final String str;
    private final byte[] bytes;

    @JsonCreator
    public Address(String str) {
        this(parse(str));
    }

    public Address(byte[] bytes) throws IllegalArgumentException {
        super(bytes);
        if (bytes == null) {
            throw new IllegalArgumentException("raw could not be null");
        }
        if (bytes.length != LENGTH) {
            throw new IllegalArgumentException("invalid length");
        }
        type = Type.of(bytes[0]);
        this.bytes = bytes;
        this.str = type.str() + IconStringConverter.fromBytes(bytes).substring(4);
    }

    public Address(Type type, byte[] body) throws IllegalArgumentException {
        this(concat(type.value(), body));
    }

    public boolean isContract() {
        return Type.CONTRACT.equals(type);
    }

    public static byte[] concat(byte type, byte[] body) {
        byte[] copy = new byte[LENGTH];
        copy[0] = type;
        System.arraycopy(body, 0, copy, 1, BODY_LENGTH);
        return copy;
    }

    public byte[] toByteArray() {
        return Arrays.copyOf(bytes, LENGTH);
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj ||
                (obj instanceof score.Address && toString().equals(obj.toString()));
    }

    @JsonValue
    @Override
    public String toString() {
        return str;
    }

    public static byte[] parse(String str) {
        if (str == null) {
            throw new IllegalArgumentException("string could not be null");
        }
        if (str.length() != LENGTH * 2) {
            throw new IllegalArgumentException("invalid length");
        }
        byte[] bytes = new byte[LENGTH];
        bytes[0] = Type.of(str.substring(0, 2)).value();
        System.arraycopy(IconStringConverter.toBytes(str.substring(2)), 0, bytes, 1, BODY_LENGTH);
        return bytes;
    }

    public static Address of(Wallet wallet) {
        return of(wallet.getAddress());
    }

    private static Address of(foundation.icon.icx.data.Address address) {
        return new Address(address.toString());
    }

}
