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

package foundation.icon.jsonrpc.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import foundation.icon.jsonrpc.IconStringConverter;


public class Hash {
    private final byte[] bytes;

    public Hash(byte[] bytes) {
        if (bytes == null) {
            throw new IllegalArgumentException("bytes could not be null");
        }
        this.bytes = bytes;
    }

    @JsonCreator
    public Hash(String string) {
        if (string == null) {
            throw new IllegalArgumentException("string could not be null");
        }
        this.bytes = IconStringConverter.toBytes(string);
    }

    public byte[] toBytes() {
        return bytes;
    }

    @JsonValue
    @Override
    public String toString() {
        return IconStringConverter.fromBytes(bytes);
    }
}
