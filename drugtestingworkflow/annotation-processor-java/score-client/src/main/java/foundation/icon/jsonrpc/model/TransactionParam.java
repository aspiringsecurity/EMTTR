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

import foundation.icon.jsonrpc.Address;

import java.math.BigInteger;

public class TransactionParam extends AbstractTransaction {
    public static final BigInteger TIMESTAMP_MSEC_SCALE = BigInteger.valueOf(1000);
    public static BigInteger currentTimestamp() {
        return TIMESTAMP_MSEC_SCALE.multiply(BigInteger.valueOf(System.currentTimeMillis()));
    }

    public TransactionParam(BigInteger nid, Address to, BigInteger value, String dataType, Object data) {
        super();
        this.nid = nid;
        this.to = to;
        this.value = value;
        this.dataType = dataType;
        this.data = data;
    }

    public void setFrom(Address from) {
        this.from = from;
    }

    public void setTimestamp(BigInteger timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("TransactionParam{");
        sb.append("version=").append(version);
        sb.append(", from=").append(from);
        sb.append(", to=").append(to);
        sb.append(", value=").append(value);
        sb.append(", timestamp=").append(timestamp);
        sb.append(", nid=").append(nid);
        sb.append(", nonce=").append(nonce);
        sb.append(", dataType='").append(dataType).append('\'');
        sb.append(", data=").append(data);
        sb.append('}');
        return sb.toString();
    }
}
