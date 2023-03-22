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
import java.util.Objects;

public class SendTransactionParam extends TransactionParam {
    private BigInteger stepLimit;

    public SendTransactionParam(BigInteger nid, Address to, BigInteger value, String dataType, Object data) {
        super(nid, to, value, dataType, data);
        Objects.requireNonNull(nid, "nid required not null");
        Objects.requireNonNull(to, "to Address required not null");
        if (value != null && value.signum() == -1) {
            throw new IllegalArgumentException("nid must be positive");
        }
    }

    public BigInteger getStepLimit() {
        return stepLimit;
    }

    public void setStepLimit(BigInteger stepLimit) {
        this.stepLimit = stepLimit;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("SendTransactionParam{");
        sb.append("stepLimit=").append(stepLimit);
        sb.append(", version=").append(version);
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
