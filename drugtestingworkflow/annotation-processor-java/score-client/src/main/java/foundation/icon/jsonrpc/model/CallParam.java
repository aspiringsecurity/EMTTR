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

public class CallParam {
    private Address to;
    private String dataType = "call";
    private CallData data;

    public CallParam(Address to, CallData data) {
        this.to = to;
        this.data = data;
    }

    public Address getTo() {
        return to;
    }

    public String getDataType() {
        return dataType;
    }

    public CallData getData() {
        return data;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("CallParam{");
        sb.append("to=").append(to);
        sb.append(", dataType='").append(dataType).append('\'');
        sb.append(", data=").append(data);
        sb.append('}');
        return sb.toString();
    }
}
