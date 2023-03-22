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

import java.util.Map;
import java.util.Objects;

public class CallData {
    private String method;
    private Map<String, Object> params;

    public CallData(String method, Map<String, Object> params) {
        Objects.requireNonNull(method, "method required not null");
        if (method.isEmpty()) {
            throw new IllegalArgumentException("method required not empty");
        }
        this.method = method;
        this.params = params;
    }

    public String getMethod() {
        return method;
    }

    public Map<String, Object> getParams() {
        return params;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("CallData{");
        sb.append("method='").append(method).append('\'');
        sb.append(", params=").append(params);
        sb.append('}');
        return sb.toString();
    }
}
