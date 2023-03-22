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

import foundation.icon.jsonrpc.IconStringConverter;

import java.util.Map;
import java.util.Objects;

public class DeployData {
    private String contentType;
    private byte[] content;
    private Map<String, Object> params;

    public DeployData(String contentType, byte[] content, Map<String, Object> params) {
        Objects.requireNonNull(contentType, "contentType required not null");
        if (contentType.isEmpty()) {
            throw new IllegalArgumentException("contentType required not empty");
        }
        Objects.requireNonNull(content, "content required not null");
        if (content.length == 0) {
            throw new IllegalArgumentException("content required not empty");
        }
        this.contentType = contentType;
        this.content = content;
        this.params = params;
    }

    public String getContentType() {
        return contentType;
    }

    public byte[] getContent() {
        return content;
    }

    public Map<String, Object> getParams() {
        return params;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("DeployData{");
        sb.append("contentType='").append(contentType).append('\'');
        sb.append(", content=").append(IconStringConverter.fromBytes(content));
        sb.append(", params=").append(params);
        sb.append('}');
        return sb.toString();
    }
}
