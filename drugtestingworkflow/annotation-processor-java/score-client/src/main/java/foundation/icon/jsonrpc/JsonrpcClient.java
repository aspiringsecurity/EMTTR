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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.deser.std.NumberDeserializers;
import okhttp3.*;
import okio.BufferedSink;

import java.io.IOException;

public class JsonrpcClient {
    public static MediaType APPLICATION_JSON = MediaType.parse("application/json");

    protected final String endpoint;
    protected final OkHttpClient httpClient;
    protected final ObjectMapper mapper;
    protected Headers customHeaders;
    protected boolean dumpJson;

    public JsonrpcClient(String endpoint) {
        this(endpoint, new OkHttpClient.Builder().build());
    }

    public JsonrpcClient(String endpoint, OkHttpClient httpClient) {
        this(endpoint, httpClient, new ObjectMapper());
    }

    public JsonrpcClient(String endpoint, ObjectMapper mapper) {
        this(endpoint, new OkHttpClient.Builder().build(), mapper);
    }

    public JsonrpcClient(String endpoint, OkHttpClient httpClient, ObjectMapper mapper) {
        this.endpoint = endpoint;
        this.httpClient = httpClient;
        this.mapper = mapper;
    }

    public String endpoint() {
        return endpoint;
    }

    public OkHttpClient httpClient() {
        return httpClient;
    }

    public ObjectMapper mapper() {
        return mapper;
    }

    public boolean isDumpJson() {
        return dumpJson;
    }

    public void setDumpJson(boolean dumpJson) {
        this.dumpJson = dumpJson;
    }

    public Headers getCustomHeaders() {
        return customHeaders;
    }

    public void setCustomHeaders(Headers customHeaders) {
        this.customHeaders = customHeaders;
    }

    private Headers.Builder customHeadersBuilder() {
        return customHeaders == null ? new Headers.Builder() : customHeaders.newBuilder();
    }

    public void addCustomHeader(String name) {
        customHeaders = customHeadersBuilder().add(name).build();
    }

    public void addCustomHeader(String name, String value) {
        customHeaders = customHeadersBuilder().add(name, value).build();
    }

    public void setCustomHeader(String name, String value) {
        customHeaders = customHeadersBuilder().set(name, value).build();
    }

    public Object request(String method, Object param) {
        return request(Object.class, method, param);
    }

    public <T> T request(Class<T> resultType, String method, Object params) {
        return request(mapper.getTypeFactory().constructType(resultType), method, params);
    }

    public <T> T request(TypeReference<T> resultType, String method, Object params) {
        return request(mapper.getTypeFactory().constructType(resultType.getType()), method, params);
    }

    public <T> T request(JavaType resultType, String method, Object params) {
        Request.Builder builder = new Request.Builder()
                .url(endpoint)
                .post(new JsonrpcRequestBody(method, params, mapper, dumpJson));
        if (customHeaders != null) {
            builder.headers(customHeaders);
        }
        return request(builder.build(), resultType);
    }

    protected <T> T request(Request request, JavaType resultType) {
        Response response = null;
        try {
            response = httpClient.newCall(request).execute();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        ResponseBody responseBody = response.body();
        if (responseBody != null) {
            JsonrpcResponse<T> jsonrpcResponse = null;
            try {
                String json = responseBody.string();
                if (dumpJson) {
                    System.out.println(json);
                }
                jsonrpcResponse = mapper.readValue(json,
                        mapper.getTypeFactory().constructParametricType(JsonrpcResponse.class, resultType));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            JsonrpcError error = jsonrpcResponse.getError();
            if (error != null) {
                throw error;
            }
            return jsonrpcResponse.result;
        } else {
            throw new RuntimeException("empty body");
        }
    }

    public static class JsonrpcRequestBody extends RequestBody {
        ObjectMapper mapper;
        boolean dumpJson;
        JsonrpcRequest request;

        JsonrpcRequestBody(String method, Object params, ObjectMapper mapper, boolean dumpJson) {
            this.request = new JsonrpcRequest(method, params);
            this.mapper = mapper;
            this.dumpJson = dumpJson;
        }

        @Override
        public MediaType contentType() {
            return APPLICATION_JSON;
        }

        @Override
        public void writeTo(BufferedSink bufferedSink) throws IOException {
            if (dumpJson) {
                byte[] bytes = mapper.writeValueAsBytes(this.request);
                bufferedSink.write(bytes);
                System.out.println(new String(bytes));
            } else {
                mapper.writeValue(bufferedSink.outputStream(), this.request);
            }
        }
    }

    public static class JsonrpcRequest {
        String jsonrpc = "2.0";
        @JsonSerialize(using = LongLikeSerializer.class)
        @JsonDeserialize(using = NumberDeserializers.NumberDeserializer.class)
        long id;
        String method;
        @JsonInclude(JsonInclude.Include.NON_NULL)
        Object params;

        JsonrpcRequest(String method, Object params) {
            this.id = System.currentTimeMillis();
            this.method = method;
            this.params = params;
        }
        public String getJsonrpc() {
            return jsonrpc;
        }

        public long getId() {
            return id;
        }

        public String getMethod() {
            return method;
        }

        public Object getParams() {
            return params;
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class JsonrpcResponse<T> {
        String jsonrpc = "2.0";
        @JsonSerialize(using = LongLikeSerializer.class)
        @JsonDeserialize(using = NumberDeserializers.NumberDeserializer.class)
        long id;
        T result;
        JsonrpcError error;

        public String getJsonrpc() {
            return jsonrpc;
        }

        public long getId() {
            return id;
        }

        public T getResult() {
            return result;
        }

        public JsonrpcError getError() {
            return error;
        }
    }

    public static class JsonrpcError extends RuntimeException {
        @JsonSerialize(using = LongLikeSerializer.class)
        @JsonDeserialize(using = NumberDeserializers.NumberDeserializer.class)
        private long code;
        private String message;
        private byte[] data;

        public long getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        public byte[] getData() {
            return data;
        }
    }

    public static class LongLikeSerializer extends JsonSerializer<Long> {
        @Override
        public void serialize(Long aLong, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeNumber(((Number) aLong).intValue());
        }
    }
}
