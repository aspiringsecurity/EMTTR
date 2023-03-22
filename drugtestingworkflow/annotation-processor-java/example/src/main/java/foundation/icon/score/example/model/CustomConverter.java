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

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import score.ObjectReader;
import score.ObjectWriter;

public class CustomConverter {
    public static Custom parseCustom(JsonValue jsonValue) {
        if (jsonValue == null || jsonValue.isNull()) {
            return null;
        }
        JsonObject jsonObject = jsonValue.asObject();
        Custom obj = new Custom();
        JsonValue valueJsonValue = jsonObject.get("value");
        if (valueJsonValue != null && !valueJsonValue.isNull()) {
            obj.setValue(valueJsonValue.asString());
        }
        return obj;
    }

    public static JsonValue toJsonCustom(Custom obj) {
        if (obj == null) {
            return Json.NULL;
        }
        JsonObject jsonObject = Json.object();
        String value = obj.getValue();
        JsonValue valueJsonValue = value == null ? Json.NULL : Json.value(value);
        jsonObject.add("value", valueJsonValue);
        return jsonObject;
    }

    public static Custom readCustom(ObjectReader reader) {
        Custom obj = new Custom();
        obj.setValue(reader.readNullable(String.class));
        return obj;
    }

    public static void writeCustom(ObjectWriter writer, Custom obj) {
        writer.writeNullable(obj.getValue());
    }
}
