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

package foundation.icon.score.example;

import foundation.icon.score.client.ScoreInterface;
import foundation.icon.score.example.model.BackwardCompatible;
import foundation.icon.score.example.model.ParameterAcceptable;
import score.annotation.External;

import java.util.Map;

@ScoreInterface
public interface HelloWorld extends NameGetter {
    @External(readonly = true)
    String greeting();

    @External
    void setGreeting(String _greeting);

    @External
    void setParameterAcceptable(ParameterAcceptable parameterAcceptable);

    @External(readonly = true)
    ParameterAcceptable getParameterAcceptable();

    //working with ScoreDataObject and JsonObject
    @External
    void setDBAcceptable(String _json);

    @External(readonly = true)
    String getDBAcceptable();

    @External
    void setDBAcceptableArray(String _json);

    @External(readonly = true)
    String getDBAcceptableArray();

    @External
    void setBackwardCompatible(BackwardCompatible backwardCompatible);

    @External(readonly = true)
    BackwardCompatible getBackwardCompatible();

    //working with EnumerableDictDB
    @External
    void putEnumerable(String _key, String _value);

    @External
    void removeEnumerable(String _key);

    @External(readonly = true)
    String getEnumerable(String _key);

    @External(readonly = true)
    Map<String, String> getEnumerableMap();

}
