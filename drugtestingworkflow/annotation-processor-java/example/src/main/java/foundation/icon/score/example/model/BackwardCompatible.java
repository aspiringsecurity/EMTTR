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

import foundation.icon.score.data.ScoreDataObject;

@ScoreDataObject(beginOfOptionalFields = "added")
public class BackwardCompatible extends ParameterAcceptable {
    private String added;

    public BackwardCompatible() {
    }

    public BackwardCompatible(ParameterAcceptable obj) {
        super(obj);
    }

    public String getAdded() {
        return added;
    }

    public void setAdded(String added) {
        this.added = added;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BackwardCompatible{");
        sb.append("added='").append(added).append('\'');
        sb.append('}');
        return sb.toString();
    }
}
