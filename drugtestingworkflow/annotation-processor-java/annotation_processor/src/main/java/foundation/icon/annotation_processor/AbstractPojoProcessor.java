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

package foundation.icon.annotation_processor;

/**
 * Generate extend class from POJO class
 * <code>
 * class GENERATED_TYPE extend POJO {
 *  public GENERATED_TYPE(POJO) {
 *      SETTER(POJO.GETTER())
 *  }
 *  public FIELD_TYPE GETTER() {
 *      return super.GETTER()
 *  }
 *  public SETTER(FIELD_TYPE) {
 *      super.SETTER(FIELD_TYPE)
 *  }
 *  public ENCODE_TYPE toENCODE_TYPE() {
 *      return ENCODE(GETTER())
 *  }
 *  public static GENERATED_TYPE fromENCODE_TYPE(ENCODE_TYPE) {
 *      GENERATED_TYPE GENERATED = GENERATED_TYPE()
 *      SETTER(DECODE(ENCODE_TYPE))
 *      return GENERATED
 *  }
 * }
 * </code>
 */
public abstract class AbstractPojoProcessor extends AbstractProcessor {
}
