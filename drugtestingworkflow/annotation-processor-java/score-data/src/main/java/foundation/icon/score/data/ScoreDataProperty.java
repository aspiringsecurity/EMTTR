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

package foundation.icon.score.data;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.SOURCE)
public @interface ScoreDataProperty {
    /**
     * If type of field is primitive, nullable will be fixed 'false'
     * And for List type, nullable will be fixed 'true'
     *
     * @return boolean nullable default true
     */
    boolean nullable() default true;
    String getter() default "";
    String setter() default "";
    /**
     * If use ScoreDataObject as return type of external method,
     * JAVAEE reflect only typical getter which has 'get' prefix for representation.
     *
     * @return boolean direct default false
     */
    boolean direct() default false;
    boolean ignore() default false;
    String writeObject() default "";
    String readObject() default "";

    /**
     * In case of byte[] wrapped
     * @return boolean wrapped default false
     */
    boolean wrapped() default false;

    /**
     * If type of field is not array or List, nullableComponent will be ignored
     * If type of component is primitive type, nullableComponent will be fixed 'false'
     *
     * @return boolean nullableComponent default true
     */
    boolean nullableComponent() default true;
}
