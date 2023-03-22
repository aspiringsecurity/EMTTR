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

import javax.lang.model.element.TypeElement;
import java.lang.annotation.Annotation;

public class AnnotatedTypeElement<T extends Annotation> {
    private TypeElement element;
    private T annotation;

    public AnnotatedTypeElement(TypeElement element, T annotation) {
        if (element == null) {
            throw new IllegalArgumentException("element cannot be null");
        }
        if (annotation == null) {
            throw new IllegalArgumentException("annotation cannot be null");
        }
        this.element = element;
        this.annotation = annotation;
    }

    public TypeElement getElement() {
        return element;
    }

    public T getAnnotation() {
        return annotation;
    }
}
