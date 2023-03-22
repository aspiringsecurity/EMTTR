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

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.lang.model.type.*;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Pattern;

public abstract class AbstractProcessor extends javax.annotation.processing.AbstractProcessor {
    protected Messager messager;
    protected Elements elementUtil;
    protected Types typeUtil;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        messager = new Messager(processingEnv.getMessager(), this.getClass().getSimpleName());
        elementUtil = processingEnv.getElementUtils();
        typeUtil = processingEnv.getTypeUtils();
    }

    public TypeMirror getPrimitiveType(Class<?> clazz) {
        return typeUtil.getPrimitiveType(TypeKind.valueOf(clazz.getTypeName().toUpperCase()));
    }

    public TypeMirror getBoxedType(TypeMirror type) {
        return typeUtil.boxedClass((PrimitiveType) type).asType();
    }

    public TypeMirror getTypeMirror(Class<?> clazz) {
        if (clazz.isArray()) {
            return typeUtil.getArrayType(getTypeMirror(clazz.getComponentType()));
        } else if (clazz.isPrimitive()) {
            return getPrimitiveType(clazz);
        } else {
            return elementUtil.getTypeElement(clazz.getName()).asType();
        }
    }

    public TypeMirror getTypeMirrorFromAnnotation(Supplier<Class<?>> supplier) {
        try {
            return getTypeMirror(supplier.get());
        } catch (MirroredTypeException e) {
            return e.getTypeMirror();
        }
    }

    public TypeElement getTypeElement(TypeMirror type) {
        return (TypeElement)typeUtil.asElement(type);
    }

    public String findMethod(TypeMirror type, String methodNamePattern, TypeMirror returnType, Modifier[] modifiers, TypeMirror ... parameters) {
        TypeMirror objectType = getTypeMirror(Object.class);
        Pattern pattern = Pattern.compile(methodNamePattern);
        TypeElement element = (TypeElement)typeUtil.asElement(type);
        for (Element enclosedElement : element.getEnclosedElements()) {
            if (enclosedElement.getKind().equals(ElementKind.METHOD) &&
                    ProcessorUtil.hasModifier(enclosedElement, modifiers)) {
                ExecutableElement method = (ExecutableElement)enclosedElement;
                List<? extends VariableElement> methodParameters = method.getParameters();
                String methodName = method.getSimpleName().toString();
                if (pattern.matcher(methodName).matches() &&
                        (returnType == null || typeUtil.isSameType(returnType, method.getReturnType())) &&
                        methodParameters.size() == parameters.length) {
                    boolean isEqual = true;
                    for(int i = 0; i< parameters.length; i++) {
                        TypeMirror methodParameter = methodParameters.get(i).asType();
                        TypeMirror parameter = parameters[i];
                        if (typeUtil.isSameType(objectType, parameter)) {
                            continue;
                        }
                        if (!typeUtil.isSameType(methodParameter, parameter)) {
                            isEqual = false;
                            break;
                        }
                    }
                    if (isEqual) {
                        messager.noteMessage("found pattern:%s '%s %s %s(%s)' in %s",
                                methodNamePattern,
                                Arrays.toString(modifiers),
                                returnType == null ? "void" : returnType,
                                methodName,
                                Arrays.toString(parameters),
                                type);
                        return methodName;
                    }
                }
            }
        }
        return null;
    }

    public String findMethod(TypeMirror type, String methodNamePattern, Class<?> returnType, Modifier[] modifiers, Class<?> ... parameters) {
        TypeMirror[] parameterTypes = new TypeMirror[parameters.length];
        for (int i = 0; i < parameters.length; i++) {
            parameterTypes[i] = getTypeMirror(parameters[i]);
        }
        return findMethod(type, methodNamePattern, getTypeMirror(returnType), modifiers, parameterTypes);
    }

    public <T extends Annotation> AnnotatedTypeElement<T> getAnnotatedTypeElement(TypeMirror type, Class<T> annotationType) {
        if (!type.getKind().isPrimitive()) {
            TypeElement element = getTypeElement(type);
//                element = elementsuper.getTypeElement(type.toString());
            if (element != null) {
                T ann = element.getAnnotation(annotationType);
                if (ann != null){
                    return new AnnotatedTypeElement<>(element, ann);
                }
            }
        }
        return null;
    }

    public boolean containsDeclaredType(Collection<TypeMirror> list, TypeMirror type) {
        if (type.getKind() == TypeKind.DECLARED) {
            TypeElement element = (TypeElement) ((DeclaredType) type).asElement();
            TypeMirror varType = element.asType();
            for (TypeMirror e : list) {
                if (typeUtil.isSameType(varType, e)) {
                    return true;
                }
            }
        }
        return false;
    }

    public <V> V getDeclaredType(Map<TypeMirror, V> map, TypeMirror type) {
        if (type.getKind() == TypeKind.DECLARED) {
            TypeElement element = (TypeElement) ((DeclaredType) type).asElement();
            TypeMirror varType = element.asType();
            for (Map.Entry<TypeMirror,V> entry : map.entrySet()) {
                if (typeUtil.isSameType(varType, entry.getKey())) {
                    return entry.getValue();
                }
            }
        }
        return null;
    }

}
