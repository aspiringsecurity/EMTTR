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

import com.squareup.javapoet.AnnotationSpec;
import com.squareup.javapoet.MethodSpec;
import com.squareup.javapoet.ParameterSpec;
import com.squareup.javapoet.TypeName;

import javax.lang.model.element.*;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import java.lang.annotation.Annotation;
import java.util.*;

public class ProcessorUtil {

    public static TypeMirror getComponentType(TypeMirror fieldType) {
        if (fieldType.getKind() == TypeKind.ARRAY) {
            TypeMirror componentType = ((ArrayType) fieldType).getComponentType();
            if (componentType.getKind() == TypeKind.ARRAY) {
                return getComponentType(componentType);
            }
            return componentType;
        } else if (fieldType.getKind() == TypeKind.DECLARED) {
            return ((DeclaredType) fieldType).getTypeArguments().get(0);
        }
        return null;
    }

    public static int getComponentTypeDepth(TypeMirror fieldType) {
        if (fieldType.getKind() == TypeKind.ARRAY) {
            TypeMirror componentType = ((ArrayType) fieldType).getComponentType();
            if (componentType.getKind() == TypeKind.ARRAY) {
                return getComponentTypeDepth(componentType) + 1;
            }
            return 1;
        } else if (fieldType.getKind() == TypeKind.DECLARED) {
            return 1;
        }
        return 0;
    }

    public static boolean hasModifier(Element element, Modifier... modifiers) {
        for (Modifier modifier : modifiers) {
            if (!element.getModifiers().contains(modifier)) {
                return false;
            }
        }
        return true;
    }

    public static <A extends Annotation> boolean hasMethodAnnotation(TypeElement element, Class<A> annotationType) {
        for (Element enclosedElement : element.getEnclosedElements()) {
            if (enclosedElement.getKind().equals(ElementKind.METHOD)) {
                if (!hasModifier(enclosedElement, Modifier.STATIC)) {
                    A annotation = element.getAnnotation(annotationType);
                    if (annotation != null) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    public static boolean hasInterface(TypeElement element, Class<?> clazz) {
        if (!clazz.isInterface()) {
            throw new RuntimeException(String.format("%s is not interface class", clazz.getName()));
        }
        List<? extends TypeMirror> interfaces = element.getInterfaces();
        for (TypeMirror inf : interfaces) {
            if (clazz.getName().equals(inf.toString())) {
                return true;
            }
        }
        return false;
    }

    public static boolean hasInterface(TypeElement element, TypeMirror infType) {
        List<? extends TypeMirror> interfaces = element.getInterfaces();
        for (TypeMirror inf : interfaces) {
            if (inf.toString().equals(infType.toString())) {
                return true;
            }
        }
        return false;
    }

    public static MethodSpec getConflictMethod(Iterable<MethodSpec> methodSpecs, MethodSpec target) {
        for (MethodSpec methodSpec : methodSpecs) {
            if (methodSpec.name.equals(target.name) &&
                    compareParameterSpecs(methodSpec.parameters, target.parameters)) {
                return methodSpec;
            }
        }
        return null;
    }

    public static boolean compareParameterSpecs(List<ParameterSpec> o1, List<ParameterSpec> o2) {
        if (o1.size() == o2.size()) {
            for (int i = 0; i < o1.size(); i++) {
                ParameterSpec p1 = o1.get(i);
                ParameterSpec p2 = o2.get(i);
                if (!p1.type.toString().equals(p2.type.toString())) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public static String parameterSpecToString(List<ParameterSpec> parameterSpecs) {
        StringJoiner joiner = new StringJoiner(", ");
        for(ParameterSpec parameterSpec : parameterSpecs) {
            joiner.add(parameterSpec.type.toString());
        }
        return joiner.toString();
    }

    public static String getDefaultValueAsString(TypeMirror type) {
        switch (type.getKind()) {
            case BOOLEAN:
                return "false";
            case FLOAT:
                return "0.0f";
            case DOUBLE:
                return "0.0d";
            case LONG:
                return "0L";
            case INT:
                return "0";
            case SHORT:
                return "(short)0";
            case BYTE:
                return "(byte)0";
            case CHAR:
                return "'\u0000'";
            default:
                return "null";
        }
    }

    public static Modifier[] getModifiers(Element element, Modifier ... excludes) {
        Set<Modifier> modifierSet = element.getModifiers();
        if (modifierSet == null) {
            return new Modifier[]{};
        } else {
            if (excludes != null && excludes.length > 0) {
                List<Modifier> modifiers = new ArrayList<>();
                List<Modifier> excludeList = Arrays.asList(excludes);
                for(Modifier modifier : modifierSet) {
                    if (!excludeList.contains(modifier)) {
                        modifiers.add(modifier);
                    }
                }
                return modifiers.toArray(new Modifier[0]);
            } else {
                return modifierSet.toArray(new Modifier[0]);
            }
        }
    }

    public static List<AnnotationSpec> getAnnotationSpecs(Element element) {
        List<? extends AnnotationMirror> annotationMirrors = element.getAnnotationMirrors();
        List<AnnotationSpec> annotationSpecs = new ArrayList<>();
        if (annotationMirrors != null) {
            for(AnnotationMirror annotationMirror : annotationMirrors) {
                annotationSpecs.add(AnnotationSpec.get(annotationMirror));
            }
        }
        return annotationSpecs;
    }

    public static List<ParameterSpec> getParameterSpecs(ExecutableElement element) {
        List<? extends VariableElement> parameters = element.getParameters();
        List<ParameterSpec> parameterSpecs = new ArrayList<>();
        if (parameters != null) {
            for(VariableElement ve : parameters) {
                parameterSpecs.add(ParameterSpec.get(ve));
            }
        }
        return parameterSpecs;
    }

    public static List<TypeName> getSuperinterfaces(TypeElement element) {
        List<? extends TypeMirror> interfaces = element.getInterfaces();
        List<TypeName> typeNames = new ArrayList<>();
        if (interfaces != null) {
            for(TypeMirror tm : interfaces) {
                typeNames.add(TypeName.get(tm));
            }
        }
        return typeNames;
    }

}
