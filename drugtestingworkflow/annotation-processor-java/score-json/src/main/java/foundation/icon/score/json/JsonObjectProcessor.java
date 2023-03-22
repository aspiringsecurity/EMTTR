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

package foundation.icon.score.json;

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonValue;
import com.squareup.javapoet.*;
import foundation.icon.annotation_processor.AbstractProcessor;
import foundation.icon.annotation_processor.AnnotatedTypeElement;
import foundation.icon.annotation_processor.ProcessorUtil;
import score.Address;
import score.ArrayDB;
import score.DictDB;
import score.VarDB;

import javax.annotation.processing.Filer;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

public class JsonObjectProcessor extends AbstractProcessor {
    static final String PARAM_OBJECT = "obj";
    static final String PARAM_JSON_VALUE = "jsonValue";
    static final String LOCAL_JSON_OBJECT = "jsonObject";

    static final String DEFAULT_FORMAT_TO = "$L";

    private Map<TypeMirror, Format> formats;
    private List<TypeMirror> listTypes;
    private List<TypeMirror> mapTypes;
    private Map<TypeMirror, String> dbConstructors;
    private TypeMirror bytesType;

    private TypeMirror convertType;

    static class Format {
        private final String parse;
        private final String to;

        public Format(String parse, String to) {
            this.parse = parse;
            this.to = to;
        }

        public String getParse() {
            return parse;
        }

        public String getTo() {
            return to;
        }
    }

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        convertType = super.getTypeMirror(JsonValue.class);
        bytesType = super.getTypeMirror(byte[].class);

        listTypes = new ArrayList<>();
        listTypes.add(super.getTypeMirror(List.class));
        listTypes.add(super.getTypeMirror(scorex.util.ArrayList.class));

        mapTypes = new ArrayList<>();
        mapTypes.add(super.getTypeMirror(Map.class));
        mapTypes.add(super.getTypeMirror(scorex.util.HashMap.class));

        dbConstructors = new java.util.HashMap<>();
        dbConstructors.put(super.getTypeMirror(VarDB.class), "newVarDB");
        dbConstructors.put(super.getTypeMirror(ArrayDB.class), "newArrayDB");
        dbConstructors.put(super.getTypeMirror(DictDB.class), "newDictDB");

        formats = new HashMap<>();
        formats.put(super.getTypeMirror(Boolean.class),
                new Format("$L.asBoolean()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Character.class),
                new Format("(char)$L.asInt()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Byte.class),
                new Format("(byte)$L.asInt()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Short.class),
                new Format("(short)$L.asInt()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Integer.class),
                new Format("$L.asInt()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Long.class),
                new Format("$L.asLong()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Float.class),
                new Format("$L.asFloat()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(Double.class),
                new Format("$L.asDouble()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(String.class),
                new Format("$L.asString()", DEFAULT_FORMAT_TO));
        formats.put(super.getTypeMirror(BigInteger.class),
                new Format("new BigInteger($L.asString())", "$L.toString()"));
        formats.put(super.getTypeMirror(Address.class),
                new Format("Address.fromString($L.asString())", "$L.toString()"));
        formats.put(super.getTypeMirror(byte[].class),
                new Format("scorex.util.Base64.getDecoder().decode($L.asString().getBytes())",
                        "new String(scorex.util.Base64.getEncoder().encode($L))"));

    }

    @Override
    public Set<String> getSupportedAnnotationTypes() {
        Set<String> s = new HashSet<>();
        s.add(JsonObject.class.getCanonicalName());
        return s;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        boolean ret = false;
        for (TypeElement annotation : annotations) {
            Set<? extends Element> annotationElements = roundEnv.getElementsAnnotatedWith(annotation);
            for (Element element : annotationElements) {
                if (element.getKind().isClass()) {
                    messager.noteMessage("%s", element.toString());
                    generateExtendsClass(processingEnv.getFiler(), (TypeElement) element);
                    ret = true;
                } else {
                    throw new RuntimeException("not support");
                }
            }
        }
        return ret;
    }

    private void generateExtendsClass(Filer filer, TypeElement element) {
        ClassName parentClassName = ClassName.get(element);
        TypeSpec typeSpec = typeSpec(element);
        JavaFile javaFile = JavaFile.builder(parentClassName.packageName(), typeSpec).build();
        try {
            javaFile.writeTo(filer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static ClassName getJsonObjectClassName(AnnotatedTypeElement<JsonObject> annotated) {
        TypeElement element = annotated.getElement();
        JsonObject ann = annotated.getAnnotation();
        return ClassName.get(
                ClassName.get(element).packageName(),
                element.getSimpleName() + ann.suffix());
    }

    //jsonValue == null && !omit ? throw : null
    //jsonValue == Json.NULL ? null
    private CodeBlock getParseStatement(TypeMirror variableType, String jsonValue, JsonProperty annProperty) {
        CodeBlock.Builder codeBlock = CodeBlock.builder();
        if (annProperty != null && !annProperty.parser().isEmpty()) {
            codeBlock.add("$L($L.asObject())", annProperty.parser(), jsonValue);
        } else {
            Map.Entry<TypeMirror, Format> entry = getFormat(variableType);
            if (entry != null) {
                codeBlock.add(entry.getValue().getParse(), jsonValue);
            } else {
                AnnotatedTypeElement<JsonObject> annotated = super.getAnnotatedTypeElement(variableType, JsonObject.class);
                if (annotated != null) {
                    ClassName fieldClassName = getJsonObjectClassName(annotated);
                    codeBlock.add("$T.$L($L.asObject())", fieldClassName, annotated.getAnnotation().parse(), jsonValue);
                } else {
                    String method = super.findMethod(variableType, ".*",
                            variableType,
                            new Modifier[]{Modifier.PUBLIC, Modifier.STATIC},
                            convertType);
                    if (method != null) {
                        codeBlock.add("$T.$L($L.asObject())", variableType, method, jsonValue);
                    } else {
                        throw new RuntimeException(String.format("%s class is not JsonObject convertible, refer %s", variableType, jsonValue));
                    }
                }
            }
        }
        return codeBlock.build();
    }

    //value == null && !omit ? null => Json.NULL
    private CodeBlock getToJsonStatement(TypeMirror variableType, String variableName, JsonProperty annProperty) {
        CodeBlock.Builder codeBlock = CodeBlock.builder();
        if (annProperty != null && !annProperty.toJson().isEmpty()) {
            codeBlock.add("$L($L)", annProperty.toJson(), variableName);
        } else {
            Map.Entry<TypeMirror, Format> entry = getFormat(variableType);
            if (entry != null) {
                String toJson = String.format(entry.getValue().getTo(), variableName);
                if (!variableType.getKind().isPrimitive()) {
                    codeBlock.add("$L == null ? $T.NULL : ", variableName, Json.class);
                }
                codeBlock
                        .add("$T.value(", Json.class)
                        .add(entry.getValue().getTo(), variableName)
                        .add(")");
            } else {
                AnnotatedTypeElement<JsonObject> annotated = super.getAnnotatedTypeElement(variableType, JsonObject.class);
                if (annotated != null) {
                    ClassName fieldClassName = getJsonObjectClassName(annotated);
                    codeBlock
                            .add("$L == null ? $T.NULL : ", variableName, Json.class)
                            .add("$T.$L($L)",
                                    fieldClassName, annotated.getAnnotation().toJson(), variableName);
                } else {
                    String method = super.findMethod(variableType, ".*",
                            convertType,
                            new Modifier[]{Modifier.PUBLIC, Modifier.STATIC},
                            variableType);
                    if (method != null) {
                        codeBlock
                                .add("$L == null ? $T.NULL : ", variableName, Json.class)
                                .add("$T.$L($L)",
                                        variableType, method, variableName);
                    } else {
                        throw new RuntimeException(String.format("%s class is not JsonObject convertible, refer %s", variableType, variableName));
                    }
                }
            }
        }
        return codeBlock.build();
    }

    private Map.Entry<TypeMirror, Format> getFormat(TypeMirror variableType) {
        for (Map.Entry<TypeMirror, Format> entry : formats.entrySet()) {
            if (typeUtil.isAssignable(variableType, entry.getKey())) {
                return entry;
            }
        }
        return null;
    }

    private TypeSpec typeSpec(TypeElement element) {
        ClassName parentClassName = ClassName.get(element);
        JsonObject annClass = element.getAnnotation(JsonObject.class);
        ClassName className = ClassName.get(parentClassName.packageName(), parentClassName.simpleName() + annClass.suffix());
        TypeSpec.Builder builder = TypeSpec
                .classBuilder(ClassName.get(parentClassName.packageName(), className.simpleName()))
                .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
                .superclass(element.asType());

        builder.addMethod(MethodSpec.constructorBuilder()
                .addStatement("super()")
                .build());
        MethodSpec.Builder constructor = MethodSpec.constructorBuilder()
                .addParameter(TypeName.get(element.asType()), PARAM_OBJECT)
                .addStatement("super()");

        builder.addMethod(MethodSpec.methodBuilder(annClass.parse())
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(String.class, "jsonString")
                .returns(className)
                .addStatement("return $T.$L($T.parse(jsonString))",
                        className, annClass.parse(), Json.class)
                .build());
        MethodSpec.Builder parseMethod = MethodSpec.methodBuilder(annClass.parse())
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(TypeName.get(convertType), PARAM_JSON_VALUE)
                .returns(className)
                .beginControlFlow("if ($L == null || $L.isNull())", PARAM_JSON_VALUE, PARAM_JSON_VALUE)
                .addStatement("return null")
                .endControlFlow()
                .addStatement("$T $L = $L.asObject()",
                        com.eclipsesource.json.JsonObject.class, LOCAL_JSON_OBJECT, PARAM_JSON_VALUE)
                .addStatement("$T obj = new $T()", className, className);
        builder.addMethod(MethodSpec.methodBuilder(annClass.toJson())
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(TypeName.get(element.asType()), PARAM_OBJECT)
                .returns(TypeName.get(convertType))
                .addStatement("return $L == null ? $T.NULL : new $T($L).$L()",
                        PARAM_OBJECT, Json.class, className, PARAM_OBJECT, annClass.toJson())
                .build());
        MethodSpec.Builder toJsonMethod = MethodSpec.methodBuilder(annClass.toJson())
                .addModifiers(Modifier.PUBLIC)
                .returns(com.eclipsesource.json.JsonObject.class)
                .addStatement("$T $L = $T.object()",
                        com.eclipsesource.json.JsonObject.class, LOCAL_JSON_OBJECT, Json.class);

        processMethod(element, constructor, parseMethod, toJsonMethod);

        builder.addMethod(constructor.build());
        builder.addMethod(parseMethod
                .addStatement("return $L", PARAM_OBJECT)
                .build());
        builder.addMethod(toJsonMethod
                .addStatement("return $L", LOCAL_JSON_OBJECT)
                .build());
        return builder.build();
    }

    private void processMethod(
            TypeElement element,
            MethodSpec.Builder constructor,
            MethodSpec.Builder parseMethod, MethodSpec.Builder toJsonMethod) {
        TypeMirror superClass = element.getSuperclass();
        TypeElement superElement = super.getTypeElement(superClass);
        if (superElement != null) {
            processMethod(superElement, constructor, parseMethod, toJsonMethod);
        }

        for (Element enclosedElement : element.getEnclosedElements()) {
            if (enclosedElement.getKind().equals(ElementKind.FIELD) &&
                    !ProcessorUtil.hasModifier(enclosedElement, Modifier.STATIC)) {
                VariableElement variableElement = (VariableElement) enclosedElement;
                TypeMirror fieldType = variableElement.asType();
                JsonProperty annField = variableElement.getAnnotation(JsonProperty.class);
                if (annField != null && annField.ignore()) {
                    continue;
                }
                if (super.containsDeclaredType(dbConstructors.keySet(), fieldType)) {
                    continue;
                }

                String field = variableElement.getSimpleName().toString();
                String property = field;
                String capitalized = field.substring(0, 1).toUpperCase() + field.substring(1);
                String getter = (fieldType.getKind() == TypeKind.BOOLEAN ? "is" : "get") + capitalized;
                String setter = "set" + capitalized;

                boolean direct = false;
                if (annField != null) {
                    direct = annField.direct();
                    if (!annField.value().isEmpty()) {
                        property = annField.value();
                    }
                    if (!annField.getter().isEmpty()) {
                        getter = annField.getter();
                    }
                    if (!annField.setter().isEmpty()) {
                        setter = annField.setter();
                    }
                }

                if (direct) {
                    constructor.addStatement("this.$L = $L.$L", field, PARAM_OBJECT, field);
                    toJsonMethod.addStatement("$T $L = this.$L", fieldType, field, field);
                } else {
                    constructor.addStatement("this.$L($L.$L())", setter, PARAM_OBJECT, getter);
                    toJsonMethod.addStatement("$T $L = this.$L()", fieldType, field, getter);
                }

                String jsonValue = field + "JsonValue";
                parseMethod.addStatement("$T $L = $L.get(\"$L\")", JsonValue.class, jsonValue, LOCAL_JSON_OBJECT, property);
                parseMethod.beginControlFlow("if ($L != null && !$L.isNull())", jsonValue, jsonValue);

                CodeBlock setterValue;
                boolean isArray = fieldType.getKind() == TypeKind.ARRAY;
                if (super.containsDeclaredType(mapTypes, fieldType)) {
                    List<? extends TypeMirror> types = ((DeclaredType) fieldType).getTypeArguments();
                    TypeMirror keyType = types.get(0);
                    TypeMirror valueType = types.get(1);
                    String jsonObjectName = field + "JsonObject";
                    toJsonMethod
                            .addStatement("$T $L = $T.object()",
                                    com.eclipsesource.json.JsonObject.class, jsonObjectName, Json.class)
                            .beginControlFlow("for($T<$T,$T> entry : $L.entrySet())",
                                    Map.Entry.class, keyType, valueType, field)
                            .addStatement("$L.add(entry.getKey(), $L)", jsonObjectName, getToJsonStatement(valueType, "entry.getValue()", annField))
                            .endControlFlow();

                    parseMethod
                            .addStatement("$T $L = $L.asObject()",
                                    com.eclipsesource.json.JsonObject.class, jsonObjectName, jsonValue)
                            .addStatement("$T $L = new $T<>()", fieldType, field, scorex.util.HashMap.class)
                            .beginControlFlow("for(String name : $L.names())", jsonObjectName)
                            .addStatement("$L.put(name, $L)", field, getParseStatement(valueType, jsonObjectName + ".get(name)", annField))
                            .endControlFlow();
                    setterValue = CodeBlock.builder().add("$L",field).build();
                    jsonValue = jsonObjectName;
                } else if (!typeUtil.isSameType(fieldType, bytesType)  &&
                        (isArray || super.containsDeclaredType(listTypes, fieldType))) {
                    TypeMirror componentType = ProcessorUtil.getComponentType(fieldType);
                    int componentDepth = ProcessorUtil.getComponentTypeDepth(fieldType);
                    if (componentType.getKind() == TypeKind.BYTE) {
                        componentDepth--;
                        componentType = typeUtil.getArrayType(componentType);
                    }

                    String jsonArrayName = field + "JsonArray";
                    if (isArray) {
                        TypeMirror constructComponentType = typeUtil.isSameType(componentType, bytesType) ? ProcessorUtil.getComponentType(fieldType) : componentType;

                        for(int i=0; i<componentDepth; i++) {
                            String braket = "[]".repeat(componentDepth-i-1);
                            if (constructComponentType.getKind() == TypeKind.BYTE) {
                                braket = "[]".repeat(componentDepth-i);
                            }
                            String suffix = (i == 0 ?"":Integer.toString(i));
                            String jsonArray = field+"JsonArray"+suffix;
                            String fieldName = field+suffix;
                            String index = "i"+suffix;
                            parseMethod
                                    .addStatement("$T $L = $L.asArray()", JsonArray.class, jsonArray, field+"JsonValue"+suffix)
                                    .addStatement("$T[]$L $L = new $T[$L.size()]$L",
                                            constructComponentType,braket,fieldName, constructComponentType,jsonArray,braket)
                                    .beginControlFlow("for(int $L=0; $L<$L.size(); $L++)",
                                            index,index,jsonArray,index)
                                    .addStatement("$T $L = $L.get($L)", JsonValue.class, field+"JsonValue"+(i+1), jsonArray, index)
                                    .beginControlFlow("if (!$L.isNull())", field+"JsonValue"+(i+1));
                            toJsonMethod
                                    .beginControlFlow("if ($L != null)", fieldName)
                                    .addStatement("$T $L = $T.array()", JsonArray.class, jsonArray, Json.class)
                                    .beginControlFlow("for($T$L $L : $L)",
                                            constructComponentType, braket, (i+1) == componentDepth ? "v" : field+(i+1), fieldName);
                        }
                        for(int i=componentDepth; i>0; i--) {
                            String suffix = (i == 1 ?"":Integer.toString(i-1));
                            String jsonArray = field+"JsonArray"+suffix;
                            String fieldName = field+suffix;
                            String index = "i"+suffix;
                            if (i == componentDepth) {
//                                setterValue = getParseStatement(componentType, jsonArray+".get("+index+")", annField);
                                setterValue = getParseStatement(componentType, field+"JsonValue"+i, annField);
                                toJsonMethod.addStatement("$L.add($L)",
                                        jsonArray, getToJsonStatement(componentType, "v", annField));
                            } else {
                                setterValue = CodeBlock.builder().add("$L",field+i).build();
                            }

                            CodeBlock addCode;
                            CodeBlock addNullCode;
                            if (i == 1) {
                                addCode = CodeBlock.builder().addStatement("$L.add(\"$L\", $L)", LOCAL_JSON_OBJECT, property, jsonArray).build();
                                addNullCode = CodeBlock.builder().addStatement("$L.add(\"$L\", $T.NULL)", LOCAL_JSON_OBJECT, property, Json.class).build();
                            } else {
                                String parentSuffix = (i <= 2 ?"":Integer.toString(i-2));
                                String parentArray = field+"JsonArray"+parentSuffix;
                                addCode = CodeBlock.builder().addStatement("$L.add($L)", parentArray, jsonArray).build();
                                addNullCode = CodeBlock.builder().addStatement("$L.add($T.NULL)", parentArray, Json.class).build();
                            }
                            toJsonMethod.endControlFlow(); //endControlFlow("for")
                            toJsonMethod.addCode(addCode);
                            toJsonMethod.nextControlFlow("else");
                            toJsonMethod.addCode(addNullCode);
                            toJsonMethod.endControlFlow(); //endControlFlow("if")
                            parseMethod
                                    .addStatement("$L[$L] = $L", fieldName,index, setterValue)
                                    .endControlFlow()
                                    .endControlFlow();
                        }
                    } else {
                        setterValue = getParseStatement(componentType, jsonArrayName + ".get(i)", annField);
                        parseMethod
                                .addStatement("$T $L = $L.asArray()", JsonArray.class, jsonArrayName, jsonValue)
                                .addStatement("$T $L = new $T<>()", fieldType, field, scorex.util.ArrayList.class)
                                .beginControlFlow("for(int i=0; i<$L.size(); i++)", jsonArrayName)
                                .addStatement("$L.add($L)", field, setterValue)
                                .endControlFlow();

                        toJsonMethod
                                .addStatement("$T $L = $T.array()", JsonArray.class, jsonArrayName, com.eclipsesource.json.Json.class)
                                .beginControlFlow("for($T v : $L)", componentType, field)
                                .addStatement("$L.add($L)", jsonArrayName, getToJsonStatement(componentType, "v", annField))
                                .endControlFlow()
                                .addStatement("$L.add(\"$L\", $L)", LOCAL_JSON_OBJECT, property, jsonArrayName);
                    }
                    setterValue = CodeBlock.builder().add("$L", field).build();
                } else {
                    setterValue = getParseStatement(fieldType, jsonValue, annField);
                    toJsonMethod
                            .addStatement("$T $L = $L",
                                JsonValue.class, jsonValue, getToJsonStatement(fieldType, field, annField))
                            .addStatement("$L.add(\"$L\", $L)", LOCAL_JSON_OBJECT, property, jsonValue);
                }
                if (direct) {
                    parseMethod.addStatement("obj.$L = $L", field, setterValue);
                } else {
                    parseMethod.addStatement("obj.$L($L)", setter, setterValue);
                }
                parseMethod.endControlFlow();
            }
        }
    }
}
