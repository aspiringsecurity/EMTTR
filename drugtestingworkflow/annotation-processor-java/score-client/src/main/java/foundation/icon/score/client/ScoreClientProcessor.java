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

package foundation.icon.score.client;

import com.squareup.javapoet.*;
import foundation.icon.annotation_processor.AbstractProcessor;
import foundation.icon.annotation_processor.ProcessorUtil;
import foundation.icon.jsonrpc.Address;
import foundation.icon.jsonrpc.IconStringConverter;
import foundation.icon.jsonrpc.TypeReference;
import foundation.icon.jsonrpc.model.TransactionResult;
import score.annotation.EventLog;
import score.annotation.External;
import score.annotation.Payable;

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
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class ScoreClientProcessor extends AbstractProcessor {
    static final String METHOD_OF = "_of";
    static final String PARAM_PROPERTEIS = "properties";
    static final String PARAM_PREFIX = "prefix";
    static final String METHOD_DEPLOY = "_deploy";
    static final String PARAM_URL = "url";
    static final String PARAM_NID = "nid";
    static final String PARAM_STEP_LIMIT = "stepLimit";
    static final String PARAM_WALLET = "wallet";
    static final String PARAM_ADDRESS = "address";
    static final String PARAM_CLIENT = "client";
    static final String PARAM_SCORE_FILE_PATH = "scoreFilePath";
    static final String PARAM_PARAMS = "params";
    //
    static final String PARAM_PAYABLE_VALUE = "valueForPayable";
    static final String PARAM_CONSUMER = "consumerFunc";
    //
    static final String PARAM_TXR = "txr";
    static final String PARAM_FILTER = "filter";
    static final String METHOD_EVENT_LOGS = "eventLogs";

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
    }

    @Override
    public Set<String> getSupportedAnnotationTypes() {
        Set<String> s = new HashSet<>();
        s.add(ScoreClient.class.getCanonicalName());
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
                if (element.getKind().isInterface() || element.getKind().isClass() || element.getKind().isField()) {
                    messager.noteMessage("process %s %s", element.getKind(), element.asType(), element.getSimpleName());
                    generateImplementClass(processingEnv.getFiler(), element);
                    ret = true;
                } else {
                    throw new RuntimeException("not support, element:" + element);
                }
            }
        }
        return ret;
    }

    private void generateImplementClass(Filer filer, Element element) {
        TypeElement typeElement;
        if (element instanceof TypeElement) {
            typeElement = (TypeElement) element;
        } else if (element instanceof VariableElement) {
            typeElement = super.getTypeElement(element.asType());
        } else {
            throw new RuntimeException("not support, element:" + element);
        }

        ClassName elementClassName = ClassName.get(typeElement);
        String suffix = element.getAnnotation(ScoreClient.class).suffix();
        ClassName className = ClassName.get(elementClassName.packageName(),
                elementClassName.simpleName() + suffix);
        TypeSpec typeSpec = typeSpec(className, typeElement);
        JavaFile javaFile = JavaFile.builder(className.packageName(), typeSpec).build();
        try {
            javaFile.writeTo(filer);
        } catch (IOException e) {
            messager.warningMessage("create javaFile error : %s", e.getMessage());
        }
    }

    private TypeSpec typeSpec(ClassName className, TypeElement element) {
        TypeSpec.Builder builder = TypeSpec
                .classBuilder(className)
                .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
                .superclass(DefaultScoreClient.class)
                .addSuperinterfaces(ProcessorUtil.getSuperinterfaces(element));

        if (element.getKind().isInterface()) {
            builder.addSuperinterface(element.asType());
        }

        //Constructor
        builder.addMethod(MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addParameter(ParameterSpec.builder(String.class, PARAM_URL).build())
                .addParameter(ParameterSpec.builder(BigInteger.class, PARAM_NID).build())
                .addParameter(ParameterSpec.builder(Wallet.class, PARAM_WALLET).build())
                .addParameter(ParameterSpec.builder(Address.class, PARAM_ADDRESS).build())
                .addStatement("super($L, $L, $L, $L)",
                        PARAM_URL, PARAM_NID, PARAM_WALLET, PARAM_ADDRESS).build());
        builder.addMethod(MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addParameter(ParameterSpec.builder(String.class, PARAM_URL).build())
                .addParameter(ParameterSpec.builder(BigInteger.class, PARAM_NID).build())
                .addParameter(ParameterSpec.builder(BigInteger.class, PARAM_STEP_LIMIT).build())
                .addParameter(ParameterSpec.builder(Wallet.class, PARAM_WALLET).build())
                .addParameter(ParameterSpec.builder(Address.class, PARAM_ADDRESS).build())
                .addStatement("super($L, $L, $L, $L, $L)",
                        PARAM_URL, PARAM_NID, PARAM_STEP_LIMIT, PARAM_WALLET, PARAM_ADDRESS).build());
        builder.addMethod(MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addParameter(ParameterSpec.builder(DefaultScoreClient.class, PARAM_CLIENT).build())
                .addStatement("super($L)", PARAM_CLIENT).build());
        builder.addMethod(MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addParameter(ParameterSpec.builder(DefaultScoreClient.class, PARAM_CLIENT).build())
                .addParameter(ParameterSpec.builder(Wallet.class, PARAM_WALLET).build())
                .addStatement("super($L, $L)", PARAM_CLIENT, PARAM_WALLET).build());

        //_of(Properties)
        builder.addMethod(MethodSpec.methodBuilder(METHOD_OF)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ParameterSpec.builder(Properties.class, PARAM_PROPERTEIS).build())
                .addStatement("return _of(\"\", $L)", PARAM_PROPERTEIS)
                .returns(className)
                .build());
        //_of(String prefix, Properties)
        builder.addMethod(MethodSpec.methodBuilder(METHOD_OF)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ParameterSpec.builder(String.class, PARAM_PREFIX).build())
                .addParameter(ParameterSpec.builder(Properties.class, PARAM_PROPERTEIS).build())
                .addStatement("return new $T($T.of($L, $L))", className, DefaultScoreClient.class, PARAM_PREFIX, PARAM_PROPERTEIS)
                .returns(className)
                .build());

        builder.addMethods(overrideMethods(element));
        builder.addMethods(deployMethods(className, element));
        builder.addTypes(eventTypes(element));
        return builder.build();
    }

    private List<MethodSpec> overrideMethods(TypeElement element) {
        List<MethodSpec> methods = new ArrayList<>();
        TypeMirror superClass = element.getSuperclass();
        if (!superClass.getKind().equals(TypeKind.NONE) && !superClass.toString().equals(Object.class.getName())) {
            messager.noteMessage("superClass[kind:%s, name:%s]", superClass.getKind().name(), superClass.toString());
            List<MethodSpec> superMethods = overrideMethods(super.getTypeElement(element.getSuperclass()));
            addMethods(methods, superMethods, element);
        }

        for (TypeMirror inf : element.getInterfaces()) {
            TypeElement infElement = super.getTypeElement(inf);
            List<MethodSpec> infMethods = overrideMethods(infElement);
            addMethods(methods, infMethods, element);
        }

        boolean mustGenerate = element.getKind().isInterface();
        for (Element enclosedElement : element.getEnclosedElements()) {
            if (ElementKind.METHOD.equals(enclosedElement.getKind()) &&
                    ProcessorUtil.hasModifier(enclosedElement, Modifier.PUBLIC) &&
                    !ProcessorUtil.hasModifier(enclosedElement, Modifier.STATIC)) {
                ExecutableElement ee = (ExecutableElement) enclosedElement;
                External external = ee.getAnnotation(External.class);
                EventLog eventLog = ee.getAnnotation(EventLog.class);
                if (external != null || mustGenerate) {
                    CodeBlock paramsCodeblock = paramsCodeblock(ee);
                    MethodSpec methodSpec = methodSpec(ee, paramsCodeblock);
                    addMethod(methods, methodSpec, element);
                    boolean readonly = external != null ?
                            external.readonly() :
                            !methodSpec.returnType.equals(TypeName.VOID);
                    if (!readonly && eventLog == null) {
                        addMethod(methods, consumerMethodSpec(methodSpec, paramsCodeblock, false), element);
                    }
                    if (ee.getAnnotation(Payable.class) != null) {
                        if (readonly) {
                            messager.warningMessage(
                                    "Method annotated @Payable cannot be readonly '%s' in %s",
                                    ee, element.getQualifiedName());
                        } else {
                            addMethod(methods, payableMethodSpec(methodSpec, paramsCodeblock), element);
                            addMethod(methods, consumerMethodSpec(methodSpec, paramsCodeblock, true), element);
                        }
                    }
                }

                if (eventLog != null) {
                    addMethod(methods, eventConsumerMethodSpec(ee), element);
                }
            }
        }
        return methods;
    }

    private void addMethods(List<MethodSpec> methods, List<MethodSpec> methodSpecs, TypeElement element) {
        for (MethodSpec methodSpec : methodSpecs) {
            addMethod(methods, methodSpec, element);
        }
    }

    private void addMethod(List<MethodSpec> methods, MethodSpec methodSpec, TypeElement element) {
        if (methodSpec != null) {
            MethodSpec conflictMethod = ProcessorUtil.getConflictMethod(methods, methodSpec);
            if (conflictMethod != null) {
                methods.remove(conflictMethod);
                if (element.getKind().isInterface()) {
                    messager.warningMessage(
                            "Redeclare '%s %s(%s)' in %s",
                            conflictMethod.returnType.toString(),
                            conflictMethod.name,
                            ProcessorUtil.parameterSpecToString(conflictMethod.parameters),
                            element.getQualifiedName());
                }
            }
            methods.add(methodSpec);
        }
    }

    private CodeBlock paramsCodeblock(ExecutableElement element) {
        if (element == null || element.getParameters() == null || element.getParameters().size() == 0) {
            return null;
        }
        List<ParameterSpec> parameterSpecs = ProcessorUtil.getParameterSpecs(element);
        Set<String> nameSet = parameterSpecs.stream()
                .map((v) -> v.name).collect(Collectors.toSet());
        String paramsName = newParameterName(nameSet, PARAM_PARAMS);

        CodeBlock.Builder builder = CodeBlock.builder();
        builder.addStatement("$T<$T,$T> $L = new $T<>()",
                Map.class, String.class, Object.class, paramsName, HashMap.class);
        for (ParameterSpec ps : parameterSpecs) {
            builder.addStatement("$L.put(\"$L\",$L)", paramsName, ps.name, ps.name);
        }
        return builder.build();
    }

    static Map<TypeKind, TypeName> wrapperTypeNames = Map.of(
            TypeKind.BOOLEAN, TypeName.get(Boolean.class),
            TypeKind.BYTE, TypeName.get(Boolean.class),
            TypeKind.SHORT, TypeName.get(Byte.class),
            TypeKind.INT, TypeName.get(Integer.class),
            TypeKind.LONG, TypeName.get(Long.class),
            TypeKind.CHAR, TypeName.get(Character.class),
            TypeKind.FLOAT, TypeName.get(Float.class),
            TypeKind.DOUBLE, TypeName.get(Double.class));

    private MethodSpec methodSpec(ExecutableElement ee, CodeBlock paramsCodeblock) {
        if (ee.getAnnotation(EventLog.class) != null) {
            return notSupportedMethod(ee, "not supported EventLog method", null);
        }

        String methodName = ee.getSimpleName().toString();
        TypeMirror returnType = ee.getReturnType();
        TypeName returnTypeName = TypeName.get(returnType);
        External external = ee.getAnnotation(External.class);

        List<ParameterSpec> parameterSpecs = ProcessorUtil.getParameterSpecs(ee);
        MethodSpec.Builder builder = MethodSpec
                .methodBuilder(methodName)
                .addModifiers(ProcessorUtil.getModifiers(ee, Modifier.ABSTRACT))
                .addParameters(parameterSpecs)
                .returns(returnTypeName);
//                .addAnnotation(Override.class);

        String params = "null";
        if (paramsCodeblock != null) {
            builder.addCode(paramsCodeblock);
            params = newParameterNameMap(parameterSpecs, PARAM_PARAMS).get(PARAM_PARAMS);
        }

        boolean isVoid = returnTypeName.equals(TypeName.VOID);
        boolean isExternal = external != null ? !external.readonly() : isVoid;
        if (isExternal) {
            if (isVoid) {
                builder.addStatement("super._send(\"$L\", $L)", methodName, params);
                if (ee.getAnnotation(Payable.class) != null) {
                    builder.addJavadoc("To payable, use $L($T $L, ...)", methodName, BigInteger.class, PARAM_PAYABLE_VALUE);
                }
            } else {
                return notSupportedMethod(ee, "not supported response of writable method in ScoreClient",
                        CodeBlock.builder().add("$L($T<$T> $L, ...)",
                                methodName, Consumer.class, TransactionResult.class, PARAM_CONSUMER).build());
            }
        } else {
            if (!isVoid) {
                if (returnType.getKind().isPrimitive()) {
                    builder.addStatement("return super._call($T.class, \"$L\", $L)",
                            wrapperTypeNames.get(returnType.getKind()), methodName, params);
                } else {
                    if (returnType.getKind().equals(TypeKind.DECLARED) &&
                            ((DeclaredType) returnType).getTypeArguments().size() > 0) {
                        builder.addStatement("return super._call(new $T<$T>(){}, \"$L\", $L)",
                                TypeReference.class, returnTypeName, methodName, params);
                    } else {
                        builder.addStatement("return super._call($T.class, \"$L\", $L)",
                                returnTypeName, methodName, params);
                    }
                }
            } else {
                return notSupportedMethod(ee, "not supported, void of readonly method in ScoreClient", null);
            }
        }
        return builder.build();
    }

    private MethodSpec payableMethodSpec(MethodSpec methodSpec, CodeBlock paramsCodeblock) {
        Map<String, String> paramNameMap = newParameterNameMap(methodSpec.parameters,
                PARAM_PAYABLE_VALUE, PARAM_PARAMS);
        MethodSpec.Builder builder = MethodSpec.methodBuilder(methodSpec.name)
                .addModifiers(methodSpec.modifiers)
                .addParameter(BigInteger.class, paramNameMap.get(PARAM_PAYABLE_VALUE))
                .addParameters(methodSpec.parameters)
                .returns(TypeName.VOID);

        String params = "null";
        if (paramsCodeblock != null) {
            builder.addCode(paramsCodeblock);
            params = paramNameMap.get(PARAM_PARAMS);
        }
        builder.addStatement("super._send($L, \"$L\", $L)",
                paramNameMap.get(PARAM_PAYABLE_VALUE),
                methodSpec.name, params);
        return builder.build();
    }

    private MethodSpec consumerMethodSpec(MethodSpec methodSpec, CodeBlock paramsCodeblock, boolean isPayable) {
        Map<String, String> paramNameMap = newParameterNameMap(methodSpec.parameters,
                PARAM_CONSUMER, PARAM_PAYABLE_VALUE, PARAM_PARAMS);
        MethodSpec.Builder builder = MethodSpec.methodBuilder(methodSpec.name)
                .addModifiers(methodSpec.modifiers)
                .addParameter(ParameterSpec.builder(
                        ParameterizedTypeName.get(Consumer.class, TransactionResult.class),
                        paramNameMap.get(PARAM_CONSUMER)).build())
                .returns(TypeName.VOID);

        String params = "null";
        if (paramsCodeblock != null) {
            builder.addCode(paramsCodeblock);
            params = paramNameMap.get(PARAM_PARAMS);
        }
        if (isPayable) {
            builder.addParameter(BigInteger.class, paramNameMap.get(PARAM_PAYABLE_VALUE))
                    .addStatement("$L.accept(super._send($L, \"$L\", $L))",
                            paramNameMap.get(PARAM_CONSUMER),
                            paramNameMap.get(PARAM_PAYABLE_VALUE),
                            methodSpec.name, params);
        } else {
            builder.addStatement("$L.accept(super._send(\"$L\", $L))",
                    paramNameMap.get(PARAM_CONSUMER),
                    methodSpec.name, params);
        }
        return builder.addParameters(methodSpec.parameters).build();
    }

    private MethodSpec notSupportedMethod(ExecutableElement ee, String msg, CodeBlock instead) {
        String methodName = ee.getSimpleName().toString();
        TypeName returnTypeName = TypeName.get(ee.getReturnType());
        return MethodSpec.methodBuilder(methodName)
                .addModifiers(ProcessorUtil.getModifiers(ee, Modifier.ABSTRACT))
                .addParameters(ProcessorUtil.getParameterSpecs(ee))
                .returns(returnTypeName)
                .addStatement("throw new $T(\"$L\")", RuntimeException.class, msg)
                .addJavadoc("@deprecated Do not use this method, this is generated only for preventing compile error.\n Instead, use $L\n",
                        instead != null ? instead : "N/A")
                .addJavadoc("@throws $L(\"$L\")", RuntimeException.class.getName(), msg)
                .addAnnotation(Deprecated.class)
                .build();
    }

    private List<MethodSpec> deployMethods(ClassName className, TypeElement element) {
        List<MethodSpec> methods = new ArrayList<>();
        if (element.getKind().isInterface()) {
            methods.add(deployMethodSpec(className, null, null));
            methods.add(ofMethodSpec(className, null, null));
        } else {
            for (Element enclosedElement : element.getEnclosedElements()) {
                if (ElementKind.CONSTRUCTOR.equals(enclosedElement.getKind()) &&
                        ProcessorUtil.hasModifier(enclosedElement, Modifier.PUBLIC)) {
                    ExecutableElement ee = (ExecutableElement) enclosedElement;
                    List<ParameterSpec> parameterSpecs = ProcessorUtil.getParameterSpecs(ee);
                    CodeBlock paramsCodeblock = paramsCodeblock(ee);
                    methods.add(deployMethodSpec(className, parameterSpecs, paramsCodeblock));
                    if (parameterSpecs.size() > 0) {
                        methods.add(ofMethodSpec(className, parameterSpecs, paramsCodeblock));
                    }
                }
            }
        }
        return methods;
    }

    static String newParameterName(Set<String> nameSet, String name) {
        return nameSet != null && nameSet.contains(name) ? newParameterName(nameSet, "_" + name) : name;
    }

    static Map<String, String> newParameterNameMap(List<ParameterSpec> parameterSpecs, String ... names) {
        Set<String> nameSet = parameterSpecs == null ? null :
                parameterSpecs.stream().map((v) -> v.name).collect(Collectors.toSet());
        Map<String, String> nameMap = new HashMap<>();
        for (String name : names) {
            nameMap.put(name, newParameterName(nameSet, name));
        }
        return nameMap;
    }

    private MethodSpec deployMethodSpec(ClassName className, List<ParameterSpec> parameterSpecs, CodeBlock paramsCodeblock) {
        Map<String, String> paramNameMap = newParameterNameMap(parameterSpecs,
                PARAM_URL, PARAM_NID, PARAM_WALLET, PARAM_SCORE_FILE_PATH, PARAM_PARAMS);
        MethodSpec.Builder builder = MethodSpec.methodBuilder(METHOD_DEPLOY)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ParameterSpec.builder(String.class, paramNameMap.get(PARAM_URL)).build())
                .addParameter(ParameterSpec.builder(BigInteger.class, paramNameMap.get(PARAM_NID)).build())
                .addParameter(ParameterSpec.builder(Wallet.class, paramNameMap.get(PARAM_WALLET)).build())
                .addParameter(ParameterSpec.builder(String.class, paramNameMap.get(PARAM_SCORE_FILE_PATH)).build())
                .returns(className);
        if (parameterSpecs == null) {
            builder.addParameter(ParameterSpec.builder(
                    ParameterizedTypeName.get(Map.class, String.class, Object.class), PARAM_PARAMS).build());
        } else {
            builder.addParameters(parameterSpecs);
        }

        if (paramsCodeblock != null) {
            builder.addCode(paramsCodeblock);
        }
        builder
                .addStatement("return new $T($T._deploy($L,$L,$L,$L,$L))",
                        className, DefaultScoreClient.class,
                        paramNameMap.get(PARAM_URL),
                        paramNameMap.get(PARAM_NID),
                        paramNameMap.get(PARAM_WALLET),
                        paramNameMap.get(PARAM_SCORE_FILE_PATH),
                        paramsCodeblock != null || parameterSpecs == null ? paramNameMap.get(PARAM_PARAMS) : "null")
                .build();
        return builder.build();
    }

    private MethodSpec ofMethodSpec(ClassName className, List<ParameterSpec> parameterSpecs, CodeBlock paramsCodeblock) {
        Map<String, String> paramNameMap = newParameterNameMap(parameterSpecs,
                PARAM_PREFIX, PARAM_PROPERTEIS, PARAM_PARAMS);
        MethodSpec.Builder builder = MethodSpec.methodBuilder(METHOD_OF)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ParameterSpec.builder(String.class, paramNameMap.get(PARAM_PREFIX)).build())
                .addParameter(ParameterSpec.builder(Properties.class, paramNameMap.get(PARAM_PROPERTEIS)).build())
                .returns(className);
        if (parameterSpecs == null) {
            builder.addParameter(ParameterSpec.builder(
                    ParameterizedTypeName.get(Map.class, String.class, Object.class), PARAM_PARAMS).build());
        } else {
            builder.addParameters(parameterSpecs);
        }

        if (paramsCodeblock != null) {
            builder.addCode(paramsCodeblock);
        }
        builder
                .addStatement("return new $T($T.of($L, $L, $L))",
                        className, DefaultScoreClient.class,
                        paramNameMap.get(PARAM_PREFIX),
                        paramNameMap.get(PARAM_PROPERTEIS),
                        paramsCodeblock != null || parameterSpecs == null ? paramNameMap.get(PARAM_PARAMS) : "null")
                .build();
        return builder.build();
    }

    private List<TypeSpec> eventTypes(TypeElement element) {
        List<TypeSpec> types = new ArrayList<>();
        TypeMirror superClass = element.getSuperclass();
        if (!superClass.getKind().equals(TypeKind.NONE) && !superClass.toString().equals(Object.class.getName())) {
            messager.noteMessage("superClass[kind:%s, name:%s]", superClass.getKind().name(), superClass.toString());
            List<TypeSpec> superEvents = eventTypes(super.getTypeElement(element.getSuperclass()));
            addEventTypes(types, superEvents, element);
        }

        for (TypeMirror inf : element.getInterfaces()) {
            TypeElement infElement = super.getTypeElement(inf);
            List<TypeSpec> infEvents = eventTypes(infElement);
            addEventTypes(types, infEvents, element);
        }

        for (Element enclosedElement : element.getEnclosedElements()) {
            if (ElementKind.METHOD.equals(enclosedElement.getKind()) &&
                    ProcessorUtil.hasModifier(enclosedElement, Modifier.PUBLIC) &&
                    !ProcessorUtil.hasModifier(enclosedElement, Modifier.STATIC)) {
                ExecutableElement ee = (ExecutableElement) enclosedElement;
                EventLog eventLog = ee.getAnnotation(EventLog.class);
                if (eventLog != null) {
                    TypeSpec eventTypeSpec = eventTypeSpec(ee, element);
                    addEventType(types, eventTypeSpec, element);
                }
            }
        }
        return types;
    }

    private void addEventTypes(List<TypeSpec> types, List<TypeSpec> typeSpecs, TypeElement element) {
        for (TypeSpec typeSpec : typeSpecs) {
            addEventType(types, typeSpec, element);
        }
    }

    private void addEventType(List<TypeSpec> types, TypeSpec typeSpec, TypeElement element) {
        if (typeSpec != null) {
            for (TypeSpec type : types) {
                if (type.name.equals(typeSpec.name)) {
                    messager.warningMessage(
                            "Redeclare '%s' in %s",
                            type.name,
                            element.getQualifiedName());
                    types.remove(type);
                    break;
                }
            }
            types.add(typeSpec);
        }
    }

    private static final Map<String, String> eventTypeStrings = Map.of(
            "byte", "int",
            "char", "int",
            "short", "int",
            "int", "int",
            "long", "int",
            "java.math.BigInteger", "int",
            "java.lang.String", "str",
            "byte[]", "bytes",
            "boolean", "bool",
            "score.Address", "Address");

    private static final Map<String, String> eventConverts = Map.of(
            "byte", "this.$L=$T.toBigInteger(%s).byteValue()",
            "char", "this.$L=(char)$T.toBigInteger(%s).intValue()",
            "short", "this.$L=$T.toBigInteger(%s).shortValue()",
            "int", "this.$L=$T.toBigInteger(%s).intValue()",
            "long", "this.$L=$T.toBigInteger(%s).longValue()",
            "java.math.BigInteger", "this.$L=$T.toBigInteger(%s)",
            "byte[]", "this.$L=$T.toBytes(%s)",
            "boolean", "this.$L=$T.toBoolean(%s)",
            "score.Address", "this.$L=$T.toAddress(%s)"
    );

    private TypeSpec eventTypeSpec(ExecutableElement ee, Element element) {
        String methodName = ee.getSimpleName().toString();
        String className = methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
        TypeSpec.Builder builder = TypeSpec
                .classBuilder(className)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC);
        String FIELD_SIGNATURE = "SIGNATURE";
        String FIELD_INDEXED = "INDEXED";
        String PARAM_EVENTLOG = "el";
        String EVENTLOG_INDEXED = "indexed";
        String EVENTLOG_DATA = "data";
        MethodSpec.Builder constructorBuilder = MethodSpec.constructorBuilder()
                .addParameter(TransactionResult.EventLog.class, PARAM_EVENTLOG)
                .addModifiers(Modifier.PUBLIC)
                .addStatement("$T<String> $L = $L.getIndexed()", List.class, EVENTLOG_INDEXED, PARAM_EVENTLOG)
                .addStatement("$T<String> $L = $L.getData()", List.class, EVENTLOG_DATA, PARAM_EVENTLOG);
        StringJoiner joiner = new StringJoiner(",");
        List<FieldSpec> fields = new ArrayList<>();
        List<MethodSpec> getters = new ArrayList<>();
        int indexed = ee.getAnnotation(EventLog.class).indexed();
        //MAX_INDEXED_COUNT=3
        boolean isIndex = true;
        int i = 1;
        for (VariableElement ve : ee.getParameters()) {
            ParameterSpec ps = ParameterSpec.get(ve);
            String eventTypeString = eventTypeStrings.get(ps.type.toString());
            if (eventTypeString == null) {
                throw new RuntimeException("not allowed event parameter type, element:" + element + ", method:" + methodName + ", argument:" + ps);
            }
            joiner.add(eventTypeString);
            fields.add(FieldSpec.builder(ps.type, ps.name, Modifier.PRIVATE, Modifier.FINAL).build());
            if (isIndex && i > indexed) {
                isIndex = false;
                i = 0;
            }
            String argGetter = String.format("%s.get(%d)", isIndex ? EVENTLOG_INDEXED : EVENTLOG_DATA, i++);
            if (ps.type.toString().equals("java.lang.String")) {
                constructorBuilder.addStatement("this.$L=$L",
                        ps.name, argGetter);
            } else {
                constructorBuilder.addStatement(
                        String.format(eventConverts.get(ps.type.toString()), argGetter),
                        ps.name, IconStringConverter.class);
            }
            getters.add(MethodSpec.methodBuilder("get" + ps.name.substring(0, 1).toUpperCase() +
                            ps.name.substring(1))
                    .returns(ps.type)
                    .addModifiers(Modifier.PUBLIC)
                    .addStatement("return this.$L", ps.name)
                    .build());
        }
        builder.addField(FieldSpec.builder(String.class, FIELD_SIGNATURE, Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
                .initializer("\"$L($L)\"", methodName, joiner.toString())
                .build());
        builder.addField(FieldSpec.builder(int.class, FIELD_INDEXED, Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
                .initializer("$L", indexed)
                .build());
        builder.addFields(fields);
        builder.addMethod(constructorBuilder.build());
        builder.addMethods(getters);
        ParameterizedTypeName listEventType = ParameterizedTypeName.get(
                ClassName.get(List.class), TypeVariableName.get(className));
        builder.addMethod(MethodSpec.methodBuilder(METHOD_EVENT_LOGS)
                .addParameter(TransactionResult.class, PARAM_TXR)
                .addParameter(Address.class, PARAM_ADDRESS)
                .addParameter(ParameterizedTypeName.get(
                        ClassName.get(Predicate.class),
                        TypeVariableName.get(className)), PARAM_FILTER)
                .returns(listEventType)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addStatement("return $T.eventLogs($L, $L, $L, $L::new, $L)",
                        DefaultScoreClient.class,
                        PARAM_TXR, FIELD_SIGNATURE, PARAM_ADDRESS, className, PARAM_FILTER)
                .build());
        return builder.build();
    }

    private MethodSpec eventConsumerMethodSpec(ExecutableElement ee) {
        String methodName = ee.getSimpleName().toString();
        String className = methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
        TypeVariableName classTypeName = TypeVariableName.get(className);
        ParameterizedTypeName listEventType = ParameterizedTypeName.get(
                ClassName.get(List.class), classTypeName);
        return MethodSpec.methodBuilder(methodName)
                .addParameter(ParameterizedTypeName.get(
                        ClassName.get(Consumer.class), listEventType), PARAM_CONSUMER)
                .addParameter(ParameterizedTypeName.get(
                        ClassName.get(Predicate.class), classTypeName), PARAM_FILTER)
                .returns(ParameterizedTypeName.get(Consumer.class, TransactionResult.class))
                .addModifiers(Modifier.PUBLIC)
                .addStatement("return ($L) -> $L.accept($L.$L($L, this.address, $L))",
                        PARAM_TXR, PARAM_CONSUMER, className, METHOD_EVENT_LOGS,
                        PARAM_TXR, PARAM_FILTER)
                .build();
    }
}
