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

import com.squareup.javapoet.*;
import foundation.icon.annotation_processor.AbstractProcessor;
import foundation.icon.annotation_processor.AnnotatedTypeElement;
import foundation.icon.annotation_processor.ProcessorUtil;
import score.*;

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

public class ScoreDataObjectProcessor extends AbstractProcessor {
    static final String METHOD_READ = "readObject";
    static final String METHOD_WRITE = "writeObject";
    static final String PARAM_READER = "reader";
    static final String PARAM_WRITER = "writer";
    static final String PARAM_OBJECT = "obj";
    static final String LOCAL_OBJECT = "obj";

    static final String METHOD_TO_BYTES = "toBytes";
    static final String PARAM_BYTES = "bytes";
    static final String METHOD_FROM_BYTES = "fromBytes";

    static final String DEFAULT_FORMAT_WRITE = "writeNullable(%s)";

    private Map<TypeMirror, Format> formats;
    private List<TypeMirror> listTypes;
    private List<TypeMirror> mapTypes;
    private Map<TypeMirror, String> dbConstructors;
    private TypeMirror bytesType;

    private TypeMirror readerType;
    private TypeMirror writerType;

    static class Format {
        private final String read;
        private final String write;

        public Format(String read, String write) {
            this.read = read;
            this.write = write;
        }

        public String getRead() {
            return read;
        }

        public String getWrite() {
            return write;
        }
    }

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        readerType = super.getTypeMirror(ObjectReader.class);
        writerType = super.getTypeMirror(ObjectWriter.class);
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
                new Format("$L.readBoolean()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Character.class),
                new Format("$L.readChar()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Byte.class),
                new Format("$L.readByte()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Short.class),
                new Format("$L.readShort()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Integer.class),
                new Format("$L.readInt()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Long.class),
                new Format("$L.readLong()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Float.class),
                new Format("$L.readFloat()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Double.class),
                new Format("$L.readDouble()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(String.class),
                new Format("$L.readString()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(BigInteger.class),
                new Format("$L.readBigInteger()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(Address.class),
                new Format("$L.readAddress()", DEFAULT_FORMAT_WRITE));
        formats.put(super.getTypeMirror(byte[].class),
                new Format("$L.readByteArray()", DEFAULT_FORMAT_WRITE));
    }

    @Override
    public Set<String> getSupportedAnnotationTypes() {
        Set<String> s = new HashSet<>();
        s.add(ScoreDataObject.class.getCanonicalName());
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

    private static ClassName getScoreDataObjectClassName(AnnotatedTypeElement<ScoreDataObject> annotated) {
        TypeElement element = annotated.getElement();
        ScoreDataObject ann = annotated.getAnnotation();
        return ClassName.get(
                ClassName.get(element).packageName(),
                element.getSimpleName() + ann.suffix());
    }

    public String findReadMethod(TypeMirror type) {
        return super.findMethod(type, ".*", type,
                new Modifier[]{Modifier.PUBLIC, Modifier.STATIC},
                readerType);
    }

    public String findWriteMethod(TypeMirror type) {
        return super.findMethod(type, ".*", null,
                new Modifier[]{Modifier.PUBLIC, Modifier.STATIC},
                writerType, type);
    }

    private CodeBlock getReadCodeBlock(TypeMirror variableType, ScoreDataProperty annProperty, String field, String setter, boolean inner) {
        CodeBlock.Builder codeBlock = CodeBlock.builder();
        boolean nullable = !variableType.getKind().isPrimitive();
        boolean wrapped = false;
        String readObject = null;
        if (annProperty != null) {
            if (inner) {
                nullable = annProperty.nullableComponent() && nullable;
            } else {
                nullable = annProperty.nullable() && nullable;
            }
            if (!annProperty.readObject().isEmpty()) {
                readObject = annProperty.readObject();
            }
            wrapped = annProperty.wrapped();
        }
        String reader = PARAM_READER;
        if (wrapped) {
            reader = field+"Reader";
            codeBlock.addStatement("$T $LBytes = $L.$L($T.class)",
                        byte[].class, field, PARAM_READER, nullable ? "readNullable" : "read", byte[].class);
            if (nullable) {
                codeBlock.beginControlFlow("if ($LBytes != null)", field);
            }
            codeBlock.addStatement("$T $L = $T.newByteArrayObjectReader(\"RLPn\",$LBytes)",
                        ObjectReader.class, reader, Context.class, field);
        }
        if (readObject != null) {
            if (!nullable) {
                codeBlock.addStatement(setter,
                        CodeBlock.builder().add("$L($L)", readObject, reader).build());
            } else {
                codeBlock
                        .addStatement("$T $L = null", variableType, field)
                        .beginControlFlow("if ($L.readBoolean())", reader)
                        .addStatement("$L = $L($L)", field, readObject, reader)
                        .endControlFlow()
                        .addStatement(setter, field);
            }
        } else {
            CodeBlock.Builder formatCodeBlock = CodeBlock.builder();
            Map.Entry<TypeMirror, Format> entry = getFormat(variableType);
            if (entry != null) {
                if (!nullable) {
                    formatCodeBlock.add(entry.getValue().getRead(), reader);
                } else {
                    formatCodeBlock.add("$L.$L($T.class)",
                            reader,
                            wrapped ? "read" : "readNullable",
                            variableType);
                }
            } else {
                AnnotatedTypeElement<ScoreDataObject> annotated = super.getAnnotatedTypeElement(variableType, ScoreDataObject.class);
                if (annotated != null) {
                    ClassName fieldClassName = getScoreDataObjectClassName(annotated);
                    formatCodeBlock.add("$L.$L($T.class)",
                            reader,
                            (!nullable || wrapped)  ? "read" : "readNullable",
                            fieldClassName);
                } else {
                    String method = findReadMethod(variableType);
                    if (method != null) {
                        if (method.equals(METHOD_READ)) {
                            formatCodeBlock.add("$L.$L($T.class)",
                                    reader,
                                    (!nullable || wrapped)  ? "read" : "readNullable",
                                    variableType);
                        } else {
                            //TODO inner??
                            if (inner || !nullable) {
                                codeBlock.addStatement(setter,
                                        CodeBlock.builder().add("$T.$L($L)", variableType, method, reader).build());
                            } else {
                                codeBlock
                                        .addStatement("$T $L = null", variableType, field)
                                        .beginControlFlow("if ($L.readBoolean())", reader)
                                        .addStatement("$L = $T.$L($L)", field, variableType, method, reader)
                                        .endControlFlow()
                                        .addStatement(setter, field);
                            }
                        }
                    } else {
                        throw new RuntimeException(String.format("%s class is not ScoreDataObject convertible", variableType));
                    }
                }
            }
            if (!formatCodeBlock.isEmpty()) {
                codeBlock.addStatement(setter, formatCodeBlock.build());
            }
        }
        if (wrapped && nullable) {
            if (inner) {
                codeBlock
                        .nextControlFlow("else")
                        .addStatement(setter, "null");
            }
            codeBlock.endControlFlow();
        }
        return codeBlock.build();
    }

    private CodeBlock getWriteCodeBlock(TypeMirror variableType, ScoreDataProperty annProperty, String field, String getter, boolean inner) {
        CodeBlock.Builder codeBlock = CodeBlock.builder();
        boolean nullable = !variableType.getKind().isPrimitive();
        boolean wrapped = false;
        String writeObject = null;
        if (annProperty != null) {
            if (inner) {
                nullable = annProperty.nullableComponent() && nullable;
            } else {
                nullable = annProperty.nullable() && nullable;
            }
            if (!annProperty.writeObject().isEmpty()) {
                writeObject = annProperty.writeObject();
            }
            wrapped = annProperty.wrapped();
        }
        String writer = PARAM_WRITER;
        if (wrapped) {
            if (!inner) {
                codeBlock.addStatement("$T $L = $L", variableType, field, getter);
            }
            writer = field+"Writer";
            codeBlock.beginControlFlow("if ($L != null)", field);
            if (!nullable) {
                codeBlock.addStatement("$T $L = $T.newByteArrayObjectWriter(\"RLPn\")",
                        ByteArrayObjectWriter.class, writer, Context.class);
            } else {
                codeBlock.addStatement("$T $L = $T.newByteArrayObjectWriter(\"RLPn\")",
                                ByteArrayObjectWriter.class, writer, Context.class);
            }
        }
        if (writeObject != null) {
            //FIXME
            if (!inner && !wrapped) {
                codeBlock.addStatement("$T $L = $L", variableType, field, getter);
            }
            if (!nullable) {
                codeBlock.addStatement("$L($L, $L)", writeObject, writer, field);
            } else {
                codeBlock
                        .addStatement("$L.write($L != null)", writer, field)
                        .beginControlFlow("if ($L != null)", field)
                        .addStatement("$L($L, $L)", writeObject, writer, field)
                        .endControlFlow();
            }
        } else {
            Map.Entry<TypeMirror, Format> entry = getFormat(variableType);
            if (entry != null) {
                codeBlock.addStatement("$L.$L($L)",
                        writer,
                        (!nullable || wrapped) ? "write" : "writeNullable",
                        wrapped ? field : getter);
            } else {
                AnnotatedTypeElement<ScoreDataObject> annotated = super.getAnnotatedTypeElement(variableType, ScoreDataObject.class);
                if (annotated != null) {
                    ClassName fieldClassName = getScoreDataObjectClassName(annotated);
                    if (!inner && !wrapped) {
                        codeBlock.addStatement("$T $L = $L", variableType, field, getter);
                    }
                    if (!nullable || wrapped) {
                        codeBlock.addStatement("$L.write(new $T($L))", writer, fieldClassName, field);
                    } else {
                        codeBlock.addStatement("$L.writeNullable($L != null ? new $T($L) : null)",
                                        writer,
                                        field,
                                        fieldClassName,
                                        field);
                    }
                } else {
                    String method = findWriteMethod(variableType);
                    if (method != null) {
                        if (method.equals(METHOD_WRITE)) {
                            codeBlock.addStatement("$L.$L($L)",
                                    writer,
                                    (!nullable || wrapped) ? "write" : "writeNullable",
                                    getter);
                        } else {
                            //TODO inner??
                            if (inner || !nullable) {
                                codeBlock.addStatement("$T.$L($L, $L)", variableType, method, writer, getter);
                            } else {
                                codeBlock
                                        .addStatement("$T $L = $L", variableType, field, getter)
                                        .addStatement("$L.write($L != null)", writer, field)
                                        .beginControlFlow("if ($L != null)", field)
                                        .addStatement("$T.$L($L, $L)", variableType, method, writer, field)
                                        .endControlFlow();
                            }
                        }
                    } else {
                        throw new RuntimeException(String.format("%s class is not ScoreDataObject convertible", variableType));
                    }
                }
            }
        }
        if (wrapped) {
            codeBlock.addStatement("$L.$L($L.toByteArray())",
                    PARAM_WRITER,
                    nullable ? "writeNullable" : "write",
                    writer);
            if (nullable) {
                codeBlock
                        .nextControlFlow("else")
                        .addStatement("$L.writeNull()", PARAM_WRITER);
            }
            codeBlock.endControlFlow();
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
        ScoreDataObject annClass = element.getAnnotation(ScoreDataObject.class);
        ClassName className = ClassName.get(parentClassName.packageName(), parentClassName.simpleName() + annClass.suffix());
        TypeSpec.Builder builder = TypeSpec
                .classBuilder(ClassName.get(parentClassName.packageName(), className.simpleName()))
                .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
                .superclass(element.asType());

        builder.addMethod(MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addStatement("super()")
                .build());
        MethodSpec.Builder constructor = MethodSpec.constructorBuilder()
                .addModifiers(Modifier.PUBLIC)
                .addParameter(TypeName.get(element.asType()), PARAM_OBJECT)
                .addStatement("super()");

        if (!METHOD_READ.equals(annClass.readObject())) {
            builder.addMethod(MethodSpec.methodBuilder(annClass.readObject())
                    .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                    .addParameter(ObjectReader.class, PARAM_READER)
                    .returns(className)
                    .addStatement("return $L.$L($L)", className.simpleName(), METHOD_READ, PARAM_READER)
                    .build());
        }
        MethodSpec.Builder readMethod = MethodSpec.methodBuilder(METHOD_READ)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ObjectReader.class, PARAM_READER)
                .returns(className)
                .addStatement("$L $L = new $L()", className.simpleName(), LOCAL_OBJECT, className.simpleName());

        builder.addMethod(MethodSpec.methodBuilder(METHOD_WRITE)
                .addAnnotation(score.annotation.Keep.class)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ObjectWriter.class, PARAM_WRITER)
                .addParameter(TypeName.get(element.asType()), PARAM_OBJECT)
                .addStatement("$L.$L($L, $L instanceof $L ? ($L)$L : new $L($L))",
                        className.simpleName(), METHOD_WRITE, PARAM_WRITER,
                        PARAM_OBJECT, className.simpleName(),
                        className.simpleName(), PARAM_OBJECT,
                        className.simpleName(), PARAM_OBJECT)
                .build());
        builder.addMethod(MethodSpec.methodBuilder(METHOD_WRITE)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(ObjectWriter.class, PARAM_WRITER)
                .addParameter(className, PARAM_OBJECT)
                .addStatement("$L.$L($L)", PARAM_OBJECT, annClass.writeObject(), PARAM_WRITER)
                .build());
        MethodSpec.Builder writeMethod = MethodSpec.methodBuilder(annClass.writeObject())
                .addModifiers(Modifier.PUBLIC)
                .addParameter(ObjectWriter.class, PARAM_WRITER);

        List<VariableElement> fields = getFields(element);
        int beginOption = Integer.MAX_VALUE;
        if (annClass.wrapList()){
            readMethod.addStatement("$L.beginList()",PARAM_READER);
            writeMethod.addStatement("$L.beginList($L)",PARAM_WRITER, fields.size());

            if (!annClass.beginOfOptionalFields().isEmpty()) {
                for(int i = 0; i < fields.size(); i++) {
                    if (annClass.beginOfOptionalFields().equals(fields.get(i).getSimpleName().toString())) {
                        beginOption = i;
                    }
                }
            }
        }
        processMethod(element, constructor, readMethod, writeMethod, beginOption);
        if (annClass.wrapList()){
            writeMethod.addStatement("$L.end()", PARAM_WRITER);
            readMethod.addStatement("$L.end()",PARAM_READER);
        }
        builder.addMethod(constructor.build());

        builder.addMethod(readMethod
                .addStatement("return $L", LOCAL_OBJECT)
                .build());
        builder.addMethod(writeMethod.build());

        builder.addMethod(MethodSpec.methodBuilder(METHOD_FROM_BYTES)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(byte[].class, PARAM_BYTES)
                .returns(className)
                .addStatement("$T reader = $T.newByteArrayObjectReader(\"RLPn\", $L)",
                        ObjectReader.class, Context.class, PARAM_BYTES)
                .addStatement("return $T.$L(reader)", className, METHOD_READ)
                .build());
//        return obj instanceof PartSetIdSdo ? ((PartSetIdSdo) obj).toBytes() : new PartSetIdSdo(obj).toBytes();
        builder.addMethod(MethodSpec.methodBuilder(METHOD_TO_BYTES)
                .addModifiers(Modifier.PUBLIC)
                .returns(byte[].class)
                .addStatement("$T writer = $T.newByteArrayObjectWriter(\"RLPn\")",
                        ByteArrayObjectWriter.class, Context.class)
                .addStatement("$T.$L(writer, this)", className, METHOD_WRITE)
                .addStatement("return writer.toByteArray()")
                .build());
        builder.addMethod(MethodSpec.methodBuilder(METHOD_TO_BYTES)
                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                .addParameter(TypeName.get(element.asType()), PARAM_OBJECT)
                .returns(byte[].class)
                .addStatement("return $L instanceof $T ? (($T)$L).$L() : new $T($L).$L()",
                        PARAM_OBJECT, className,
                        className, PARAM_OBJECT, METHOD_TO_BYTES,
                        className, PARAM_OBJECT, METHOD_TO_BYTES)
                .build());
        builder.addMethod(MethodSpec.methodBuilder("toString")
                .addModifiers(Modifier.PUBLIC)
                .returns(String.class)
                .addStatement("return super.toString()")
                .build());
        return builder.build();
    }

    private List<VariableElement> getFields(TypeElement element) {
        List<VariableElement> fields = new ArrayList<>();
        TypeMirror superClass = element.getSuperclass();
        TypeElement superElement = super.getTypeElement(superClass);
        if (superElement != null) {
            fields.addAll(getFields(superElement));
        }
        for (Element enclosedElement : element.getEnclosedElements()) {
            if (enclosedElement.getKind().equals(ElementKind.FIELD) &&
                    !ProcessorUtil.hasModifier(enclosedElement, Modifier.STATIC)) {
                VariableElement variableElement = (VariableElement) enclosedElement;
                ScoreDataProperty annField = variableElement.getAnnotation(ScoreDataProperty.class);
                if (annField != null && annField.ignore()) {
                    continue;
                }
                fields.add(variableElement);
            }
        }
        return fields;
    }

    private int processMethod(
            TypeElement element,
            MethodSpec.Builder constructor,
            MethodSpec.Builder readMethod, MethodSpec.Builder writeMethod,
            int optionFieldIndex) {
        int fieldCnt = 0;
        TypeMirror superClass = element.getSuperclass();
        TypeElement superElement = super.getTypeElement(superClass);
        if (superElement != null) {
            fieldCnt += processMethod(superElement, constructor, readMethod, writeMethod, optionFieldIndex);
        }

        for (Element enclosedElement : element.getEnclosedElements()) {
            if (enclosedElement.getKind().equals(ElementKind.FIELD) &&
                    !ProcessorUtil.hasModifier(enclosedElement, Modifier.STATIC)) {
                VariableElement variableElement = (VariableElement) enclosedElement;
                ScoreDataProperty annField = variableElement.getAnnotation(ScoreDataProperty.class);
                if (annField != null && annField.ignore()) {
                    continue;
                }

                TypeMirror fieldType = variableElement.asType();
                String field = variableElement.getSimpleName().toString();
                String capitalized = field.substring(0, 1).toUpperCase() + field.substring(1);
                String getter = (fieldType.getKind() == TypeKind.BOOLEAN ? "is" : "get") + capitalized;
                String setter = "set" + capitalized;
                boolean nullable = true;
                boolean option = (++fieldCnt) > optionFieldIndex;

                boolean direct = false;
                if (annField != null) {
                    direct = annField.direct();
                    if (!annField.getter().isEmpty()) {
                        getter = annField.getter();
                    }
                    if (!annField.setter().isEmpty()) {
                        setter = annField.setter();
                    }
                    nullable = annField.nullable();
                }

                if (direct) {
                    constructor.addStatement("this.$L = $L.$L", field, PARAM_OBJECT, field);
                    getter = String.format("this.%s", field);
                    setter = String.format("%s.%s = $L", LOCAL_OBJECT, field);
                } else {
                    constructor.addStatement("this.$L($L.$L())", setter, PARAM_OBJECT, getter);
                    getter = String.format("this.%s()", getter);
                    setter = String.format("%s.%s($L)", LOCAL_OBJECT, setter);
                }

                if (option) {
                    readMethod.beginControlFlow("if ($L.hasNext())", PARAM_READER);
                }

                boolean isArray = fieldType.getKind() == TypeKind.ARRAY;
                if (super.containsDeclaredType(dbConstructors.keySet(), fieldType)) {
                    //TODO score db containers
                    List<? extends TypeMirror> types = ((DeclaredType) fieldType).getTypeArguments();
                    TypeMirror componentType = types.get(types.size() - 1);
                    String dbConstructor = super.getDeclaredType(dbConstructors, fieldType);
//                    writeMethod
//                            .addStatement("$T $L = $L", fieldType, field, getter)
//                            .addStatement("$L.$L($L)",
//                                    PARAM_WRITER, nullable ? "writeNullable" : "write", field);
                } else if (super.containsDeclaredType(mapTypes, fieldType)) {
                    List<? extends TypeMirror> types = ((DeclaredType) fieldType).getTypeArguments();
                    TypeMirror keyType = types.get(0);
                    TypeMirror valueType = types.get(1);

                    writeMethod.addStatement("$T $L = $L", fieldType, field, getter);
                    if (nullable) {
                        writeMethod
                                .beginControlFlow("if ($L != null)", field)
                                .addStatement("$L.beginNullableMap($L.size())", PARAM_WRITER, field);
                    } else {
                        writeMethod
                                .addStatement("$L.beginMap($L.size())", PARAM_WRITER, field);
                    }
                    writeMethod
                            .beginControlFlow("for($T<$T,$T> entry : $L.entrySet())",
                                Map.Entry.class, keyType, valueType, field)
                            .addCode(getWriteCodeBlock(keyType, annField, "entry.getKey()", "entry.getKey()", true))
                            .addCode(getWriteCodeBlock(valueType, annField, "entry.getValue()", "entry.getValue()", true))
                            .endControlFlow()
                            .addStatement("$L.end()", PARAM_WRITER)
                            .build();
                    if (nullable) {
                        writeMethod
                                .nextControlFlow("else")
                                .addStatement("$L.writeNull()", PARAM_WRITER)
                                .endControlFlow();
                    }
                    //TODO support map read
                } else if (!typeUtil.isSameType(fieldType, bytesType) &&
                        (isArray || super.containsDeclaredType(listTypes, fieldType))) {
                    //TODO support n-depth array, currently support byte[][] only
                    TypeMirror componentType = ProcessorUtil.getComponentType(fieldType);
                    int componentDepth = ProcessorUtil.getComponentTypeDepth(fieldType);
                    if (componentType.getKind() == TypeKind.BYTE) {
                        componentType = typeUtil.getArrayType(componentType);
                    }

                    writeMethod.addStatement("$T $L = $L", fieldType, field, getter);

                    if (nullable) {
                        writeMethod.beginControlFlow("if ($L != null)", field);
                        readMethod.beginControlFlow("if ($L.beginNullableList())", PARAM_READER);
                    } else {
                        readMethod.addStatement("$L.beginList()", PARAM_READER);
                    }
                    writeMethod
                            .addStatement("$L.$L($L.$L)",
                                    PARAM_WRITER, nullable ? "beginNullableList":"beginList", field, isArray ? "length" : "size()")
                            .beginControlFlow("for($T v : $L)", componentType, field)
                            .addCode(getWriteCodeBlock(componentType, annField, "v", "v", true))
                            .endControlFlow()
                            .addStatement("$L.end()", PARAM_WRITER)
                            .build();

                    readMethod.addCode("$T $L", fieldType, field);
                    String localList = field;
                    if (isArray) {
                        localList += "List";
                        readMethod.addStatement(" = null");
                        if (componentType.getKind().isPrimitive()) {
                            readMethod.addCode("$T $L", List.class, localList);
                        } else {
                            readMethod.addCode("$T<$T> $L", List.class, componentType, localList);
                        }
                    }
                    readMethod.addStatement(" = new $T<>()", scorex.util.ArrayList.class);
                    String setterOfList = localList + ".add($L)";
                    readMethod
                            .beginControlFlow("while($L.hasNext())", PARAM_READER)
                            .addCode(getReadCodeBlock(componentType, annField, field+"Element", setterOfList, true))
//                            .addStatement("$L.add($L)", localList, getReadStatement(componentType, annField))
                            .endControlFlow();
                    if (isArray) {
//                        if (componentType.getKind().isPrimitive()) {
                        boolean isBytesComponentType = typeUtil.isSameType(componentType, bytesType);
                        if (!isBytesComponentType && componentDepth > 1) {
                            throw new RuntimeException("not support "+fieldType);
                        }
                        TypeMirror constructComponentType = isBytesComponentType ? ProcessorUtil.getComponentType(fieldType) : componentType;

                        readMethod
                                .addStatement("$L = new $T[$L.size()]$L", field, constructComponentType, localList, "[]".repeat(componentDepth-1))
                                .beginControlFlow("for(int i=0; i<$L.size(); i++)", localList)
                                .addStatement("$L[i] = ($T)$L.get(i)", field, componentType, localList)
                                .endControlFlow();
//                        } else {
//                            readMethod.addStatement("$L = ($T)$L.toArray()", field, fieldType, localList);
//                        }
                    }

                    readMethod
                            .addStatement(setter, field)
                            .addStatement("$L.end()", PARAM_READER);
                    if (nullable) {
                        //currently beginNullableList is always write(nullity=false)
                        //so manually write nulllity
                        writeMethod
                                .nextControlFlow("else")
                                .addStatement("$L.writeNull()", PARAM_WRITER)
                                .endControlFlow();
                        readMethod.endControlFlow();
                    }
                } else {
                    readMethod.addCode(getReadCodeBlock(fieldType, annField, field, setter, false));
                    writeMethod.addCode(getWriteCodeBlock(fieldType, annField, field, getter, false));
                }
                if (option) {
                    readMethod.endControlFlow();
                }
            }
        }
        return fieldCnt;
    }
}
