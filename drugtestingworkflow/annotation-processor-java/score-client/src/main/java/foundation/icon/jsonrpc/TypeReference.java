package foundation.icon.jsonrpc;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * Copy from com.fasterxml.jackson.core.type.TypeReference
 */
public abstract class TypeReference<T> implements Comparable<TypeReference<T>> {
    protected final Type _type;

    protected TypeReference()
    {
        Type superClass = getClass().getGenericSuperclass();
        if (superClass instanceof Class<?>) { // sanity check, should never happen
            throw new IllegalArgumentException("Internal error: TypeReference constructed without actual type information");
        }
        _type = ((ParameterizedType) superClass).getActualTypeArguments()[0];
    }

    public Type getType() { return _type; }

    @Override
    public int compareTo(TypeReference<T> o) { return 0; }
}
