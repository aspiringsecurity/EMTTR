# ScoreData

## ScoreDataObjectProcessor

### Gradle
Add dependency to build.gradle
````
dependencies {
    compileOnly 'foundation.icon:javaee-api:0.9.0'
    implementation 'foundation.icon:javaee-scorex:0.5.2'
    
    compileOnly 'foundation.icon:javaee-score-data:0.9.0'
    annotationProcessor 'foundation.icon:javaee-score-data:0.9.0'
}
````

### Usage
Annotate `@ScoreDataObject` to class. also you can annotate `@ScoreDataProperty` to field.
````java
@ScoreDataObject
public class Xxx {
    private String value;
    
    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
}
````

When java compile, serializable class will be generated which has `@ScoreDataObject.suffix()`.
Then you can use generated class as follows.
````java
import score.annotation.External;

public class Score {
    final VarDB<XxxSdo> db = Context.newVarDB("db", XxxSdo.class);
    
    @External
    public void set(Xxx xxx) {
        db.set(new XxxSdo(xxx));
    }

    @External(readonly = true)
    public Xxx get() {
        return db.get();
    }
}
````
