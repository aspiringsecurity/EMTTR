# ScoreJson

## JsonObjectProcessor

### Gradle
Add dependency to build.gradle
````
dependencies {
    compileOnly 'foundation.icon:javaee-api:0.9.0'
    
    annotationProcessor 'foundation.icon:javaee-score-json:0.9.0'
    implementation 'foundation.icon:javaee-score-json:0.9.0'
    implementation 'com.github.sink772:minimal-json:0.9.6'
    implementation 'foundation.icon:javaee-scorex:0.5.2'
}
````

### Usage
Annotate `@JsonObject` to class. also you can annotate `@JsonProperty` to field.
````java
@JsonObject
public class Xxx {
    private String value;
    
    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
}
````

When java compile, JSON convertable class will be generated which has `@JsonObject.suffix()`.  
Then you can use generated class as follows.
````java
import score.annotation.External;
import com.eclipsesource.json.JsonObject;

public class Score {
    @External(readonly = true)
    public String json(String jsonString) {
        // parse
        Xxx xxx = XxxJson.parse(jsonString);
        // toJsonObject
        JsonObject jsonObject = XxxJson.toJsonObject(xxx);
        // toJsonString 
        return jsonObject.toString();
    }
}
````
