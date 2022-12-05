var f = new Function('name', 'console.log("hello, " + name + "!");');
f('saru');
let testObject = {
    name: "Saru",
    age: 19
}
var testFunc = new Function('funcName','function scInteraction(){console.log(funcName.name +  funcName.age);}scInteraction();');
testFunc(testObject);