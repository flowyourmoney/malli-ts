import { k, toClj, sym, validate } from "malli-ts.ts.js";

console.log("-- toClj({a: 1, b: \"hello\"}) --")
console.log(toClj({a: 1, b: "hello"}))

console.log("-- VALIDATION TEST --")

const schema = toClj([
    k("map"),
    [k("name"), sym("string?")],
    [k("age"), sym("int?")],
])

const val = toClj({name: "Tiago", age: 21})
console.log("schema: ", schema)
console.log("val: ", val.obj)
console.log(validate(schema, val)) // true