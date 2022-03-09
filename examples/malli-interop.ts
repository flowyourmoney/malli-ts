import { k, toClj, sym, validate, toSha256 } from "./malli-ts.ts_gen-types";

console.log("-- toClj({a: 1, b: \"hello\"}) --")
console.log(toClj({a: 1, b: "hello"}))

console.log("-- VALIDATION TEST --")

const schema = toClj([
    k("map"),
    [k("name"), sym("string?")],
    [k("age"), sym("int?")],
    [k("passwordHash"), sym("string")]
])

type Person = {
    name: string;
    age: number;
    passwordHash: string
}

const tiago: Person = {
    name: "Tiago", 
    age: 21, 
    passwordHash: toSha256("verystrongpassword").digest('hex') }

const val = toClj(tiago)
console.log("schema: ", schema)
console.log("val: ", val.obj)
console.log(validate(schema, val)) // true