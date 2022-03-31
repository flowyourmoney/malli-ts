# malli-ts

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.flowyourmoney/malli-ts.svg)](https://clojars.org/org.clojars.tiagodalloca/malli-ts)

A library for generating TS type definitions from malli schemas

## FAQ

### >I would like to know why (mt/external-type … has 2 "crypto" argument

```clojure
(defn external-type
  ([type-name type-path type-import-alias]
   [any? {::external-type {:t-name type-name
                           :t-path type-path
                           :t-alias type-import-alias}}])
  ([type-name type-path]
   (external-type type-name type-path nil))
  ([type-name]
   (external-type type-name nil nil)))

(external-type "Hash" "crypto" "crypto")
```

One of them is the type path such as require('type-path') corresponds to the type's module, and then that we can refer to it as import * as type_import_alias from 'type-path' . In this specific example, both were "crypto" coincidently. 
