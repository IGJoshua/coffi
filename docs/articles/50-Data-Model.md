# Data Model
In addition to the macros and functions provided to build a Clojure API for
native libraries, facilities are provided for taking data and loading all the
symbols specified by it. This can be useful if a library provides (or an
external provider maintains) a data representation of their API, as Clojure data
to represent it may be programmatically generated from these sources.

The data to represent an API is a map with the following form:

```clojure
(def strlen-libspec
  {:strlen {:type :function
            :symbol "strlen"
            :function/args [::mem/c-string]
            :function/ret ::mem/long}})
```

Each key in this map represents a single symbol to be loaded. The value is a map
with at least the keys `:type` and `:symbol`. These are the currently recognized
types:

- function
- varargs-factory
- const
- static-var

Each one has its own set of additional keys which can be added to the map. Both
`function` and `varargs-factory` have the three keys `:function/args`,
`:function/ret`, and `:function/raw-fn?`. The `const` type has `:const/type` and
`static-var` has `:static-var/type`.

This data can be passed to the function `reify-libspec`, which will take the
data and return a map from the same keys as the input map to whatever value is
appropriate for a given symbol type (e.g. a Clojure function for `function`, a
value for `const`, etc.).

```clojure
(ffi/reify-libspec strlen-libspec)
;; => {:strlen #function[...]}
```

This functionality can be extended by specifying new types as implementations of
the multimethod `reify-symbolspec`, although it's recommended that for any
library authors who do so, namespaced keywords be used to name types.
