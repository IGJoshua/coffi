# Built-in Types **WIP**

### TODO Primitives {#primitives}

### TODO Arrays {#arrays}

### TODO Pointers {#pointers}

### TODO Structs {#structs}

### TODO Enums {#enums}

### TODO Flagsets {#flagsets}

### TODO Functions {#functions}

### Unions {#unions}
Unions in coffi are rather limited. They can be serialized, but not deserialized
without external information.

```clojure
[::mem/union
 #{::mem/float ::mem/double}
 :dispatch #(cond
             (float? %) ::mem/float
             (double? %) ::mem/double)]
```

This is a minimal union in coffi. If the `:dispatch` keyword argument is not
passed, then the union cannot be serialized, as coffi would not know which type
to serialize the values as. In [the example with a tagged
union](04-Custom-Types.md#tagged-union), a dispatch function was not provided
because the type was only used for the native layout.

In addition to a dispatch function, when serializing a union an extract function
may also be provided. In the case of the value in the tagged union from before,
it could be represented for serialization purposes like so:

```clojure
[::mem/union
 #{::mem/int ::mem/c-string}
 :dispatch #(case (first %)
              :ok ::mem/int
              :err ::mem/c-string)
 :extract second]
```

This union however would not include the tag when serialized.

If a union is deserialized, then all that coffi does is to allocate a new
segment of the appropriate size with an implicit arena so that it may later be
garbage collected, and copies the data from the source segment into it. It's up
to the user to call `deserialize-from` on that segment with the appropriate
type.

### TODO Raw Types {#raw-types}
