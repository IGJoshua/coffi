# Custom Types
Custom types with serializers and deserializers may be created. This is done
using two sets of three multimethods which can be extended by the user. For any
given type, only one set need be implemented.

Two examples of custom types are given here, one is a 3d vector, and the other
an example of a tagged union.

### Vector3
For the vector type, it will serialize to a pointer to an array of three floats.

The multimethod `primitive-type` returns the primitive type that a given type
serializes to. For this example, it should be a pointer.

```clojure
(defmethod mem/primitive-type ::vector
  [_type]
  ::mem/pointer)
```

For any type which doesn't serialize to a primitive, it returns nil, and
therefore need not be overriden.

Next is `serialize*` and `deserialize*`, multimethods that work with types that
serialize to primitives.

```clojure
(defmethod mem/serialize* ::vector
  [obj _type arena]
  (mem/serialize obj [::mem/array ::mem/float 3] arena))

(defmethod mem/deserialize* ::vector
  [segment _type]
  (mem/deserialize (mem/reinterpret segment (mem/size-of [::mem/array ::mem/float 3]))
                   [::mem/array ::mem/float 3]))
```

The `reinterpret` function allows you to take a segment and decorate it with a
new size, and possibly associate it with an arena or add cleanup functions on
it.

In cases like this where we don't know the arena of the pointer, we could use
`reinterpret` to ensure it's freed. For example if a `free-vector!` function
that takes a pointer exists, we could use this:

```clojure
(defcfn returns-vector
  "returns_vector" [] ::mem/pointer
  native-fn
  [arena]
  (let [ret-ptr (native-fn)]
    (-> (reinterpret ret-ptr (mem/size-of ::vector) arena free-vector!)
        (deserialize ::vector))))
```

This function takes an arena and returns the deserialized vector, and it will
free the pointer when the arena closes.

### Tagged Union
For the tagged union type, we will represent the value as a vector of a keyword
naming the tag and the value. The type itself will need to take arguments,
similar to `struct`. For example, if we were to represent a result type like in
Rust, we might have the following values:

```clojure
[:ok 5]
[:err "Invalid number format"]
```

To represent this, we can have a `tagged-union` type. For this instance of the
result type, it may look like this:

```clojure
[::tagged-union [:ok :err] {:ok ::mem/int :err ::mem/c-string}]
```

The native representation of these objects is a struct of the tag and a union of
the value. In order to correctly serialize the data and pass it to native code,
we need a representation of the native layout of the data. The `c-layout`
multimethod provides that.

```clojure
(defmethod mem/c-layout ::tagged-union
  [[_tagged-union tags type-map]]
  (mem/c-layout [::mem/struct
                 [[:tag ::mem/long]
                  [:value [::mem/union (vals type-map)]]]]))
```

Types with type arguments are represented as vectors of the type name and any
additional arguments. The type name is what is dispatched on for the
multimethods.

Now that we have a native layout, we need to be able to serialize and
deserialize the value into and out of memory segments. This is accomplished with
`serialize-into` and `deserialize-from`.

```clojure
(defn item-index
  "Gets the index of the first occurance of `item` in `coll`."
  [coll item]
  (first
   (->> coll
        (map-indexed vector)
        (filter (comp #{item} second))
        (map first))))

(defmethod mem/serialize-into ::tagged-union
  [obj [_tagged-union tags type-map] segment arena]
  (mem/serialize-into
   {:tag (item-index tags (first obj))
    :value (second obj)}
   [::mem/struct
    [[:tag ::mem/long]
     [:value (get type-map (first obj))]]]
   segment
   arena))
```

This serialization method is rather simple, it just turns the vector value into
a map, and serializes it as a struct, choosing the type of the value based on
the tag.

```clojure
(defmethod mem/deserialize-from ::tagged-union
  [segment [_tagged-union tags type-map]]
  (let [tag (mem/deserialize-from segment ::mem/long)]
    [(nth tags tag)
     (mem/deserialize-from
      (mem/slice segment (mem/size-of ::mem/long))
      (get type-map tag))]))
```

Deserialization is a little more complex. First the tag is retrieved from the
beginning of the segment, and then the type of the value is decided based on
that before it is deserialized.
