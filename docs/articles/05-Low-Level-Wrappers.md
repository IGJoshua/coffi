# Low-Level Wrappers

### Unwrapped Native Handles
Some native libraries work with handles to large amounts of data at once, making
it undesirable to marshal data back and forth from Clojure, both because it's
not necessary to work with the data in Clojure directly, or also because of the
high (de)serialization costs associated with marshaling. In cases like these,
unwrapped native handles are desirable.

The functions `make-downcall` and `make-varargs-factory` are also provided to
create raw function handles.

```clojure
(def raw-strlen (ffi/make-downcall "strlen" [::mem/c-string] ::mem/long))
(raw-strlen (mem/serialize "hello" ::mem/c-string))
;; => 5
```

With raw handles, the argument types are expected to exactly match the types
expected by the native function. For primitive types, those are primitives. For
pointers, that is `MemorySegment`, and for composite types like structs and
unions, that is also `MemorySegment`. `MemorySegment` comes from the
`java.lang.foreign` package.

In addition, when a raw handle returns a composite type represented with a
`MemorySegment`, it requires an additional first argument, a `SegmentAllocator`,
which can be acquired with `arena-allocator` to get one associated with a
specific arena. The returned value will live until that arena is released.

In addition, function types can be specified as being raw, in the following
manner:

```clojure
[::ffi/fn [::mem/int] ::mem/int :raw-fn? true]
```

Clojure functions serialized to this type will have their arguments and return
value exactly match the types specified and will not perform any serialization
or deserialization at their boundaries.

One important caveat to consider when writing wrappers for performance-sensitive
functions is that the convenience macro `defcfn` that coffi provides will
already perform no serialization or deserialization on primitive arguments and
return types, so for functions with only primitive argument and return types
there is no performance reason to choose unwrapped native handles over the
convenience macro.

### Manual (De)Serialization
Coffi uses multimethods to dispatch to (de)serialization functions to enable
code that's generic over the types it operates on. However, in cases where you
know the exact types that you will be (de)serializing and the multimethod
dispatch overhead is too high a cost, it may be appropriate to manually handle
(de)serializing data. This will often be done paired with [Unwrapped Native
Handles](#unwrapped-native-handles).

Convenience functions are provided to both read and write all primitive types
and addresses, including byte order.

As an example, when wrapping a function that returns an array of big-endian
floats, the following code might be used.

``` clojure
;; int returns_float_array(float **arr)
(def ^:private returns-float-array* (ffi/make-downcall "returns_float_array" [::mem/pointer] ::mem/int))
;; void releases_float_array(float *arr)
(def ^:private release-floats* (ffi/make-downcall "releases_float_array" [::mem/pointer] ::mem/void))

(defn returns-float-array
  []
  (with-open [arena (mem/confined-arena)]
    ;; float *out_floats;
    ;; int num_floats = returns_float_array(&out_floats);
    (let [out-floats (mem/alloc mem/pointer-size arena)
          num-floats (returns-float-array* out-floats)
          floats-addr (mem/read-address out-floats)
          floats-slice (mem/reinterpret floats-addr (unchecked-multiply-int mem/float-size num-floats))]
      ;; Using a try/finally to perform an operation when the stack frame exits,
      ;; but not to try to catch anything.
      (try
        (loop [floats (transient [])
               index 0]
          (if (>= index num-floats)
            (persistent! floats)
            (recur (conj! floats (mem/read-float floats-slice
                                                 (unchecked-multiply-int index mem/float-size)
                                                 mem/big-endian))
                   (unchecked-inc-int index))))
        (finally
          (release-floats* floats-addr))))))
```

The above code manually performs all memory operations rather than relying on
coffi's dispatch. This will be more performant, but because multimethod overhead
is usually relatively low, it's recommended to use the multimethod variants for
convenience in colder functions.
