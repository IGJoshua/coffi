# coffi
[![cljdoc badge](https://cljdoc.org/badge/org.suskalo/coffi)](https://cljdoc.org/d/org.suskalo/coffi/CURRENT)
[![Clojars Project](https://img.shields.io/clojars/v/org.suskalo/coffi.svg)](https://clojars.org/org.suskalo/coffi)

Coffi is a foreign function interface library for Clojure, using the new
[Project Panama](https://openjdk.java.net/projects/panama/) that's a part of the
incubator in Java 17. This allows calling native code directly from Clojure
without the need for either Java or native code specific to the library, as e.g.
the JNI does. Coffi focuses on ease of use, including functions and macros for
creating wrappers to allow the resulting native functions to act just like
Clojure ones, however this doesn't remove the ability to write systems which
minimize the cost of marshaling data and optimize for performance, to make use
of the low-level access Panama gives us.

## Installation
This library is available on Clojars. Add the following entry to the `:deps` key
of your `deps.edn`:

```clojure
org.suskalo/coffi {:mvn/version "0.1.0"}
```

## Usage
There are two major components to coffi and interacting with native code:
manipulating off-heap memory, and loading native code for use with Clojure.

In the simplest cases, the native functions you call will work exclusively with
built-in types, for example the function `strlen` from libc.

```clojure
(require '[coffi.ffi :as ffi :refer [defcfn defalias]])

(defcfn strlen
  "Given a string, measures its length in bytes."
  strlen [::ffi/c-string] ::ffi/long)

(strlen "hello")
;; => 5
```

The first argument to `defcfn` is the name of the Clojure var that will hold the
native function reference, followed by an optional docstring and attribute map,
then the C function identifier, including the name of the native symbol, a
vector of argument types, and the return type.

If you wish to use a native function as an anonymous function, it can be done
with the `cfn` function.

```clojure
((ffi/cfn "strlen" [::ffi/c-string] ::ffi/long) "hello")
;; => 5
```

### Primitive Types
Coffi defines a basic set of primitive types:
- byte
- short
- int
- long
- long-long
- char
- float
- double
- pointer

Each of these types maps to their C counterpart. Values of any of these
primitive types except for `pointer` will be cast with their corresponding
Clojure function (with `long-long` mapping to the `long` function) when they are
passed as arguments to native functions. Additionally, the `c-string` type is
defined, although it is not primitive.

### Composite Types
In addition, some composite types are also defined in coffi, including struct
and union types (unions will be discussed with serialization and
deserialization). For an example c struct and function:

```c
typedef struct point {
    float x;
    float y;
} Point;

Point zero(void) {
    Point res = {};

    res.x = 0.0;
    res.y = 0.0;

    return res;
}
```

The corresponding coffi definition is like so:

```clojure
(defcfn zero-point
  "zero" [] [::ffi/struct [[:x ::ffi/float] [:y ::ffi/float]]])

(zero-point)
;; => {:x 0.0,
;;     :y 0.0}
```

Writing out struct definitions like this every time would get tedious, so the
macro `defalias` is used to define a struct alias.

```clojure
(defalias ::point
  [::ffi/struct
   [[:x ::ffi/float]
    [:y ::ffi/float]]])

(defcfn zero-point
  "zero" [] ::point)
```

In cases where a pointer to some data is required to pass as an argument to a
native function, but dosn't need to be read back in, the `pointer` primitive
type can take a type argument.

```clojure
[::ffi/pointer ::ffi/int]
```

Arrays are also supported via a type argument. Keep in mind that they are the
array itself, and not a pointer to the array like you might see in certain cases
in C.

```clojure
[::ffi/array ::ffi/int 3]
```

### Callbacks
In addition to these composite types, there is also support for Clojure
functions.

```clojure
[::ffi/fn [::ffi/c-string] ::ffi/int]
```

Be aware though that if an exception is thrown out of a callback that is called
from C, the JVM will crash. The resulting crash log should include the exception
type and message in the registers section, but it's important to be aware of all
the same. Ideally you should test your callbacks before actually passing them to
native code.

### Variadic Functions
Some native functions can take any number of arguments, and in these cases coffi
provides `vacfn-factory` (for "varargs C function factory").

```clojure
(def printf-factory (ffi/vacfn-factory "printf" [::ffi/c-string] ::ffi/int))
```

This returns a function of the types of the rest of the arguments which itself
returns a native function wrapper.

```clojure
(def print-int (printf-factory ::ffi/int))

(print-int "Some integer: %d\n" 5)
;; Some integer: 5
```

At the moment there is no equivalent to `defcfn` for varargs functions.

Some native functions that are variadic use the type `va_list` to make it easier
for other languages to call them in their FFI. At the time of writing, coffi
does not support va-list, however it is a planned feature.

### Global Variables
Some libraries include global variables or constants accessible through symbols.
To start with, constant values stored in symbols can be fetched with `const`

```clojure
(def some-const (ffi/const "some_const" ::ffi/int))
```

This value is fetched once when you call `const` and is turned into a Clojure
value. If you need to refer to a global variable, then `static-variable` can be
used to create a reference to the native value.

```clojure
(def some-var (ffi/static-variable "some_var" ::ffi/int))
```

This variable is an `IDeref`. Each time you dereference it, the value will be
deserialized from the native memory and returned. Additional functions are
provided for mutating the variable.

```clojure
(ffi/freset! some-var 5)
;; => 5
@some-var
;; => 5
```

Be aware however that there is no synchronization on these types. The value
being read is not read atomically, so you may see an inconsistent state if the
value is being mutated on another thread.

A parallel function `fswap!` is also provided, but it does not provide any
atomic semantics either.

### Complex Wrappers
Some functions require more complex code to map nicely to a Clojure function.
The `defcfn` macro provides facilities to wrap the native function with some
Clojure code to make this easier.

```clojure
(defcfn takes-array
  "takes_array_with_count" [::ffi/pointer ::ffi/long] ::ffi/void
  native-fn
  [ints]
  (let [arr-len (count ints)
        int-array (serialize ints [::ffi/array ::ffi/int arr-len]
    (native-fn (ffi/address-of int-array) arr-len))]))
```

The symbol `native-fn` can be any unqualified symbol, and names the native
function being wrapped. It must be called in the function body below if you want
to call the native code.

This `serialize` function has a paired `deserialize`, and allows marshaling
Clojure data back and forth to native data structures.

This can be used to implement out variables often seen in native code.

```clojure
(defcfn out-int
  "out_int" [::ffi/pointer] ::ffi/void
  native-fn
  [i]
  (let [int-ptr (serialize i [::ffi/pointer ::ffi/int])]
    (native-fn int-ptr)
    (deserialize int-ptr [::ffi/pointer ::ffi/int])))
```

### Scopes
In order to serialize any non-primitive type (such as the previous
`[::ffi/pointer ::ffi/int]`), off-heap memory needs to be allocated. When memory
is allocated inside the JVM, the memory is associated with a scope. Because none
was provided here, the scope is an implicit scope, and the memory will be freed
when the serialized object is garbage collected.

In many cases this is not desirable, because the memory is not freed in a
deterministic manner, causing garbage collection pauses to become longer, as
well as changing allocation performance. Instead of an implicit scope, there are
other kinds of scopes as well. A `stack-scope` is a thread-local scope. Stack
scopes are `Closeable`, which means they should usually be used in a `with-open`
form. When a `stack-scope` is closed, it immediately frees all the memory
associated with it. The previous example, `out-int`, can be implemented with a
stack scope.

```clojure
(defcfn out-int
  "out_int" [::ffi/pointer] ::ffi/void
  native-fn
  [i]
  (with-open [scope (ffi/stack-scope)]
    (let [int-ptr (ffi/serialize i [::ffi/pointer ::ffi/int] scope)]
      (native-fn int-ptr)
      (ffi/deserialize int-ptr [::ffi/pointer ::ffi/int]))))
```

This will free the pointer immediately upon leaving the function.

When memory needs to be accessible from multiple threads, there's
`shared-scope`. When using a `shared-scope`, it should be accessed inside a
`with-acquired` block. When a `shared-scope` is `.close`d, it will release all
its associated memory when every `with-acquired` block associated with it is
exited.

In addition, two non-`Closeable` scopes are `global-scope`, which never frees
the resources associated with it, and `connected-scope`, which is a scope that
frees its resources on garbage collection, like an implicit scope.

### Serialization and Deserialization
Custom serializers and deserializers may also be created. This is done using two
sets of three multimethods which can be extended by the user. For any given
type, only one set need be implemented.

Two examples of custom types are given here, one is a 3d vector, and the other
an example of a tagged union.

#### Vector3
For the vector type, it will serialize to a pointer to an array of three floats.

The multimethod `primitive-type` returns the primitive type that a given type
serializes to. For this example, it should be a pointer.

```clojure
(defmethod ffi/primitive-type ::vector
  [_type]
  ::ffi/pointer)
```

For any type which doesn't serialize to a primitive, it returns nil, and
therefore need not be overriden.

Next is `serialize*` and `deserialize*`, multimethods that work with types that
serialize to primitives.

```clojure
(defmethod ffi/serialize* ::vector
  [obj _type scope]
  (ffi/address-of (ffi/serialize obj [::ffi/array ::ffi/float 3] scope)))

(defmethod ffi/deserialize* ::vector
  [addr _type]
  (ffi/deserialize (ffi/slice-global addr (ffi/size-of [::ffi/array ::ffi/float 3]))
                   [::ffi/array ::ffi/float 3]))
```

The `slice-global` function allows you to take an address without an associated
scope and get a memory segment which can be deserialized.

In cases like this where we don't know the scope of the pointer, we could use
`add-close-action!` to ensure it's freed. For example if a `free-vector!`
function that takes a pointer exists, we could use this:

```clojure
(defcfn returns-vector
  "returns_vector" [] ::ffi/pointer
  native-fn
  [scope]
  (let [ret-ptr (native-fn)]
    (add-close-action! scope #(free-vector! ret-ptr))
    (deserialize ret-ptr ::vector)))
```

This function takes a scope and returns the deserialized vector, and it will
free the pointer when the scope closes.

#### TODO Tagged Union
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
[::tagged-union [:ok :err] {:ok ::ffi/int :err ::ffi/c-string}]
```

The native representation of these objects is a struct of the tag and a union of
the value. In order to correctly serialize the data and pass it to native code,
we need a representation of the native layout of the data. The `c-layout`
multimethod provides that.

```clojure
(defmethod ffi/c-layout ::tagged-union
  [[_tagged-union tags type-map]]
  (ffi/c-layout [::ffi/struct
                 [[:tag ::ffi/long]
                  [:value [::ffi/union (vals type-map)]]]]))
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

(defmethod ffi/serialize-into ::tagged-union
  [obj [_tagged-union tags type-map] segment scope]
  (ffi/serialize-into
   {:tag (item-index tags (first obj))
    :value (second obj)}
   [::ffi/struct
    [[:tag ::ffi/long]
     [:value (get type-map (first obj))]]]
   segment
   scope))
```

This serialization method is rather simple, it just turns the vector value into
a map, and serializes it as a struct, choosing the type of the value based on
the tag.

```clojure
(defmethod ffi/deserialize-from ::tagged-union
  [segment [_tagged-union tags type-map]]
  (let [tag (ffi/deserialize-from segment ::ffi/long)]
    [(nth tags tag)
     (ffi/deserialize-from
      (ffi/slice segment (ffi/size-of ::ffi/long))
      (get type-map tag))]))
```

Deserialization is a little more complex. First the tag is retrieved from the
beginning of the segment, and then the type of the value is decided based on
that before it is deserialized.

### Unions
In the last section the custom serialization and deserialization of a tagged
union used a union from coffi in order to define the native layout, but not for
actual serialization or deserialization. This is intentional. A union in coffi
is rather limited. It can be serialized, but not deserialized without external
information.

```clojure
[::ffi/union
 #{::ffi/float ::ffi/double}
 :dispatch #(cond
             (float? %) ::ffi/float
             (double? %) ::ffi/double)]
```

This is a minimal union in coffi. If the `:dispatch` keyword argument is not
passed, then the union cannot be serialized, as coffi would not know which type
to serialize the values as. In the example with a tagged union, a dispatch
function was not provided because the type was only used for the native layout.

In addition to a dispatch function, when serializing a union an extract function
may also be provided. In the case of the value in the tagged union from before,
it could be represented for serialization purposes like so:

```clojure
[::ffi/union
 #{::ffi/int ::ffi/c-string}
 :dispatch #(case (first %)
              :ok ::ffi/int
              :err ::ffi/c-string)
 :extract second]
```

This union however would not include the tag when serialized.

If a union is deserialized, then all that coffi does is to allocate a new
segment of the appropriate size with an implicit scope so that it may later be
garbage collected, and copies the data from the source segment into it. It's up
to the user to call `deserialize-from` on that segment with the appropriate
type.

### Unwrapped Native Handles
Sometimes the overhead brought by the automatic serialization and
deserialization from the methods explained so far is too much. In cases like
these, unwrapped native handles are desirable.

The functions `make-downcall` and `make-varargs-factory` are provided to create
these raw handles.

```clojure
(def raw-strlen (ffi/make-downcall "strlen" [::ffi/c-string] ::ffi/long))
(raw-strlen (ffi/serialize "hello" ::ffi/c-string))
;; => 5
```

In these cases, the argument types are expected to exactly match the types
expected by the native function. For primitive types, those are primitives. For
addresses, that is `MemoryAddress`, and for composite types like structs and
unions, that is `MemorySegment`. Both `MemoryAddress` and `MemorySegment` come
from the `jdk.incubator.foreign` package.

In addition, when a raw handle returns a composite type represented with a
`MemorySegment`, it requires an additional first argument, a `SegmentAllocator`,
which can be acquired with `scope-allocator` to get one associated with a
specific scope. The returned value will live until that scope is released.

In addition, function types can be specified as being raw, in the following
manner:

```clojure
[::ffi/fn [::ffi/int] ::ffi/int :raw-fn? true]
```

Clojure functions serialized to this type will have their arguments and return
value exactly match the types specified and will not perform any serialization
or deserialization at their boundaries.

### TODO Data Model

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
