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
This library is available on Clojars. Add one of the following entries to the
`:deps` key of your `deps.edn`:

```clojure
org.suskalo/coffi {:mvn/version "0.1.169"}
io.github.IGJoshua/coffi {:git/tag "v0.1.169" :git/sha "7ec2748"}
```

If you use this library as a git dependency, you will need to prepare the
library.

```sh
$ clj -X:deps prep
```

Coffi also includes support for the linter clj-kondo. If you use clj-kondo and
this library's macros are not linting correctly, you may need to install the
config bundled with the library. You can do so with the following shell command:

```sh
$ clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"
```

And then adding `"org.suskalo/coffi"` to the `:config-paths` key in your
`.clj-kondo/config.edn` file.

## Usage
There are two major components to coffi and interacting with native code:
manipulating off-heap memory, and loading native code for use with Clojure.

In the simplest cases, the native functions you call will work exclusively with
built-in types, for example the function `strlen` from libc.

```clojure
(require '[coffi.mem :as mem :refer [defalias]])
(require '[coffi.ffi :as ffi :refer [defcfn]])

(defcfn strlen
  "Given a string, measures its length in bytes."
  strlen [::mem/c-string] ::mem/long)

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
((ffi/cfn "strlen" [::mem/c-string] ::mem/long) "hello")
;; => 5
```

If you want to use functions from libraries other than libc, then you'll need to
load them. Two functions are provided for this, `load-system-library`, and
`load-library`. `load-system-library` takes a string which represents the name
of a library that should be loaded via system lookup.

```clojure
(ffi/load-system-library "z")
```

This will load libz from the appropriate place on the user's load path.

Alternatively, `load-library` takes a file path to a dynamically loaded library.

```clojure
(ffi/load-library "lib/libz.so")
```

This will load libz from the lib subdirectory of the current working directory.
As you can see this requires the entire filename, including platform-specific
file extensions.

If a library is attempted to be loaded but doesn't exist or otherwise can't be
loaded, an exception is thrown. This can be convenient as any namespace with a
`load-library` call at the top level cannot be required without the library
being able to be loaded.

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
  "zero" [] [::mem/struct [[:x ::mem/float] [:y ::mem/float]]])

(zero-point)
;; => {:x 0.0,
;;     :y 0.0}
```

Writing out struct definitions like this every time would get tedious, so the
macro `defalias` is used to define a struct alias.

```clojure
(defalias ::point
  [::mem/struct
   [[:x ::mem/float]
    [:y ::mem/float]]])

(defcfn zero-point
  "zero" [] ::point)
```

In cases where a pointer to some data is required to pass as an argument to a
native function, but dosn't need to be read back in, the `pointer` primitive
type can take a type argument.

```clojure
[::mem/pointer ::mem/int]
```

Arrays are also supported via a type argument. Keep in mind that they are the
array itself, and not a pointer to the array like you might see in certain cases
in C.

```clojure
[::mem/array ::mem/int 3]
```

### Callbacks
In addition to these composite types, there is also support for Clojure
functions.

```clojure
[::ffi/fn [::mem/c-string] ::mem/int]
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
(def printf-factory (ffi/vacfn-factory "printf" [::mem/c-string] ::mem/int))
```

This returns a function of the types of the rest of the arguments which itself
returns a native function wrapper.

```clojure
(def print-int (printf-factory ::mem/int))

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
(def some-const (ffi/const "some_const" ::mem/int))
```

This value is fetched once when you call `const` and is turned into a Clojure
value. If you need to refer to a global variable, then `static-variable` can be
used to create a reference to the native value.

```clojure
(def some-var (ffi/static-variable "some_var" ::mem/int))
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
  "takes_array_with_count" [::mem/pointer ::mem/long] ::mem/void
  native-fn
  [ints]
  (let [arr-len (count ints)
        int-array (serialize ints [::mem/array ::mem/int arr-len]
    (native-fn (mem/address-of int-array) arr-len))]))
```

The symbol `native-fn` can be any unqualified symbol, and names the native
function being wrapped. It must be called in the function body below if you want
to call the native code.

This `serialize` function has a paired `deserialize`, and allows marshaling
Clojure data back and forth to native data structures.

This can be used to implement out variables often seen in native code.

```clojure
(defcfn out-int
  "out_int" [::mem/pointer] ::mem/void
  native-fn
  [i]
  (let [int-ptr (serialize i [::mem/pointer ::mem/int])]
    (native-fn int-ptr)
    (deserialize int-ptr [::mem/pointer ::mem/int])))
```

### Scopes
In order to serialize any non-primitive type (such as the previous
`[::mem/pointer ::mem/int]`), off-heap memory needs to be allocated. When memory
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
  "out_int" [::mem/pointer] ::mem/void
  native-fn
  [i]
  (with-open [scope (mem/stack-scope)]
    (let [int-ptr (mem/serialize i [::mem/pointer ::mem/int] scope)]
      (native-fn int-ptr)
      (mem/deserialize int-ptr [::mem/pointer ::mem/int]))))
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
  [obj _type scope]
  (mem/address-of (mem/serialize obj [::mem/array ::mem/float 3] scope)))

(defmethod mem/deserialize* ::vector
  [addr _type]
  (mem/deserialize (mem/slice-global addr (mem/size-of [::mem/array ::mem/float 3]))
                   [::mem/array ::mem/float 3]))
```

The `slice-global` function allows you to take an address without an associated
scope and get a memory segment which can be deserialized.

In cases like this where we don't know the scope of the pointer, we could use
`add-close-action!` to ensure it's freed. For example if a `free-vector!`
function that takes a pointer exists, we could use this:

```clojure
(defcfn returns-vector
  "returns_vector" [] ::mem/pointer
  native-fn
  [scope]
  (let [ret-ptr (native-fn)]
    (add-close-action! scope #(free-vector! ret-ptr))
    (deserialize ret-ptr ::vector)))
```

This function takes a scope and returns the deserialized vector, and it will
free the pointer when the scope closes.

#### Tagged Union
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
  [obj [_tagged-union tags type-map] segment scope]
  (mem/serialize-into
   {:tag (item-index tags (first obj))
    :value (second obj)}
   [::mem/struct
    [[:tag ::mem/long]
     [:value (get type-map (first obj))]]]
   segment
   scope))
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

### Unions
In the last section the custom serialization and deserialization of a tagged
union used a union from coffi in order to define the native layout, but not for
actual serialization or deserialization. This is intentional. A union in coffi
is rather limited. It can be serialized, but not deserialized without external
information.

```clojure
[::mem/union
 #{::mem/float ::mem/double}
 :dispatch #(cond
             (float? %) ::mem/float
             (double? %) ::mem/double)]
```

This is a minimal union in coffi. If the `:dispatch` keyword argument is not
passed, then the union cannot be serialized, as coffi would not know which type
to serialize the values as. In the example with a tagged union, a dispatch
function was not provided because the type was only used for the native layout.

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
(def raw-strlen (ffi/make-downcall "strlen" [::mem/c-string] ::mem/long))
(raw-strlen (mem/serialize "hello" ::mem/c-string))
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
[::ffi/fn [::mem/int] ::mem/int :raw-fn? true]
```

Clojure functions serialized to this type will have their arguments and return
value exactly match the types specified and will not perform any serialization
or deserialization at their boundaries.

### Data Model
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

## Known Issues
The project author is aware of these issues and plans to fix them in a future
release:

There are currently no known issues! Hooray!

## Future Plans
These features are planned for future releases.

- Support for va_args type
- Functions for wrapping structs in padding following various standards
- Header parsing tool for generating a data model?
- Generic type aliases

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
