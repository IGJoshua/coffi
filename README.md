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

### TODO Complex Wrappers

### TODO Serialization and Deserialization

### TODO Data Model

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
