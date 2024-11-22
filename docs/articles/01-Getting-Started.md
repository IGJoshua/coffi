# Getting Started

## Installation {#installation}
This library is available on Clojars. Add one of the following entries to the
`:deps` key of your `deps.edn`:

```clojure
org.suskalo/coffi {:mvn/version "x.y.z"}
io.github.IGJoshua/coffi {:git/tag "x.y.z" :git/sha "abcdef0"}
```

See GitHub for the [latest releases](https://github.com/IGJoshua/coffi/releases).

If you use this library as a git dependency, you will need to prepare the
library.

```sh
$ clj -X:deps prep
```

Coffi requires usage of the package `java.lang.foreign`, and most of the
operations are considered unsafe by the JDK, and are therefore unavailable to
your code without passing some command line flags. In order to use coffi, add
the following JVM arguments to your application.

```sh
--enable-native-access=ALL-UNNAMED
```

You can specify JVM arguments in a particular invocation of the Clojure CLI with
the -J flag like so:

``` sh
clj -J--enable-native-access=ALL-UNNAMED
```

You can also specify them in an alias in your `deps.edn` file under the
`:jvm-opts` key (see the next example) and then invoking the CLI with that alias
using `-M`, `-A`, or `-X`.

``` clojure
{:aliases {:dev {:jvm-opts ["--enable-native-access=ALL-UNNAMED"]}}}
```

Other build tools should provide similar functionality if you check their
documentation.

When creating an executable jar file, you can avoid the need to pass this
argument by adding the manifest attribute `Enable-Native-Access: ALL-UNNAMED` to
your jar.

## Basic Usage {#usage}
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

### Primitive Types {#primitive-types}
Coffi defines a basic set of primitive types:

- byte
- short
- int
- long
- char
- float
- double
- pointer

Each of these types maps to their C counterpart. Values of any of these
primitive types except for `pointer` will be cast with their corresponding
Clojure function when they are passed as arguments to native functions.
Additionally, the `c-string` type is defined, although it is not primitive.

### Composite Types {#composite-types}
In addition, some composite types are also defined in coffi, including struct
and union types (unions will be discussed with serialization and
deserialization). For an example C struct and function:

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

Struct definitions do not include any padding by default. Functions for
transforming struct types to include padding conforming to various standards can
be found in `coffi.layout`.

``` clojure
(require '[coffi.layout :as layout])

(defalias ::needs-padding
  (layout/with-c-layout
   [::mem/struct
    [[:a ::mem/char]
     [:x ::mem/float]]]))

(mem/size-of ::needs-padding)
;; => 8

(mem/align-of ::needs-padding)
;; => 4
```

Values deserialized with types produced from layout functions may include an
extra `:coffi.layout/padding` key with a nil value.

A limitation of the `defcfn` macro in its current form is that types provided to
it must be provided in a literal form, not as an expression that evaluates to a
type. This means that if you wish to use a layout function on a struct you must
define an alias for it before the type can be used as a type in `defcfn`.

In cases where a pointer to some data is required to pass as an argument to a
native function, but doesn't need to be read back in, the `pointer` primitive
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

### Callbacks {#callbacks}
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

When writing a wrapper library for a C library, it may be a good choice to wrap
all passed Clojure functions in an additional function which catches all
throwables, potentially notifies the user in some manner (e.g. logging), and
returns a default value. This is on the wrapper library's developer to decide
when and where this is appropriate, as in some cases no reasonable default
return value can be determined and it is most sensible to simply crash the JVM.
This is the reason that coffi defaults to this behavior, as in the author's
opinion it is better to fail hard and fast rather than to attempt to produce a
default and cause unexpected behavior later.

Another important thing to keep in mind is the expected lifetime of the function
that you pass to native code. For example it is perfectly fine to pass an
anonymous function to a native function if the callback will never be called
again once the native function returns. If however it saves the callback for
later use the JVM may collect it prematurely, causing a crash when the callback
is later called by native code.

### Variadic Functions {#variadic-functions}
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

### Global Variables {#globals}
Some libraries include global variables or constants accessible through symbols.
To start with, constant values stored in symbols can be fetched with `const`, or
the parallel macro `defconst`

```clojure
(def some-const (ffi/const "some_const" ::mem/int))
(ffi/defconst some-const "some_const" ::mem/int)
```

This value is fetched once when you call `const` and is turned into a Clojure
value. If you need to refer to a global variable, then `static-variable` (or
parallel `defvar`) can be used to create a reference to the native value.

```clojure
(def some-var (ffi/static-variable "some_var" ::mem/int))
(ffi/defvar some-var "some_var" ::mem/int)
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

The memory that backs the static variable can be fetched with the function
`static-variable-segment`, which can be used to pass a pointer to the static
variable to native functions that require it.

### Complex Wrappers {#complex-wrappers}
Some functions require more complex code to map nicely to a Clojure function.
The `defcfn` macro provides facilities to wrap the native function with some
Clojure code to make this easier.

```clojure
(defcfn takes-array
  "takes_array_with_count" [::mem/pointer ::mem/long] ::mem/void
  native-fn
  [ints]
  (let [arr-len (count ints)
        int-array (mem/serialize ints [::mem/array ::mem/int arr-len])]
    (native-fn int-array arr-len)))
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
  (let [int-ptr (mem/serialize i [::mem/pointer ::mem/int])]
    (native-fn int-ptr)
    (mem/deserialize int-ptr [::mem/pointer ::mem/int])))
```
