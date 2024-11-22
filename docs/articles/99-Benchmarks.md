# Benchmarks **OUTDATED**
**BENCHMARKS FOR COFFI AND DTYPE-NEXT ARE BASED ON AN OLD VERSION. NEW BENCHMARKS WILL BE CREATED SOON.**

An additional consideration when thinking about alternatives is the performance
of each available option. It's an established fact that JNA (used by all three
alternative libraries on JDK <16) introduces more overhead when calling native
code than JNI does.

In order to provide a benchmark to see how much of a difference the different
native interfaces make, we can use
[criterium](https://github.com/hugoduncan/criterium) to benchmark each.
[GLFW](https://www.glfw.org)'s
[`glfwGetTime`](https://www.glfw.org/docs/latest/group__input.html#gaa6cf4e7a77158a3b8fd00328b1720a4a)
function will be used for the test as it performs a simple operation, and is
conveniently already wrapped in JNI by the excellent
[LWJGL](https://www.lwjgl.org/) library.

The following benchmarks were run on a Lenovo Thinkpad with an Intel i7-10610U
running Manjaro Linux, using Clojure 1.10.3 on Java 17.

### JNI
The baseline for performance is the JNI. Using LWJGL it's relatively simple to
benchmark. The following Clojure CLI command will start a repl with LWJGL and
criterium loaded.

```sh
$ clj -Sdeps '{:deps {org.lwjgl/lwjgl {:mvn/version "3.2.3"}
                      org.lwjgl/lwjgl-glfw {:mvn/version "3.2.3"}
                      org.lwjgl/lwjgl$natives-linux {:mvn/version "3.2.3"}
                      org.lwjgl/lwjgl-glfw$natives-linux {:mvn/version "3.2.3"}
                      criterium/criterium {:mvn/version "0.4.6"}}}'
```

Then from the repl

```clojure
user=> (import 'org.lwjgl.glfw.GLFW)
org.lwjgl.glfw.GLFW
user=> (require '[criterium.core :as bench])
nil
user=> (GLFW/glfwInit)
true
user=> (bench/bench (GLFW/glfwGetTime) :verbose)
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: -Dclojure.basis=/home/jsusk/.clojure/.cpcache/2667074721.basis
Evaluation count : 1613349900 in 60 samples of 26889165 calls.
      Execution time sample mean : 32.698446 ns
             Execution time mean : 32.697811 ns
Execution time sample std-deviation : 1.274600 ns
    Execution time std-deviation : 1.276437 ns
   Execution time lower quantile : 30.750813 ns ( 2.5%)
   Execution time upper quantile : 33.757662 ns (97.5%)
                   Overhead used : 6.400704 ns
nil
```

GLFW requires that we initialize it before calling the `glfwGetTime` function.
Besides that this is a simple interop call which directly maps to the native
function.

This gives us a basis of 32.7 ns +/-1.3 ns. All other libraries will be
evaluated relative to this result.

To ensure fairness, we'll also get that overhead value to be used in further
tests.

```clojure
user=> bench/estimated-overhead-cache
6.400703613065185E-9
```

### Coffi
The dependencies when using coffi are simpler, but it also requires some JVM
options to support the foreign access api.

```sh
$ clj -Sdeps '{:deps {org.suskalo/coffi {:mvn/version "0.1.205"}
                      criterium/criterium {:mvn/version "0.4.6"}}}' \
      -J--add-modules=jdk.incubator.foreign \
      -J--enable-native-access=ALL-UNNAMED
```

In order to ensure fair comparisons, we're going to use the same overhead value
on each run, so before we do the benchmark we'll set it to the observed value
from last time.

```clojure
user=> (require '[criterium.core :as bench])
nil
user=> (alter-var-root #'bench/estimated-overhead-cache (constantly 6.400703613065185E-9))
6.400703613065185E-9
user=> (require '[coffi.ffi :as ffi])
nil
user=> (require '[coffi.mem :as mem])
nil
user=> (ffi/load-system-library "glfw")
nil
user=> ((ffi/cfn "glfwInit" [] ::mem/int))
1
user=> (let [f (ffi/cfn "glfwGetTime" [] ::mem/double)]
         (bench/bench (f) :verbose))
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: --add-modules=jdk.incubator.foreign --enable-native-access=ALL-UNNAMED -Dclojure.basis=/home/jsusk/.clojure/.cpcache/72793624.basis
Evaluation count : 1657995600 in 60 samples of 27633260 calls.
      Execution time sample mean : 31.382665 ns
             Execution time mean : 31.386493 ns
Execution time sample std-deviation : 1.598571 ns
    Execution time std-deviation : 1.608818 ns
   Execution time lower quantile : 29.761194 ns ( 2.5%)
   Execution time upper quantile : 33.228276 ns (97.5%)
                   Overhead used : 6.400704 ns
nil
```

This result is about 1.3 ns faster, and while that is less than the standard
deviation of 1.6, it's quite close to it.

### Clojure-JNA
Clojure-JNA uses the JNA library, which was designed to provide Java with an
easy way to access native libraries, but which is known for not having the
greatest performance. Since this is an older project, I'm also including the
clojure dependency to ensure the correct version is used.

```sh
$ clj -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.10.3"}
                      net.n01se/clojure-jna {:mvn/version "1.0.0"}
                      criterium/criterium {:mvn/version "0.4.6"}}}'
```

The naive way to call the function using Clojure-JNA is to use `jna/invoke`.

```clojure
user=> (require '[criterium.core :as bench])
nil
user=> (alter-var-root #'bench/estimated-overhead-cache (constantly 6.400703613065185E-9))
6.400703613065185E-9
user=> (require '[net.n01se.clojure-jna :as jna])
nil
user=> (jna/invoke Integer glfw/glfwInit)
1
user=> (bench/bench (jna/invoke Double glfw/glfwGetTime) :verbose)
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: -Dclojure.basis=/home/jsusk/.clojure/.cpcache/3229486237.basis
Evaluation count : 195948720 in 60 samples of 3265812 calls.
      Execution time sample mean : 350.335614 ns
             Execution time mean : 350.373520 ns
Execution time sample std-deviation : 24.833070 ns
    Execution time std-deviation : 24.755929 ns
   Execution time lower quantile : 300.000019 ns ( 2.5%)
   Execution time upper quantile : 365.759273 ns (97.5%)
                   Overhead used : 6.400704 ns

Found 13 outliers in 60 samples (21.6667 %)
	low-severe	 12 (20.0000 %)
	low-mild	 1 (1.6667 %)
 Variance from outliers : 53.4220 % Variance is severely inflated by outliers
nil
```

As you can see, this method of calling functions is very bad for performance,
with call overhead dominating function runtime by an order of magnitude. That
said, this isn't a completely fair comparison, nor the most realistic, because
this way of calling functions looks the function up on each invocation.

To adjust for this, we'll use the `jna/to-fn` function to give a persistent
handle to the function that we can call.

```clojure
user=> (let [f (jna/to-fn Double glfw/glfwGetTime)]
         (bench/bench (f) :verbose))
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: -Dclojure.basis=/home/jsusk/.clojure/.cpcache/3229486237.basis
Evaluation count : 611095020 in 60 samples of 10184917 calls.
      Execution time sample mean : 104.623634 ns
             Execution time mean : 104.638406 ns
Execution time sample std-deviation : 7.649296 ns
    Execution time std-deviation : 7.638963 ns
   Execution time lower quantile : 92.446016 ns ( 2.5%)
   Execution time upper quantile : 110.258832 ns (97.5%)
                   Overhead used : 6.400704 ns
nil
```

This is much better, but is still about 3x slower than JNI, meaning the overhead
from using JNA is still bigger than the function runtime.

This performance penalty is still small in the scope of longer-running
functions, and so may not be a concern for your application, but it is something
to be aware of.

### tech.jna
The tech.jna library is similar in scope to Clojure-JNA, however was written to
fit into an ecosystem of libraries meant for array-based programming for machine
learning and data science.

```sh
$ clj -Sdeps '{:deps {techascent/tech.jna {:mvn/version "4.05"}
                      criterium/criterium {:mvn/version "0.4.6"}}}'
```

This library is also quite simple to use, the only slightly odd thing I'm doing
here is to dereference the var outside the benchmark in order to ensure it's an
apples-to-apples comparison. We don't want var dereference time mucking up our
benchmark.

```clojure
user=> (require '[criterium.core :as bench])
nil
user=> (alter-var-root #'bench/estimated-overhead-cache (constantly 6.400703613065185E-9))
6.400703613065185E-9
user=> (require '[tech.v3.jna :as jna])
nil
user=> (jna/def-jna-fn "glfw" glfwInit "initialize glfw" Integer)
#'user/glfwInit
user=> (glfwInit)
Oct 09, 2021 10:30:50 AM clojure.tools.logging$eval1122$fn__1125 invoke
INFO: Library glfw found at [:system "glfw"]
1
user=> (jna/def-jna-fn "glfw" glfwGetTime "gets the time as a double since init" Double)
#'user/glfwGetTime
user=> (let [f @#'glfwGetTime]
         (bench/bench (f) :verbose))
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: -Dclojure.basis=/home/jsusk/.clojure/.cpcache/2910209237.basis
Evaluation count : 323281680 in 60 samples of 5388028 calls.
      Execution time sample mean : 203.976803 ns
             Execution time mean : 203.818712 ns
Execution time sample std-deviation : 14.557312 ns
    Execution time std-deviation : 14.614080 ns
   Execution time lower quantile : 179.732593 ns ( 2.5%)
   Execution time upper quantile : 213.929374 ns (97.5%)
                   Overhead used : 6.400704 ns
nil
```

This version is even slower than Clojure-JNA. I'm unsure where this overhead is
coming from, but I'll admit that I haven't looked at their implementations very
closely.

### dtype-next
The library dtype-next replaced tech.jna in the toolkit of the group working on
machine learning and array-based programming, and it includes support for
composite data types including structs, as well as primitive functions and
callbacks.

In addition, dtype-next has two different ffi backends. First is JNA, which is
usable on any JDK version, and is what we'll use for the first benchmark. Second
is the Java 16 version of Project Panama, which will be shown next.

In order to use the dtype-next ffi with the JNA backend, the JNA library has to
be included in the dependencies.

```sh
$ clj -Sdeps '{:deps {cnuernber/dtype-next {:mvn/version "8.032"}
                      net.java.dev.jna/jna {:mvn/version "5.8.0"}
                      criterium/criterium {:mvn/version "0.4.6"}}}'
```

The dtype-next library also requires some more ceremony around declaring native
functions. One advantage this has is that multiple symbols with the same name
can be loaded from different shared libraries, but it also does increase
friction when defining native wrappers.

Some easier ways to define native wrappers are provided than what is seen here,
but they share some disadvantages in documentation over the core methods
provided in coffi, although they are comparable to the data model provided in
coffi.

```clojure
user=> (require '[criterium.core :as bench])
nil
user=> (alter-var-root #'bench/estimated-overhead-cache (constantly 6.400703613065185E-9))
6.400703613065185E-9
user=> (require '[tech.v3.datatype.ffi :as dt-ffi])
nil
user=> (def fn-defs {:glfwInit {:rettype :int32} :glfwGetTime {:rettype :float64}})
#'user/fn-defs
user=> (def library-def (dt-ffi/define-library fn-defs))
#'user/library-def
user=> (def library-instance (dt-ffi/instantiate-library library-def "/usr/lib/libglfw.so"))
#'user/library-instance
user=> (def init (:glfwInit @library-instance))
#'user/init
user=> (init)
1
user=> (let [f (:glfwGetTime @library-instance)]
         (bench/bench (f) :verbose))
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 17+35-2724
Runtime arguments: -Dclojure.basis=/home/jsusk/.clojure/.cpcache/643862289.basis
Evaluation count : 710822100 in 60 samples of 11847035 calls.
      Execution time sample mean : 90.900112 ns
             Execution time mean : 90.919917 ns
Execution time sample std-deviation : 6.463312 ns
    Execution time std-deviation : 6.470108 ns
   Execution time lower quantile : 79.817126 ns ( 2.5%)
   Execution time upper quantile : 95.454652 ns (97.5%)
                   Overhead used : 6.400704 ns
nil
```

This version of JNA usage is significantly faster than either of the other JNA
libraries, but is still substantially slower than using JNI or coffi.

In addition to the JNA backend, dtype-next has a Java 16-specific backend that
uses an older version of Panama. This version requires similar setup to coffi in
order to run.

```sh
$ clj -Sdeps '{:deps {cnuernber/dtype-next {:mvn/version "8.032"}
                      criterium/criterium {:mvn/version "0.4.6"}}}' \
      -J--add-modules=jdk.incubator.foreign \
      -J-Dforeign.restricted=permit \
      -J--add-opens=java.base/java.lang=ALL-UNNAMED \
      -J-Djava.library.path=/usr/lib/x86_64-linux-gnu
```

The actual code to run the benchmark is identical to the last example, but is
reproduced here for completeness.

```clojure
user=> (require '[criterium.core :as bench])
nil
user=> (alter-var-root #'bench/estimated-overhead-cache (constantly 6.400703613065185E-9))
6.400703613065185E-9
user=> (require '[tech.v3.datatype.ffi :as dt-ffi])
nil
user=> (def fn-defs {:glfwInit {:rettype :int32} :glfwGetTime {:rettype :float64}})
#'user/fn-defs
user=> (def library-def (dt-ffi/define-library fn-defs))
#'user/library-def
user=> (def library-instance (dt-ffi/instantiate-library library-def "/usr/lib/libglfw.so"))
#'user/library-instance
user=> (def init (:glfwInit @library-instance))
#'user/init
user=> (init)
1
user=> (let [f (:glfwGetTime @library-instance)]
         (bench/bench (f) :verbose))
amd64 Linux 5.10.68-1-MANJARO 8 cpu(s)
OpenJDK 64-Bit Server VM 16.0.2+7
Runtime arguments: --add-modules=jdk.incubator.foreign -Dforeign.restricted=permit --add-opens=java.base/java.lang=ALL-UNNAMED -Djava.library.path=/usr/lib/x86_64-linux-gnu -Dclojure.basis=/home/jsusk/.clojure/.cpcache/2337051659.basis
Evaluation count : 1588513080 in 60 samples of 26475218 calls.
      Execution time sample mean : 58.732468 ns
             Execution time mean : 58.647361 ns
Execution time sample std-deviation : 9.732389 ns
    Execution time std-deviation : 9.791738 ns
   Execution time lower quantile : 31.318115 ns ( 2.5%)
   Execution time upper quantile : 65.449222 ns (97.5%)
                   Overhead used : 6.400704 ns

Found 14 outliers in 60 samples (23.3333 %)
	low-severe	 8 (13.3333 %)
	low-mild	 4 (6.6667 %)
	high-mild	 2 (3.3333 %)
 Variance from outliers : 87.6044 % Variance is severely inflated by outliers
nil
```

Not reproduced here, but notable for comparison, in my testing Java 16's version
of the JNI version performed about the same.

This is significantly faster than the JNA version of dtype-next, but it is still
slower than modern Panama. This is likely to simply be a result of optimizations
and changes to the Panama API, and when dtype-next is updated to use the Java 17
version of Panama I expect it will perform in line with coffi, but this
benchmark will be reproduced when this happens. Still, this shows that as it
stands, coffi is the fastest FFI available to Clojure developers.
