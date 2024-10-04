# Memory Management
In order to serialize any non-primitive type, off-heap memory needs to be
allocated. When memory is allocated inside the JVM, the memory is associated
with an arena. If none is provided, the arena is an implicit arena, and the
memory will be freed when the serialized object is garbage collected.

In many cases this is not desirable, because the memory is not freed in a
deterministic manner, causing garbage collection pauses to become longer, as
well as changing allocation performance. Instead of an implicit arena, there
are other kinds of arenas as well. A `confined-arena` is a thread-local arena.
Confined arenas are `Closeable`, which means they should usually be used in a
`with-open` form. When a `confined-arena` is closed, it immediately frees all
the memory associated with it. The previous example, `out-int`, can be
implemented with a confined arena.

```clojure
(defcfn out-int
  "out_int" [::mem/pointer] ::mem/void
  native-fn
  [i]
  (with-open [arena (mem/confined-arena)]
    (let [int-ptr (mem/serialize i [::mem/pointer ::mem/int] arena)]
      (native-fn int-ptr)
      (mem/deserialize int-ptr [::mem/pointer ::mem/int]))))
```

This will free the pointer immediately upon leaving the function.

When memory needs to be accessible from multiple threads, there's
`shared-arena`. When a `shared-arena` is `.close`d, it will release all its
associated memory immediately, and so this should only be done once all other
threads are done accessing memory associated with it.

In addition, two non-`Closeable` arenas are `global-arena`, which never frees
the resources associated with it, and `auto-arena`, which is an arena that frees
its resources once all of them are unreachable during a garbage collection
cycle, like an implicit arena, but potentially for multiple allocations rather
than just one.
