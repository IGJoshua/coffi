# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- Function `coffi.mem/struct-field-offset` for getting struct field offsets for use with reading/writing with unmarshaled segments that contain structs
- Support for `:raw?` type flag for array serdes to deserialize to raw java arrays for feature parity with defstruct

### Fixed
- Cyclic dependency when requiring `coffi.layout` before `coffi.mem`
- Missing require for `clojure.pprint` used in impl of `coffi.mem/defstruct`

## [1.0.615] - 2025-05-15
### Added
- New `coffi.mem/defstruct` macro to allow the definition of struct types with more performant serdes (thanks to @rutenkolk)
- Support for named union members in c-layout (thanks to @jjttjj)

### Fixed
- Bug where constants were deserialized incorrectly due to indirections

## [1.0.486] - 2024-10-04
### Fixed
- Bug where one too many indirections is used when serializing/deserializing pointer types

## [1.0.472] - 2024-10-03
### Added
- New `coffi.mem/null` var for implementing custom types

### Performance
- Upcall functions serialized from functions returned by deserializing function pointers now use the backing function pointer directly
- Upcall and downcall classes have been changed to be memoized, meaning ASM is no longer invoked every time a function is serialized, which should drastically improve performance where functions are serialized in a hot loop

### Fixed
- Incorrect docstring on `coffi.mem/address-of` that implied some type of pointer type was returned rather than a long
- Usage of deprecated `(Class/STATIC_FIELD)` access pattern

## [1.0.450] - 2024-10-02
### Added
- Support for JDK 22
- `reinterpret` function which changes the size associated with a segment, optionally associating it with an arena and cleanup action

### Changed
- Arglists and docstrings of functions to refer to arenas rather than the outdated terms scope or session
- Change the arguments to `as-segment` to take longs to account for the removal of an Address type

### Removed
- Deprecated functions referring to sessions and scopes
- Deprecated functions `slice-into` and `with-offset`, replaced by the function `slice`

### Fixed
- Prep step when using coffi as a dependency wouldn't re-run if it failed during execution, e.g. when using the incorrect java version

## [0.6.409] - 2023-03-31
### Added
- Support for JDK 19
- New macros for defining vars with values from native code
- New function to allow getting the backing memory segment of a `coffi.ffi.StaticVariable`, to replace the `Addressable` implementation lost in the migration to JDK 18

### Fixed
- Bug where `static-variable`s with primitive types would not deserialize properly on `deref`
- Uses of `defvar` not compiling
- Bug where nil values would not be correctly coerced to null pointers when passed to inlined functions
- Bug where inline serde functions would fail on complex pointer types
- Bug where padding in structs may be increased when fields have alignments less than their size
- Bug where pointer alignment was incorrectly defined

### Changed
- References to `scope` as a term have been changed to `session` to match Panama messaging. Where this conflicts with function names, old versions have been deprecated and new names have been introduced.

## [0.5.357] - 2022-07-07
### Removed
- `:coffi.mem/long-long` primitive type
- `coffi.mem/slice-into`; the function no longer has an equivalent in panama, but see 2-arity of `coffi.mem/as-segment` for an alternative

### Changed
- `coffi.mem/as-segment` no longer has a close action arity
- JDK version from 17 to 18

## [0.4.341] - 2022-01-23
### Added
- Constants for size and alignment of primitive types
- Support for non-native byte orders of primitive types
- Functions for reading and writing primitive types (e.g. `coffi.mem/read-float`, `coffi.mem/write-long`, etc.)
- Layout objects may now be passed to `coffi.mem/size-of` and `coffi.mem/align-of`
- Constants for native-order primitive layouts
- Constants for byte orders

### Changed
- The `coffi.mem/primitive?` predicate is now actually a function instead of a set

## [0.3.298] - 2022-01-10
### Added
- New `coffi.layout` namespace with support for forcing C layout rules on structs

### Fixed
- C-characters were being read as UTF-16 rather than ASCII code points

## [0.2.277] - 2021-10-25
### Fixed
- Non-primitive arguments on upcalls would generate invalid bytecode with `nil` instructions

## [0.2.259] - 2021-10-16
### Fixed
- Long and double arguments to upcalls failed to compile in some cases
- Void return types on upcalls would crash on serialization

## [0.1.251] - 2021-10-14
### Fixed
- Bug with the inline expansion of `make-serde-wrapper`, make it more maintainable

## [0.1.246] - 2021-10-14
### Fixed
- Incorrect inline expansion of `make-serde-wrapper` in cases where a function has no arguments

## [0.1.241] - 2021-10-14
### Performance
- Added an `:inline` function to `make-serde-wrapper` to remove serialization overhead on primitives
- Added multimethod implementations for primitives in (de)serialization functions, rather than using the default

### Fixed
- `cfn` didn't add serializers with non-primitive types in some cases

## [0.1.220] - 2021-10-09
### Fixed
- All-primitive method types still used serialization when called from `cfn`
- Arrays deserialized to non-vector sequences
- Non-primitive argument types fail to link

## [0.1.205] - 2021-10-06
### Added
- An `address?` predicate

### Fixed
- Compound types caused problems in arglists meta on expansion of `defcfn`
- Compound types were not allowed as return types in `defcfn`
- `nil` was not considered a null pointer
- Primitive-serializing types fail to compile as arguments to downcall handles
- Primitive-serializing types fail to load as arguments to upcall functions
- Void return types on upcalls crash the JVM
- Invalid implementation of serialize-into for primitive types

## [0.1.192] - 2021-09-30
### Added
- An `::ffi/address` key to wrapper functions' metadata

### Fixed
- Usage of a method no longer in Panama that breaks `with-acquired`

## [0.1.184] - 2021-09-30
### Fixed
- Deserializing nullpointers as functions threw an exception
- Upcall stubs with non-primitive arguments failed to compile
- Upcall stubs had incorrect types

## [0.1.176] - 2021-09-29
### Fixed
- Usage of `defcfn` without a docstring produced an invalid `def` form

## [0.1.169] - 2021-09-28
### Added
- Macro for easily wrapping native functions with arbitrary logic
- Functions for creating wrapper fns for native functions
- Macro for easily aliasing types
- Functions to create scopes for deterministic deallocation
- Functions to load libraries by name or path
- Support for serializing and deserializing arbitrary Clojure functions
- Support for serializing and deserializing arbitrary Clojure data structures

[Unreleased]: https://github.com/IGJoshua/coffi/compare/v1.0.615...develop
[1.0.615]: https://github.com/IGJoshua/coffi/compare/v1.0.486...v1.0.615
[1.0.486]: https://github.com/IGJoshua/coffi/compare/v1.0.472...v1.0.486
[1.0.472]: https://github.com/IGJoshua/coffi/compare/v1.0.450...v1.0.472
[1.0.450]: https://github.com/IGJoshua/coffi/compare/v0.6.409...v1.0.450
[0.6.409]: https://github.com/IGJoshua/coffi/compare/v0.5.357...v0.6.409
[0.5.357]: https://github.com/IGJoshua/coffi/compare/v0.4.341...v0.5.357
[0.4.341]: https://github.com/IGJoshua/coffi/compare/v0.3.298...v0.4.341
[0.3.298]: https://github.com/IGJoshua/coffi/compare/v0.2.277...v0.3.298
[0.2.277]: https://github.com/IGJoshua/coffi/compare/v0.2.259...v0.2.277
[0.2.259]: https://github.com/IGJoshua/coffi/compare/v0.1.251...v0.2.259
[0.1.251]: https://github.com/IGJoshua/coffi/compare/v0.1.246...v0.1.251
[0.1.246]: https://github.com/IGJoshua/coffi/compare/v0.1.241...v0.1.246
[0.1.241]: https://github.com/IGJoshua/coffi/compare/v0.1.220...v0.1.241
[0.1.220]: https://github.com/IGJoshua/coffi/compare/v0.1.205...v0.1.220
[0.1.205]: https://github.com/IGJoshua/coffi/compare/v0.1.192...v0.1.205
[0.1.192]: https://github.com/IGJoshua/coffi/compare/v0.1.184...v0.1.192
[0.1.184]: https://github.com/IGJoshua/coffi/compare/v0.1.176...v0.1.184
[0.1.176]: https://github.com/IGJoshua/coffi/compare/v0.1.169...v0.1.176
[0.1.169]: https://github.com/IGJoshua/coffi/compare/16f56bc31d69142ec4d2fb61b15b069d78b127ca...v0.1.169
