# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
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

[Unreleased]: https://github.com/IGJoshua/coffi/compare/v0.1.205...HEAD
[0.1.205]: https://github.com/IGJoshua/coffi/compare/v0.1.192...v0.1.205
[0.1.192]: https://github.com/IGJoshua/coffi/compare/v0.1.184...v0.1.192
[0.1.184]: https://github.com/IGJoshua/coffi/compare/v0.1.176...v0.1.184
[0.1.176]: https://github.com/IGJoshua/coffi/compare/v0.1.169...v0.1.176
[0.1.169]: https://github.com/IGJoshua/coffi/compare/16f56bc31d69142ec4d2fb61b15b069d78b127ca...v0.1.169
