# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
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

[Unreleased]: https://github.com/IGJoshua/coffi/compare/v0.1.176...HEAD
[0.1.176]: https://github.com/IGJoshua/coffi/compare/v0.1.169...v0.1.176
[0.1.169]: https://github.com/IGJoshua/coffi/compare/16f56bc31d69142ec4d2fb61b15b069d78b127ca...v0.1.169
