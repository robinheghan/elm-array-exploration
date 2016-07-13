# Changelog

## 3.0.0

* New and better unit tests
* Fixed API incompatability problems with Dict and Array. We're now 100% API compatible.

## 2.0.0

* `size` function for `Dict` and `Set` returned wrong result when inserting or removing keys which
did not change size of collection.
* `Dict.merge` now works as expected, with solution inspired by elm-core PR #648.

## 1.0.0

* Initial release