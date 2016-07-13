# Changelog

## Next

* New and better tests

## 2.0.0

* `size` function for `Dict` and `Set` returned wrong result when inserting or removing keys which
did not change size of collection.
* `Dict.merge` now works as expected, with solution inspired by elm-core PR #648.

## 1.0.0

* Initial release