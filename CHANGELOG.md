# ChangeLog

## 2.0.5

* Performance improvements.

## 2.0.4

* Fixed runtime exception when slicing a large array to a small (< 32 elements) one.

## 2.0.3

* Optimized append when appending a small array.

## 2.0.2

* Fixed bug where slicing everything but elements from the tail could return erronous result.
* Made append faster, especially when appending a small array.
* Made initialize faster.

## 2.0.1

* Fixed bug where slice could return an `Array` which would have failed a `==` operation.

## 2.0.0

* `Array` is now an opaque type.
* Performance improvements.

## 1.1.0

* Added `toString` function

## 1.0.0

* Initial release
