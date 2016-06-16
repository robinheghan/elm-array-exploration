# CollectionsNg

CollectionsNg provides new implementations of `Array`, `Dict` and `Set`. The collections are
API-compatible with those found in `elm-core`. All you have to do to use these collections is to
import the proper namespace.

The collections in `elm-core` do not work properly with equality `(==)`, and the array implementation
is known to be unstable. This library aims to fix those problems.

The performance characteristics differ. Until a benchmark suite has been set up, I don't know exactly
how they differ or by how much. My guess, however, is that this implementation on average uses more memory
and has slower insertion and removal of elements, but is faster to read from.

## Code shared

This library makes liberal use of code from `elm-core`. The `Set` implementation is more
or less the exact same code, the same goes for a lot of documentation comments.

## Todo

* Make sure the tests covers most situations, that the same conventions are used
throughout the project, and investigate use of `elm-check`.
* Setup a benchmark suite.
* Optimize (look at the bottom for optimization ideas).

## Why equality fails `elm-core` collections

The goal of this library is to provide stable and API-compatible implementations of the collections
in `elm-core`, but with working equality. Before discussing how the collections in this library
are implemented, it might be a good idea to talk about why the collections in `elm-core` fail.
The explanation I'm about to give applies to `Dict` and `Set`, I don't fully understand the `Array`
implementation and can therefore not say much about it.

The `elm-core` implementation of `Dict`, as is common in functional languages, uses a binary search
tree as it's basic data structure. For those who are new to this data structure, the basic idea is
that the top of the tree (the root) is represented by a node that contains a key and a value. The
node also has a reference to a key-value node where the key is less than the current key (this
reference is called `left`) and a reference to a key-value node where the key is larger than the
current key (this reference is called `right`). Those references also have `left` and `right`
references, and thus formes a tree. To find a given key, you start at the root, and if the
key you are looking for is larger than the key in the root, you go right, if it is less than
the root key, you go left. You do this until you've found the key you are looking for.

There are several upsides and downsides with binary search trees, and I don't plan to explain them
here. All I want is to explain why this implementation works badly in Elm with regards to equality.
To do that, I need to explain how equality works in Elm.

Elm's equality operator `(==)` is actually pretty simple, at least in theory. It compares if two
values look the same. For numbers it means that `1` equals `1`. For strings it means that `"key"`
equals `"key"`, and for booleans it means that `True` equals `True`. For compound values, like
lists, tuples and records, it checks that the structure and containing values are the same.

The last sentence is important. It doesn't check if the *content* is the same, it checks the
actual structure. A binary search tree's underlying structure between to instances can be different
even though the content is the same. Try imagining a tree containing the keys `"key1"` and `"key2"`.
Depending on which key is added first, the underlying structure will be different.

Since `Set` is just a thin wrapper around a `Dict`, the same problems applies here as well.

## Implementation details

So for equality to work, we need to implement `Dict` in a way that the underlying structure is
always the same given the same content. To ensure this we have implemented `Dict` using a data
structure known as Hash Array Mapped Trie (or HAMT for short). This data structure also powers the
core collections in `Clojure` and `Scala`. Our `Array` implementation also uses this data structure,
and simply uses the value index as a key.

This section describes the code in `src/CollectionsNg/Hamt.elm` from a birds eye view. You
should probably read this before reading the source code to understand how it all works.

The key component behind every hash-based data structure, is a hash. A hash is a simple
integer that represents a value. Equal values always have the same hash, while different
values *might* have the same hash, although we hope that doesn't happen too often. There
are many ways to generate hashes. For arrays, we just use the index. For sets and
dictionaries, we convert the key to a string using Elm's built-in `toString` function,
and then we hash it using `Skinney/murmur3`. Murmur 3 is the hashing function used
in Clojure for this purpose, and should generate hashes with a reasonably low chance
of generating the same hash for different strings.

Once you have a hash, you use it to locate the element you are looking for. In a HAMT,
data is stored in a tree structure where each node has 32 children, and the maximum
depth is 7. This gives us a total of 34 359 738 368 (32^7) places to store information.
So how do we use the hash to locate our information?

A hash is just an integer, containing 32 bits. To represent a number between 0 and 31 (remember, our
nodes can have 32 children), we only need 5 bits. Since there are 32 bits in an integer,
we can divide a hash into 7 groups, each group containing a number between 0 and 31.

So to find our element, we look at the first five bits to know where to look in the
first node in our tree. Then, if we've found a sub tree, we look at the next 5 bits
to know where to look next. We continue this until we've found the key we are looking for.

## Elm specific implementation challenges

### Equality

To ensure that the underlying data structure is always the same for a given set of contents,
we need to do two things:

* `remove` always does the exact opposite of `insert`. For this to work, we need a
bitmap to keep track of which positions in a node contains values. When a sub tree
only contains a single element, `remove` converts the sub tree to a simple value.

* Collision lists must be sorted. When multiple keys have the same hash, we store them
in a simple list. For equality to work, the order has to be the same. We therefore
sort collision lists. This also means that keys have to be comparable.

If we were able to override how equality worked, removing elements from a collection
would be a simpler operation. Because we have to sort collision lists, a collection
with a large number of collisions will have bad performance.

### Hashing

Creating a hash is perhaps the most intensive part of inserting, removing and retrieving elements.
Because Elm (currently)
have nothing that resembles interfaces, we need to create a string representation of the key,
and then hash the string. In Java, a default hash function using reflection is used for classes
that don't override this behaviour. In Kotlin's Javascript compiler, a custom hash function is
generated for each data class.

A built-in `hash` function in Elm would allow us to skip the `toString` step, which
should speed up hash based operations.

### No native arrays

Every node can store a total of 32 elements. Since storing
32 instances of `Nothing` would be just a waste of space, most HAMT implementations
use a compact array to store the contents of a node. However, Elm doesn't have a
built-in array structure, it only has a list. Since we need constant access to our
elements, the only way to implement this properly is to use a record with 32 fields.

This is bad for a couple of reasons. First of all it wastes memory, as a node can never
be smaller than 32 elements. This also means, that every modification to the collection,
always have to copy at least 31 references.

Another problem is that iterating through the collection (equality, foldl...) requires checking every field
on the node and testing if it contains a value or not. This is slower than it has to be.

Lucky for us, the fact that `remove` works as an exact opposite of `insert` ensures
that the collection always retains the most compact structure possible (we don't store 7 sub-trees
if one will do).

A better implementation would be possible if `elm-core` contained a copy-on-write abstraction
for Javascript arrays.

## Ideas for optimization

Language support for copy-on-write native arrays and interfaces aside, there are other ways to
improve performance of these collections.

* Be smarter when sorting the collision lists. Different keys with the same hash are stored in
a collision list. To ensure structural equality, this list is sorted after modification using
`List.sortBy`. Since we know that the list is always sorted, we can insert the element at
the correct position, instead of sorting the entire list.

* Specialize arrays. `Array` shares the underlying implementation of `Dict`, where the value index
is used as both hash and key. This is fine for a 1.0 release, but there are several drawbacks in
regards to performance. The biggest drawback is that when an array surpasses 32 elements, a new
sub tree is created for every element with index 33-64. This wastes a lot of space, and is also bad for iterating
the array. By creating a specialized implementation for `Array`, we can improve a lot on these
problems.

* Make more use of the position bitmap. We currently use a bitmap to store which fields in a node
contains a value. It might be faster to make use of this bitmap when iterating, instead of checking
the value of each field manually.

## License

This library uses the BSD3 License. See LICENSE for more information.
