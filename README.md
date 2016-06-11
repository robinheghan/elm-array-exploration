# HAMT Collections

A Hash Array Mapped Trie implementation of Array, Dict and Set in Elm.
Look towards the bottom of this document for a description of the implementation,
and ideas for further improvements.

## Usage

The collections are API compatible with the similarly named collections in `elm-core`.
All you have to do is change some `import` statements and some type information.

The big difference from the collections in `elm-core` is that equality (`==`) works.

## Code shared

This code makes liberal use of code from `elm-core`. The `Set` implementation is more
or less the exact same code, the same goes for a lot of documentation comments.

## Rationale

There are several reasons why I decided to write this library. The biggest of those
reasons is that I wanted to learn Elm and the workings of the HAMT data structure,
which is heavily used in both Clojure and Scala. Another big reason was that `Array`
in `elm-core` has some bugs, and that equality is known not to work with `Dict` and
`Set`.

## Todo

* Make sure the tests covers most situations, that the same conventions are used
throughout the project, and investigate use of `elm-check`.
* Setup benchmark.
* Optimize (look at the bottom for more concrete information).

## Hash Array Mapped Trie

The Hash Array Mapped Trie (or HAMT for short) backs the core data structures in both
Clojure and Scala. The goal of this project was to implement the data structure in
nothing but pure Elm, which presented some challenges.

Before describing the Elm specific challenges, let's first discuss how a HAMT works.
You can read the actual implementation in `src/HAMT/NodeList.elm`.

The key component behind every hash-based data structure, is a hash. A hash is a simple
integer that represents a value. Equal values always have the same hash, while different
values might have the same hash, although we hope that doesn't happen too often. There
are many ways to generate hashes. For arrays, we just use the index. For sets and
dictionaries, we convert the key/value to a string using Elm's built in `toString` function,
and then we hash it using `Skinney/murmur3`. Murmur 3 is the hashing function used
in Clojure for this purpose, and should generate hashes with a reasonably low chance
of generating the same hash for different strings.

Once you have a hash, you use it to locate the element you are looking for. In a HAMT,
data is stored in a tree structure where each node has 32 children, and the maximum
depth is 7. This gives us a total of 34 359 738 368 (32^7) places to store information.
So how do we use the hash to locate our information?

A hash is just a number, containing 32 bits (actually 53 bits in Javascript, but that's
another story). To represent a number between 0 and 31 (remember, our
nodes can have 32 children), we only need 5 bits. Since there are 32 bits in an integer,
we can divide a hash into 7 groups, each group containing a number between 0 and 31.

So to find our element, we look at the first five bits to know where to look in the
first node in our tree. Then, if we've found a child-node, we look at the next 5 bits
to know where to look in the next node. We continue this until we've found the value
we are looking for.

## Elm specific implementation challenges

### Equality

Elm (currently) doesn't have something that resembles interfaces. For this reason, we
need to use `toString` to be able to hash keys. Another, much bigger, problem is that
we have no way to specify how to consider two collections as equal. Elm's way of checking
that two data structures are equal, is a deep compare of the two. This means that,
for equality to work, we need to make sure that two collections with the same content
also has the exact same structure. In practice, this means that:

* `remove` always does the exact opposite of `insert`. For this to work, we need a
bitmap to keep track of which positions in a node contains values. When a child-node
only contains a single element, `remove` converts the child node to a simple value.

* Collision lists are sorted. When multiple keys have the same hash, we store them
in a simple list. For equality to work, the order has to be the same. We therefore
sort collision lists. This also means that keys have to be comparable.

If we were able to override how equality worked, removing elements from a collection
would be a simpler operation. Because we have to sort collision lists, a collection
with a large number of collisions will have bad performance.

### No native arrays

Every child node can store a total of 32 elements (or child nodes). Since storing
32 instances of `Nothing` would be just a waste of space, most HAMT implementations
use a simple array to store the contents of a node. However, Elm doesn't have a
builtin array structure, it only has a list. Since we need constant access to our
elements, the only way to implement this properly is to use a record with 32 fields.

This is bad for a couple of reasons. First of all it wastes memory, as a node can never
be smaller than 32 elements. This also means, that every modification to the collection,
always have to copy at least 31 references.

Another problem is that iterating through the collection requires checking every field
on the node and testing if it contains a value or not. This is slow.

Lucky for us, the fact that `remove` works as an exact opposite of `insert` ensures
that the collection always has the lowest node depth possible (we don't store 7 sub-trees
if one will do).

A better implementation would be possible if `elm-core` contained an immutable wrapper
over Javascript arrays.

### Hashing

Creating a hash is perhaps the most intensive part of inserting elements. Currently we
have no choice but to create a string representation of the key, and then hash the string.
In Java, a default hash function using reflection is used for classes that don't override
this behaviour. In Kotlin's Javascript compiler, a custom hash function is generated for
each data class.

A built in `hash` function in Elm would allow us to skip the `toString` step, which
should speed up insertion significantly.

## Ideas for better performance

Language support for simple/flat arrays and interfaces aside, there are other ways to
improve performance of these collections.

* Use a sorted binary tree instead of lists for collisions. Should improve performance
noticeably on collections with a high number of collisions. This was also recently done
in Java for their hash maps.

* Smarter use of array index as hash. In arrays, we just use the value index as the hash.
This works great when number if elements is less than 32, but once you get above that,
you are more or less guaranteed to create a child-node for every single push/append.

* We currently use a bitmap to store which fields in a node contains a value. It might
be faster to make use of this bitmap when iterating, instead of checking the value of
each field manually.

## License

This library uses the MIT License. See LICENSE for more information.
