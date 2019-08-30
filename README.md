# Laughable

Disclaimer: until the official release, this readme is only aspirational, not all the features described here are implemented yet.

Given a type `t` with a `Traversable` instance, this library provides several variants of `t a` and `Free t a` for free:

* `NonEmpty t a`, which guarantees that there is at least one element.
* `Singleton t a`, which guarantees that there is exactly one element.
* `Zipper t a`, which guarantees that there is at least one element, the one at the cursor. You can move the cursor left and right in the order prescribed by `traverse` and apply `a -> a` modifications to the focussed element.
* `Dissected t c a j`, a zipper which supports `a -> b` modifications. All the elements to the left of the cursor have type `c` and all the elements to the right of the focus have type `j`, so you must change the cursor to a `j` in order to move left, or to a `c` in order to move right. Useful for converting a `t c` into a `t j` one element at a time. `c` and `j` stand for "clown" and "joker", from the paper "Clowns to the left of me, Jokers to the right" by Conor McBride, which is also where the word "dissected" comes from. The name of this library obviously derives from clowns and jokers.
* `Bisected t c j`, a variant of `Dissected` in which the cursor is in-between two elements rather than on an element. Use a `c -> j` to move left, or a `j -> c` to move right. This allows the container to be empty.
* `ZipperFree t a`, a variant of `Zipper (Free t) a` in which you can move up and down in addition to left and right.
* `DissectedFree t c a j`, a `Dissected (Free t) c a j` in which you can move up and down in addition to left and right. It doesn't make sense to have a `BisectedFree` variant because we can only move up and down at a node, not between nodes.
