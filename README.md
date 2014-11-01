Haskell SortedSet and SortedMap based on [2-3trees][1] with node deletion marks

(2-3tree: uniform root-to-leaves tree height)

insert O(log n)
delete O(log n)

the clean function rebuilds the tree giving up the deleted nodes

[1]: https://en.wikipedia.org/wiki/2%E2%80%933_tree


