---
title: Save hashes to a .bigtree file
...

[ex1]: /examples/01-dedup-folder-3-easy-steps.html

This is useful any time you want to scan something now and use it later.
That might be so you can:

* diff the current state with a future one
* search the tree without plugging in the corresponding backup drive
* pre-run the long hashing step to speed up other operations

{{snippets/example03_step1_cmd.md}}

{{snippets/example03_step1_bigtree.md}}

You can use `.bigtree` files in place of scanned folders in many of the
other commands (`diff`, `set-add`, `find`, and `dupes`).

Here's the same process from [the first example][ex1] again,
this time using the tree file:

<!-- TODO how to finish this example? maybe just link to the first one again? -->
<!-- TODO or show the tree? need some new css for that -->

{{snippets/example03_step2_cmd.md}}

{{snippets/example03_step3_cmd.md}}

{{snippets/example03_step3_out.md}}
