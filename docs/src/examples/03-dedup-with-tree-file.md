---
title: Save hashes to a .bigtree file
...

[ex1]: /examples/01-dedup-folder-3-easy-steps.html

This is useful any time you want to scan something now and use it later.
That might be so you can:

* search the tree without plugging in the corresponding backup drive
* pre-run the long hashing step to speed up other operations
* diff the current state with a future one to see what changed

For now, let's say we want to dedup these files:

{{snippets/example03_step0_before.md}}

We'll pretend there are a lot more of them, so that it makes sense to avoid hashing them multiple times. Here's how we can do only the hashing step.

{{snippets/example03_step1_cmd.md}}

The `.bigtree` file is a flattened version of the tree structure BigTrees works with internally. It's mostly a list of lines (or a long table if you prefer) where each line has all the info we need about a particular file or folder.

{{snippets/example03_step1_bigtree.md}}

<!--
It's designed to be written and read back into memory efficiently, so you can work with it even if the list is much larger than you can fit in RAM.
-->

More importantly though, you can use it in place of an actual folder in many of the
other commands: `diff`, `dupes`, `find`, and `set-add`.

The main advantage is that if you want to play with the parameters of the dupe finding algorithm---which files it should include or exclude, how many levels deep to search, how to sort the results, etc---you won't have to re-run the hash step each time.

In my experience deduping large personal backup drives (~1-4 terabytes and a few million files each) that might save you an entire day of work per run! Hashing could take 8-10 hours if you're doing it over a USB connection, and then the dupe finding algorithm might only take 10-30 minutes at the end.

Here's the same process we used in [the first example][ex1],
except starting from the tree file...

{{snippets/example03_step2_cmd.md}}

{{snippets/example03_step3_cmd.md}}

{{snippets/example03_step3_out.md}}

And the final deduped files:

{{snippets/example03_step3_after.md}}
