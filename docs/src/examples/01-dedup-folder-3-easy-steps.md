---
title: Dedup a folder in 3 easy steps
...

Imagine we want to dedup these files:

{{snippets/example01_step0_before.md}}

Step 1: Tell BigTrees to scan them and generate a dedup script.

{{snippets/example01_step1_cmd.md}}

Step 2: Skim the script to make sure it's categorized the duplicates the way you want.
BigTrees sorts each set of dupes, putting its best guess about which copy you'd prefer to keep at the top. The script will confirm that one still exists before deleting the others.

Here are the relevant lines for our example:

{{snippets/example01_step1_dedup.md}}

You can edit the script now if you want to pick specific files to keep.
Just move them to the tops of their sets.

Step 3: When you're ready, go ahead and run it:

{{snippets/example01_step2_cmd.md}}

{{snippets/example01_step3_out.md}}

{{snippets/example01_step3_after.md}}
