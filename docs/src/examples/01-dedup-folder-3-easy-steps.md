---
title: Dedup a folder in 3 easy steps
...

Step 1: Tell BigTrees to scan your drive/folder and generate a dedup script.

{{snippets/example01_step1_cmd.md}}

Step 2: Skim the script to make sure it's categorized the duplicates the way you want.
BigTrees sorts each set of dupes, putting its best guess about which copy you'd prefer to keep at the top. The script will confirm that one still exists, then delete the others.

Here are the relevant lines for our example:

{{snippets/example01_step1_dedup.md}}

You can edit the script now to pick different files if you want.
Just be sure to move any new `keep` lines to the top of their groups.

Step 3: When you're ready, go ahead and run it:

{{snippets/example01_step2_cmd.md}}

{{snippets/example01_step3_out.md}}
