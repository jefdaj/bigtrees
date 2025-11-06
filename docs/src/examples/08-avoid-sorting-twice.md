---
title: Avoid sorting the same files twice
...

Say you're looking through your backups, and you have many/most of them sorted already.
But then you discover another drive you forgot about!
BigTrees can show you which files on the new drive aren't already included in your main collection.

```.sh
# Step 1: make a reference set of all
# hashes in your existing collection

MAIN="/path/to/your/main/collection/here"
NEW="/path/to/new/files/here"

bigtrees set-add --set main.bigset "$MAIN"
```

Now there are two ways you could go:

a. Find files on the new drive that *aren't* already in your collection so you can add them
b. Delete files on the new drive that *are* already in your collection, so you can add the rest

```.sh
# Step 2a: list unique files on the new drive

bigtrees find "$NEW" \
  --exclude-set main.bigset \
  > new-files-not-in-main.txt
```

```.sh
# Step 2b: delete duplicate files on the new drive

bigtrees dupes "$NEW" \
  --output rm-files-already-in-main.sh \
  --dupes-out-fmt dedup-script \
  --reference-set main.bigset

bash rm-files-already-in-main.sh
```
