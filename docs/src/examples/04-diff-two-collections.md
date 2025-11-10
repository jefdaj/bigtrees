---
title: Diff two collections
...

Diff two folders
----------------

You can diff two folders directly, or substitute the saved hashes.
These do the same thing:

```
gander diff backup            current
gander diff backup-hashes.txt current
```

Output is like `diff -r`, but simplified by assuming the first folder is older.
[demo.sh][4] compares them:

```
creating some demo files...
backing them up...
continuing to edit the originals...
ok, they look like:

demo
├── backup
│   ├── file1.txt
│   └── folder1
│       ├── file3.txt
│       └── folder2
│           └── file2.txt
└── current
    ├── file1.txt
    ├── file3.txt
    └── folder1
        └── folder2
            ├── file2.txt
            └── file3.txt

6 directories, 7 files

this is what `diff -r` says about them:
Only in current: file3.txt
Only in backup/folder1: file3.txt
diff -r backup/folder1/folder2/file2.txt current/folder1/folder2/file2.txt
1a2
> edit the 2nd file
Only in current/folder1/folder2: file3.txt

and this is how `gander diff` explains it:
added 'file3.txt'
moved 'folder1/file3.txt' -> 'folder1/folder2/file3.txt'
edited 'folder1/folder2/file2.txt/file2.txt'
```

This will show moves as well as edits.

<!-- gander readme example here -->

```.sh
DIR1="/path/to/dir1/here"
DIR2="/path/to/dir2/here"
bigtrees diff "$DIR1" "$DIR2"
```

The output is formatted assuming the first path is an older version of the second.
I've found that to be what I want the majority of the time,
and if not then it's still not hard to read.

You can use a `.bigtree` file in place of one or both of the folders:

```.sh
bigtrees diff dir1-backup-2025.bigtree "$DIR1"

bigtrees diff dir1-backup-2023.bigtree dir1-backup-2025.bigtree
```

A couple gotchas:

* Single lines in the diff can be large folders.

* If you move *and* edit a file or folder, that won't be detected.
  It'll appear as deleting the old one and adding the new one.
