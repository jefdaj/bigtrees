---
title: Find across multiple drives
...

This is for when you have a large collection of files that you can't easily search at once.
For example they might be on a cloud backup service or a bunch of hard drives.

You scan each one once to a `.bigtree` file, then search through the scans.

``` bash
mkdir backup-drive-scans
for drive_name in backupA backupB backupC backupD; do
  read -p "mount ${drive_name} drive and press ENTER"
  mountpoint="/media/$(whoami)/${drive_name}"
  bigtrees hash "$mountpoint" \
    --output "backup-drive-scans/${drive_name}.bigtree"
done
```

``` bash
tree backup-drive-scans
```

``` stdout
backup-drive-scans
├── backupA.bigtree
├── backupB.bigtree
├── backupC.bigtree
└── backupD.bigtree
```

``` bash
for drive_name in backupA backupB backupC backupD; do
  echo "searching ${drive_name}..."
  bigtrees find \
    "backup-drive-scans/${drive_name}.bigtree" \
    --search-regex '.*\.png'
done
```

This gets more useful in combination with the `--searches-json` option,
which lets you define a bunch of different regexes and label the resulting files.
You can find all the ssh keys, all the Word docs, all the folders named like `YYYY-MM-DD`, etc.
Then when you do have access to a particular drive, you can easily pull out all
the files on you were interested in.
