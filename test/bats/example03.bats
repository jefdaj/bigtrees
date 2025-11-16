# TODO start from a small number of actual example files in the repo,
#      and have a function in each example to elaborate it out into something duplicated.
#      much more efficient that way, and also easier to change

setup_file() {
  load 'helpers/bats-example'
  setup_example_file 'example01'
}

setup() {
  load 'helpers/bats-example'
  setup_example_step
}

teardown_file() {
  teardown_example_file 'example01'
}

@test "example 3 step 1: hash dir to .bigtree file" {
  run bigtrees hash example01 --output example03.bigtree
  assert_exists example03.bigtree
}

@test "example 3 step 2: find dupes from .bigtree file" {

  run bigtrees dupes example03.bigtree \
    --output dedup.sh \
    --dupes-out-fmt dedup-script

  assert_exists dedup.sh
  assert_exists example01/files
  assert_exists example01/files_copy

}

@test "example 3 step 3: rm dupes using dedup.sh" {

  run bash dedup.sh

  assert_output -p "KEEP 'example01/files'"
  assert_exists     example01/files
  assert_not_exists example01/files_copy

}
