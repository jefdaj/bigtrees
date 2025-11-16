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

@test "example 1 step 1: find dupes directly from dir" {

  # TODO save this to a snippet somehow for the docs site
  # TODO use an example01.tar to make linking from docs easier?

  run bigtrees dupes example01 \
    --output dedup.sh \
    --dupes-out-fmt dedup-script

  assert_exists dedup.sh
  assert_exists example01/files
  assert_exists example01/files_copy

}

@test "example 1 step 2: rm dupes using dedup.sh" {

  run bash dedup.sh

  assert_output -p "KEEP 'example01/files'"
  assert_exists     example01/files
  assert_not_exists example01/files_copy

}
