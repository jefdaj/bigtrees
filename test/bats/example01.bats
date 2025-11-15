# TODO write example 02
# TODO factor out common setup
# TODO factor out setup_example, teardown_example
# TODO separate examples/ from test/?

setup_file() {

  load 'helpers/bats-support/load'
  load 'helpers/bats-file/load'

  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
  export TEST_TARBALL="${DIR}/example01.tar.xz"

  # set up test data for this example
  export TEST_EXAMPLE_DIR="$(temp_make)"
  cd "$TEST_EXAMPLE_DIR"
  tar -xf "$TEST_TARBALL"
  assert_exists example01

}

teardown_file() {
  temp_del "$TEST_EXAMPLE_DIR"
}

setup() {

  load 'helpers/bats-support/load'
  load 'helpers/bats-assert/load'
  load 'helpers/bats-file/load'

  # add bigtrees to path
  # TODO ensure it gets built first?
  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
  PATH="$DIR/../../result/bin":$PATH

  cd "$TEST_EXAMPLE_DIR"
}

@test "example 01 step 1: find dupes" {
  # TODO save this to a snippet somehow for the docs site
  # TODO use an example01.tar to make linking from docs easier?
  run bigtrees dupes example01 \
    --output dedup.sh \
    --dupes-out-fmt dedup-script
  assert_exists dedup.sh
  assert_exists example01/files
  assert_exists example01/files_copy
}

@test "example 01 step 2: rm dupes" {
  run bash dedup.sh
  assert_output -p "KEEP 'example01/files'"
  assert_exists     example01/files
  assert_not_exists example01/files_copy
}
