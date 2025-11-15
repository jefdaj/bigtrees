# TODO what should this file be called?

setup_example_file() {

  example_basename="$1"

  load 'helpers/bats-support/load'
  load 'helpers/bats-file/load'

  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"

  export TEST_EXAMPLE_DIR="$(temp_make --prefix "$example_basename")"
  cd "$TEST_EXAMPLE_DIR"

  # TODO replace this with a generic set of base test files and custom elaboration fn
  # TODO no need to export either?
  export TEST_TARBALL="${DIR}/${example_basename}.tar.xz"
  tar -xf "$TEST_TARBALL"
  assert_exists "$example_basename"

}

teardown_example_file() {
  temp_del "$TEST_EXAMPLE_DIR"
}

setup_example_step() {

  load 'helpers/bats-support/load'
  load 'helpers/bats-assert/load'
  load 'helpers/bats-file/load'

  # add bigtrees to path
  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
  PATH="$DIR/../../result/bin":$PATH

  # always start in the tmpdir for the example
  cd "$TEST_EXAMPLE_DIR"

}
