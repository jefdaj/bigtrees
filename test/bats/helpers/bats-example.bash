# TODO what should this file be called?

download_test_files() {
  # Downloads ~72M of test files once per nix-shell, which hopefully isn't too
  # taxing? Each example*.bats is expected to unzip and elaborate these files
  # into some custom format by moving and duplicating them.
  # TODO is there a way to show progress
  [[ -z "$TMPDIR" ]] && TMPDIR='/tmp'
  export TEST_FILES_ZIP="${TMPDIR}/test-files.zip"
  if [[ ! -f "$TEST_FILES_ZIP" ]]; then
    repo='https://github.com/Josef-Friedrich/test-files'
    url="${repo}/archive/ab8948e99c2f52717fc62e2913cc277b25e5b200.zip"
    curl -o "$TEST_FILES_ZIP" -L --retry 3 "$url" || rm "$TEST_FILES_ZIP"
  fi
}

setup_example_file() {

  load 'helpers/bats-support/load'
  load 'helpers/bats-file/load'

  download_test_files
  assert_exists "$TEST_FILES_ZIP"

  # TODO can we get away without naming them?
  example_basename="$1"

  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"

  export TEST_EXAMPLE_DIR="$(temp_make --prefix "$example_basename")"
  cd "$TEST_EXAMPLE_DIR"

  # TODO replace this with a generic set of base test files and custom elaboration fn
  # TODO no need to export either?
  # export TEST_TARBALL="${DIR}/${example_basename}.tar.xz"
  # tar -xf "$TEST_TARBALL"
  # assert_exists "$example_basename"

  unzip "$TEST_FILES_ZIP"
  mv test-files-* test-files
  assert_exists "test-files"
  export TEST_FILES_DIR="${TEST_EXAMPLE_DIR}/test-files"

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
