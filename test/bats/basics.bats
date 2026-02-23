setup() {

  load 'helpers/bats-support/load'
  load 'helpers/bats-assert/load'
  load 'helpers/bats-file/load'
  load 'helpers/bats-example'

  # add bigtrees to path
  # TODO ensure it gets built first?
  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
  PATH="$DIR/../../result/bin":$PATH

}

@test "check bigtrees version" {
  run bigtrees version
  assert_output "0.27.3"
}

@test "check rsync version" {
  run rsync --version
  assert_output -p "rsync  version 3.4.1  protocol version 32"
}

@test "download test files" {
  download_test_files
  assert_exists "$TEST_FILES_ZIP"
}
