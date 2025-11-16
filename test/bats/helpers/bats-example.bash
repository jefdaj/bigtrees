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

  # TODO name them here, or in all the snippets?
  export TEST_EXAMPLE_BASENAME="$1"

  DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"

  export SNIPPETS_DIR="${DIR}/../../docs/src/snippets"

  export TEST_EXAMPLE_DIR="$(temp_make --prefix "$TEST_EXAMPLE_BASENAME")"
  cd "$TEST_EXAMPLE_DIR"

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

# TODO separate bats-snippet from bats-example

text_snippet() {
  # Takes a snippet name and some text, saves text to snippet.
  snippet_basename="$1"
  snippet_name="$(echo "$snippet_basename" | cut -d'.' -f1)"
  snippet_lang="$(echo "$snippet_basename" | cut -d'.' -f2)" # may be empty
  [[ -z "$snippet_lang" ]] || snippet_lang=".${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_text="$2"
  snippet_block="""\`\`\`${snippet_lang}
${snippet_text}
\`\`\`"""
   echo "$snippet_block" > "$snippet_path"
 }

run_snippet() {
  # Takes a snippet name and command, saves command to snippet, runs command.
  snippet_basename="$1"
  snippet_name="$(echo "$snippet_basename" | cut -d'.' -f1)"
  snippet_lang="$(echo "$snippet_basename" | cut -d'.' -f2)" # may be empty
  [[ -z "$snippet_lang" ]] || snippet_lang=".${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_text="$2" # TODO take all remaining args?
  snippet_block="""\`\`\`${snippet_lang}
${snippet_text}
\`\`\`"""
  echo "$snippet_block" > "$snippet_path"
  eval "run $snippet_text"
}

file_snippet() {
  # Takes a snippet name and a file, saves file contents to snippet.
  snippet_basename="$1"
  snippet_name="$(echo "$snippet_basename" | cut -d'.' -f1)"
  snippet_lang="$(echo "$snippet_basename" | cut -d'.' -f2)" # may be empty
  [[ -z "$snippet_lang" ]] || snippet_lang=".${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_src="$2"
  snippet_block="""\`\`\`${snippet_lang}
# ${snippet_src}

$(cat "$snippet_src")
\`\`\`"""
  echo "$snippet_block" > "$snippet_path"
}

# diff_snippet() {
  # Takes a snippet name and two files, diffs them, saves diff to snippet.
# }
