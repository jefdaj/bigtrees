text_snippet() {
  # Takes a snippet name and some text, saves text to snippet.
  snippet_basename="$1"
  snippet_name="$(echo "$snippet_basename" | cut -d'.' -f1)"
  snippet_lang="$(echo "$snippet_basename" | cut -d'.' -f2)" # may be empty
  # [[ -z "$snippet_lang" ]] || snippet_lang="${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_text="$2"
  snippet_block="""\`\`\` ${snippet_lang}
${snippet_text}
\`\`\`"""
   echo "$snippet_block" > "$snippet_path"
 }

run_snippet() {
  # Takes a snippet name and command, saves command to snippet, runs command.
  snippet_basename="$1"
  snippet_name="$(echo "$snippet_basename" | cut -d'.' -f1)"
  snippet_lang="$(echo "$snippet_basename" | cut -d'.' -f2)" # may be empty
  # [[ -z "$snippet_lang" ]] || snippet_lang="${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_text="$2" # TODO take all remaining args?
  snippet_block="""\`\`\` ${snippet_lang}
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
  # [[ -z "$snippet_lang" ]] || snippet_lang=".${snippet_lang}"
  snippet_path="${SNIPPETS_DIR}/${TEST_EXAMPLE_BASENAME}_${snippet_name}.md"
  snippet_src="$2"
  # TODO put back? ${snippet_src}
  snippet_block="""\`\`\` ${snippet_lang}
$(cat "$snippet_src")
\`\`\`"""
  echo "$snippet_block" > "$snippet_path"
}

# diff_snippet() {
  # Takes a snippet name and two files, diffs them, saves diff to snippet.
# }
