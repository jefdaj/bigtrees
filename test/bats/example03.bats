gen_example03_data() {
  # This expects that setup_example_file has unzipped ./test-files
  mkdir example03
  mv test-files/mp3/*.mp3 example03/
  for n in {1..3}; do
    cp example03/mozart.mp3 "example03/mozart (copy ${n}).mp3"
    cp -r test-files/pdf "example03/pdf_${n}"
  done
  rm -r test-files
}

setup_file() {
  load 'helpers/bats-example'
  setup_example_file 'example03'
  gen_example03_data
}

setup() {
  load 'helpers/bats-example'
  setup_example_step
}

teardown_file() {
  teardown_example_file 'example03'
}

@test "example 3 step 1: hash dir to .bigtree file" {
  run_snippet 'step1_cmd.sh' '''bigtrees hash example03 \
    --output example03.bigtree'''
  assert_exists example03.bigtree
}

@test "example 3 step 2: find dupes from .bigtree file" {

  run_snippet 'step2_cmd.sh' '''bigtrees dupes example03.bigtree \
    --output dedup.sh \
    --dupes-out-fmt dedup-script'''

  assert_exists dedup.sh

}

@test "example 3 step 3: rm dupes using dedup.sh" {

  assert_exists example03
  assert_exists example03/pdf_1
  assert_exists example03/pdf_2
  assert_exists example03/pdf_3
  assert_exists 'example03/mozart.mp3'
  assert_exists 'example03/mozart (copy 1).mp3'
  assert_exists 'example03/mozart (copy 2).mp3'
  assert_exists 'example03/mozart (copy 3).mp3'

  file_snippet 'step3_dedup.sh' 'dedup.sh'
  run_snippet  'step3_cmd.sh' 'bash dedup.sh'
  text_snippet 'step3_out.txt' "$output"

  assert_exists     example03
  assert_exists     example03/pdf_1
  assert_not_exists example03/pdf_2
  assert_not_exists example03/pdf_3
  assert_exists     'example03/mozart.mp3'
  assert_not_exists 'example03/mozart (copy 1).mp3'
  assert_not_exists 'example03/mozart (copy 2).mp3'
  assert_not_exists 'example03/mozart (copy 3).mp3'

}
