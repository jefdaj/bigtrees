gen_example01_data() {
  # This expects that we've already extracted test files to ./test-files
  mkdir example01
  mv test-files/mp3/*.mp3 example01/
  for n in {1..3}; do
    cp example01/mozart.mp3 "example01/mozart (copy ${n}).mp3"
    cp -r test-files/pdf "example01/pdf_${n}"
  done
  rm -r test-files
}

setup_file() {
  load 'helpers/bats-example'
  setup_example_file 'example01'
  gen_example01_data
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
  assert_exists 'example01/mozart (copy 1).mp3'

}

@test "example 1 step 2: rm dupes using dedup.sh" {

  assert_exists example01
  assert_exists example01/pdf_1
  assert_exists example01/pdf_2
  assert_exists example01/pdf_3
  assert_exists 'example01/mozart (copy 1).mp3'
  assert_exists 'example01/mozart (copy 2).mp3'
  assert_exists 'example01/mozart (copy 3).mp3'

  run bash dedup.sh

  assert_exists     example01
  assert_exists     example01/pdf_1
  assert_not_exists example01/pdf_2
  assert_not_exists example01/pdf_3
  assert_exists     'example01/mozart (copy 1).mp3'
  assert_not_exists 'example01/mozart (copy 2).mp3'
  assert_not_exists 'example01/mozart (copy 3).mp3'

}
