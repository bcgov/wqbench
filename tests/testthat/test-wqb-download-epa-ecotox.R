test_that("error when version beyond 4 passed", {
  expect_error(
    wqb_download_epa_ecotox(version = 5),
    regexp = "`version` must be between 1 and 4, not 5\\."
  )
})

test_that("error when version lower then 1 is passed", {
  expect_error(
    wqb_download_epa_ecotox(version = 0),
    regexp = "`version` must be between 1 and 4, not 0\\."
  )
})

test_that("folder is downloaded", {
  skip_if_offline("https://gaftp.epa.gov/ecotox/")
  
  withr::defer(unlink(write_folder, recursive = TRUE))
  write_folder <- withr::local_tempdir()
  expect_message(
    wqb_download_epa_ecotox(
      file_path = write_folder,
      ask = FALSE
    ),
    regexp = "Downloading..."
  )
  expect_true(
    length(list.files(write_folder)) == 1
  )
  expect_true(
    "tests.txt" %in% basename(list.files(write_folder, recursive = TRUE))
  )
})
