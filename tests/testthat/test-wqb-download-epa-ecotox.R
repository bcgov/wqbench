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

### may need to use withr to creates temp directory to test this step
### those tests will be slow as the db is large to download
### it could fail pending connection and other issues
