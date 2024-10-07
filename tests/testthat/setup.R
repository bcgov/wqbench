# Some underlying functions use partial name matching; these warnings
# pollute the test output so we silence them just while tests run
op <- options(warnPartialMatchArgs = FALSE)

withr::defer(options(op), teardown_env())
