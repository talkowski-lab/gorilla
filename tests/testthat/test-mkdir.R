test_that("mkdir can make a directory", {
    path <- tempfile()
    expect_invisible(mkdir(path))
    withr::defer(unlink(path))
})

test_that("mkdir signals error on failure", {
    path <- file.path(tempfile(), "foo")
    expect_error(mkdir(path))
})
