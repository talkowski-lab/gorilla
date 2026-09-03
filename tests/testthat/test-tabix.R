test_that("a tabix handle can be made", {
    skip_tabix()

    tsv_path <- test_path("data", "simple_table.tsv.gz")
    cache_path <- withr::local_tempdir("cache")
    handle <- new_tabix_handle(tsv_path, cache_path)

    expect_s3_class(handle, "tabix_handle")
    expect_equal(names(handle), c("path", "cachedir"))
    expect_equal(handle$path, tsv_path)
    expect_equal(handle$cachedir, cache_path)
})

test_that("a tabix handle can be made with a `NULL` cache directory", {
    skip_tabix()

    handle <- new_tabix_handle("foo/bar", NULL)
    expect_s3_class(handle, "tabix_handle")
    expect_named(handle, c("path", "cachedir"))
    expect_true(dir.exists(handle$cachedir))
})

test_that("a tabix handle cannot be made if the cache directory cannot be made", {
    skip_tabix()

    tsv_path <- test_path("data", "simple_table.tsv.gz")
    cache_path <- file.path(withr::local_tempdir("cache"), "foo", "bar")
    expect_error(new_tabix_handle(tsv_path, cache_path))
})

test_that("a tabix handle can be queried", {
    skip_tabix()

    tsv_path <- file.path(getwd(), test_path("data", "simple_table.tsv.gz"))
    cache_path <- withr::local_tempdir("cache")
    handle <- new_tabix_handle(tsv_path, cache_path)
    result <- query(handle, "chr1", 500, 800)
    expect_true(file.exists(result))

    ranges <- read.table(
        result,
        header = TRUE,
        sep = "\t",
        check.names = FALSE,
        comment.char = "",
        colClasses = c("character", "integer", "integer", "character")
    )
    truth <- data.frame(
        `#chr` = "chr1",
        start = 300,
        end = 1000,
        id = "bonobo",
        check.names = FALSE
    )
    expect_equal(ranges, truth)
})
