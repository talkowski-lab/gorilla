test_that("constructing a `pe_file` object works", {
    skip_tabix()

    pe_path <- "foo/bar.PE.tsv.gz"
    cache_path <- withr::local_tempdir("cache")
    handle <- new_pe_file(pe_path, cache_path)
    truth <- structure(
        list(
             handle = structure(
                list(path = pe_path, cachedir = cache_path),
                class = "tabix_handle"
             )
        ),
        class = "pe_file"
    )
    expect_identical(handle, truth)

    handle <- pe_file(pe_path, cache_path)
    expect_identical(handle, truth)
})

test_that("querying a `pe_file` object works", {
    skip_tabix()

    pe_path <- file.path(getwd(), test_path("data", "simple.PE.txt.gz"))
    cache_path <- withr::local_tempdir("cache")
    handle <- pe_file(pe_path, cache_path)
    pe_data <- query(handle, "chr8", 1000000, 1000040)

    truth_pe_data <- data.table(
        rcontig = c("chr8", "chr8", "chr8"),
        rstart = c(1000027L, 1000028L, 1000038L),
        rstrand = c("+", "-", "+"),
        mcontig = c("chrY", "chr8", "chr9"),
        mstart = c(15882971L, 1000133L, 36347121L),
        mstrand = c("+", "+", "+"),
        sample_id = c("bonobo", "orangutan", "gibbon"),
        key = c("sample_id", "rstart")
    )
    truth <- structure(
        list(
            mat = truth_pe_data,
            region = list(contig = "chr8", start = 1000000, end = 1000040)
        ),
        class = "pe_mat"
    )
    expect_identical(pe_data, truth)
})

test_that("querying a `pe_file` object without overlapping ranges returns a `data.table`", {
    skip_tabix()

    pe_path <- file.path(getwd(), test_path("data", "simple.PE.txt.gz"))
    cache_path <- withr::local_tempdir("cache")
    handle <- pe_file(pe_path, cache_path)
    pe_data <- query(handle, "chrX", 1000000, 1000040)

    truth_pe_data <- data.table(
        rcontig = character(),
        rstart = integer(),
        rstrand = character(),
        mcontig = character(),
        mstart = integer(),
        mstrand = character(),
        sample_id = character(),
        key = c("sample_id", "rstart")
    )
    truth <- structure(
        list(
            mat = truth_pe_data,
            region = list(contig = "chrX", start = 1000000, end = 1000040)
        ),
        class = "pe_mat"
    )
    expect_identical(pe_data, truth)
})

test_that("subsetting a `pe_mat` object by samples works", {
    skip_tabix()

    pe_path <- file.path(getwd(), test_path("data", "simple.PE.txt.gz"))
    cache_path <- withr::local_tempdir("cache")
    handle <- pe_file(pe_path, cache_path)
    pe_data <- query(handle, "chr8", 1000050, 1000100)
    subset_pe_data <- subset_samples(pe_data, "bonobo")

    truth_pe_data <- data.table(
        rcontig = "chr8",
        rstart = 1000085L,
        rstrand = "-",
        mcontig = "chr8",
        mstart = 1000138L,
        mstrand = "+",
        sample_id = "bonobo",
        key = c("sample_id", "rstart")
    )
    truth <- structure(
        list(
            mat = truth_pe_data,
            region = list(contig = "chr8", start = 1000050, end = 1000100)
        ),
        class = "pe_mat"
    )
    expect_identical(subset_pe_data, truth)
})

test_that("concatenating `pe_mat` objects works", {
    skip_tabix()

    pe_path <- file.path(getwd(), test_path("data", "simple.PE.txt.gz"))
    cache_path <- withr::local_tempdir("cache")
    handle <- pe_file(pe_path, cache_path)
    pe_data0 <- query(handle, "chr8", 1000050, 1000100)

    truth_pe_data0 <- data.table(
        rcontig = c("chr8", "chr8", "chr8", "chr8"),
        rstart = c(1000050L, 1000057L, 1000085L, 1000089L),
        rstrand = c("+", "+", "-", "-"),
        mcontig = c("chr14", "chr13", "chr8", "chr14"),
        mstart = c(34860520L, 55555583L, 1000138L, 99420947L),
        mstrand = c("+", "-", "+", "+"),
        sample_id = c("chimpanzee", "chimpanzee", "bonobo", "chimpanzee"),
        key = c("sample_id", "rstart")
    )
    truth <- structure(
        list(
            mat = truth_pe_data0,
            region = list(contig = "chr8", start = 1000050, end = 1000100)
        ),
        class = "pe_mat"
    )
    expect_identical(c(pe_data0), truth)
})
