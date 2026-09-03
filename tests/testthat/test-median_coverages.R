test_that("a median coverages file can be read", {
    path <- file.path(getwd(), test_path("data", "median_coverages_clean.tsv"))
    covs <- read_median_coverages(path)
    truth <- c(gibbon = 30, orangutan = 27, gorilla = 33, chimpanzee = 40)
    expect_identical(covs, truth)
})

test_that("an error is signalled for duplicate samples", {
    path <- file.path(getwd(), test_path("data", "median_coverages_duplicates.tsv"))
    expect_error(read_median_coverages(path))
})

test_that("an error is signalled for non-positive coverage values", {
    path <- file.path(getwd(), test_path("data", "median_coverages_negatives.tsv"))
    expect_error(read_median_coverages(path))
})
