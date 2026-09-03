test_that("zero as a double is a number", {
    expect_true(is_number(0))
})

test_that("zero as an integer is a number", {
    expect_true(is_number(0L))
})

test_that("a vector with more than one element is not a number", {
    expect_false(is_number(c(0, 1)))
})
