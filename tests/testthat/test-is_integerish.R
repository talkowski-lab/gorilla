test_that("zero as a double is integerish", {
    expect_true(is_integerish(0))
})

test_that("zero as an integer is integerish", {
    expect_true(is_integerish(0L))
})

test_that("a non-vector object is not integerish", {
    expect_false(is_integerish(factor(c("foo", "bar", "bar"))))
})

test_that("infinity is not integerish", {
    expect_false(is_integerish(c(2, Inf)))
})

test_that("a double with only zeros after the decimal is integerish", {
    expect_true(is_integerish(1.0))
})

test_that("a double strictly between two integers is not integerish", {
    expect_false(is_integerish(1.000009))
})
