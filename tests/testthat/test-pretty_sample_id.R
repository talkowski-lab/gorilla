test_that("an ID fewer than 30 characters is not truncated", {
    before <- "short_id"
    after <- pretty_sample_id(before)
    expect_equal(before, after)
})

test_that("an ID longer than 30 characters is truncated", {
    before <- "this_is_a_very_long_id_that_will_should_be_truncated"
    after <- pretty_sample_id(before)
    expect_equal(after, "this_is_a_very_long_id_that_wi...")
})
