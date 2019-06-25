context("Test formula_vars")

test_that("parse formula", {
    out1 <- c(x="x + m", y="y + z", series="d", weight="w")
    out2 <- c(x=".", y="y", series=NA, weight=NA)
    
    expect_equal(parse_formula("y+z~x*w+m|d"), out1)
    expect_equal(parse_formula(y+z~x*w+m|d), out1)
    expect_equal(parse_formula(y+z~x+m*w|d), out1)
    expect_equal(suppressWarnings(parse_formula(y+z~x+m*w|d)), out1)
    expect_warning(parse_formula(y+z~x*ww|d+m*w), "Only to extract the last")
    expect_equal(parse_formula("y~."), out2)
    expect_null(parse_formula(NULL))
    expect_equal(parse_formula(y, x, s, wgt, z), c(x="y", y="x", series="s", weight="wgt"))
})