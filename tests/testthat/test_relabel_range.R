context("Test relab_range")

test_that("relab_range with quick forms", {
    x <-  levels(cut(1:100, c(0, 20, 40, 60, 80, 100)))
    x_bar1 <- c("0-20", "20-40", "40-60", "60-80", "80-100")
    x_closeopen <- c("[0, 20)", "[20, 40)", "[40, 60)", "[60, 80)", "[80, 100)")
    x_bar2 <- c("\u{2264}20", "21-40", "41-60", "61-80", ">81")
    
    expect_equal(relab_range(x, "-"), x_bar1)
    expect_equal(relab_range(x, "[)"), x_closeopen)
    expect_equal(relab_range(x, "<=", left.subtract=-1), x_bar2)
    
    expect_warning(relab_range(c("(0,1]", "(0,1]", "(0,1]")), "duplicated")
    expect_error(relab_range(1:4), "yielded by cut\\(\\)")
})


test_that("relab_range with user defined format", {
    x <-  levels(cut(1:100, c(0, 20, 40, 60, 80, 100)))
    x1 <-  c("0-_20", "20-_40", "40-_60", "60-_80", "80-_100")
    x2 <- c(" ~ 20", "20 ~ 40", "40 ~ 60", "60 ~ 80", "80 ~ ")
    
    expect_equal(relab_range(x, format=c("x-_y")), x1)
    expect_equal(relab_range(x, format=c(" ~ y", "x ~ y", "x ~ ")), x2)
    
    expect_error(relab_range(x, format=c("**")), "format is not properly defined")
})