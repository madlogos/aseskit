context("Test nchar")

test_that("get_nchar with different classes",{
    in1 <- matrix(c("dws", 23, "2332", 1), ncol=2)
    out1 <- matrix(c(3, 2, 4, 1), ncol=2)
    in2 <- list(list("dws", "", c("2332", list(44, list(1)))))
    out2 <- list(list(3, as.numeric(NA), c(4, list(2, list(1)))))
    
    expect_equal(get_nchar(c("dws", 23)), c(3, 2))
    expect_equal(get_nchar(in1), out1)
    expect_equal(get_nchar(in2), out2)
    
    expect_equal(get_nchar("\u{2264}", type="byte"), 3)
    expect_equal(get_nchar(c("abc", ""), rm.blank=FALSE), c(3, 0))
})