context("Test num_pct")

test_that("num to pct", {
    in1 <- c(0.5, 0.3, NA, 1.2)
    out11 <- c("50%", "30%", NA, "120%")
    out12 <- c("0.50%", "0.30%", NA, "1.20%")
    in2 <- list(0.5, list(0.3, NA, list(1.2, NULL)))
    out2 <- list("50%", list("30%", NA, list("120%", NULL)))
    in3 <- matrix(in1, nrow=2)
    out3 <- matrix(out11, nrow=2)
    
    expect_equal(to_pct(in1), out11)
    expect_equal(to_pct(in1, digits=2, value_asis=FALSE), out12)
    expect_equivalent(to_pct(in2), out2)
    expect_equal(to_pct(in3), out3)
})


test_that("char to num", {
    in1 <- matrix(c("_84%", "32.9%", "33", "a", " -5", "0.3e2", "1,200", "$67", NA), nrow=3)
    out11 <- matrix(c(0.84, 0.329, 33, NA, -5, 30, 1200, 67, NA), nrow=3)
    out12 <- matrix(c(NA, 0.329, 33, NA, -5, 30, 1200, 67, NA), nrow=3)
    in2 <- as.list(in1)
    out21 <- as.list(out12)
    
    expect_equal(to_num(in1, strict=FALSE), out11)
    expect_equal(to_num(in1), out12)
    expect_equal(to_num(in2), out21)
})