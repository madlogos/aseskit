context("Test iif")

test_that("iif with different class", {
    in1 <- c(1, -2, NA, pi)
    out1 <- c(1, 0, NA, pi)
    in2 <- matrix(in1, nrow=1)
    out2 <- matrix(out1, nrow=1)
    in3 <- list(list(1, list(-2)), pi, NULL)
    out3 <- list(list(1, list(0)), pi, NULL)
    in4 <- data.frame(A=1:3, B=c(pi, NA, -2))
    out4 <- data.frame(A=1:3, B=c(pi, NA, 0))
    in5 <- matrix(in1, nrow=2)
    out5 <- matrix(out2, nrow=2)
    criteria_fun <-  function(x) x<0
    
    expect_equal(iif(in1, 0, criteria_fun), out1)
    expect_equal(iif(in2, 0, criteria_fun), out2)
    expect_equal(iif(rbind(in2, in2), 0, criteria_fun), rbind(out2, out2))
    expect_equal(iif(in3, 0, criteria_fun), out3)
    expect_equal(iif(in4, 0, criteria_fun), out4)
    expect_equal(iif(in5, 0, criteria_fun), out5)
    
    expect_error(iif(1:5, NA, mean), "function that yields logical")
})

test_that("iif helper functions", {
    in1 <- c(1, -2, NA, pi)
    out1 <- c(1, -2, 0, pi)
    in2 <- matrix(in1, nrow=1)
    out2 <- matrix(out1, nrow=1)
    in3 <- list(list(1, list(-2)), pi, NULL)
    out3 <- list(list(1, list(-2)), pi, 0)
    in4 <- data.frame(A=1:3, B=c(pi, 0, -2))
    out4 <- data.frame(A=1:3, B=c(pi, NA, -2))
    
    expect_equal(ifna(in1, 0), out1)
    expect_equal(ifna(in2, 0), out2)
    expect_equal(ifnull(in3, 0), out3)
    expect_equal(ifzero(in4, NA), out4)
})