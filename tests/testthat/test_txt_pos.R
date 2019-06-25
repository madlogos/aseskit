context("Test txt_pos")

test_that("clock_to_pos", {
    named_o <- structure(c("right", "top", "vertical"), 
                         names=c("horizontal", "vertical", "direction"))
    undroped_o <- matrix(c("right", "top", "vertical"), nrow=1)
    
    expect_equal(clock_to_pos(2L), c("right", "top", "vertical"))
    expect_equal(clock_to_pos(2.7), c("right", "top", "vertical"))
    expect_error(clock_to_pos("2"), "all\\(is\\.numeric\\(pos\\)\\)")
    
    expect_equal(clock_to_pos(2, unname=FALSE), named_o)
    expect_equal(clock_to_pos(2, drop=FALSE), undroped_o)
})