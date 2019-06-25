context("Test js_ops")

test_that("%+=%", {
    x <- 1
    expect_equal({x %+=% 4; x}, 5)
    expect_equal({x %-=% 2; x}, 3)
    expect_equal({x %*=% 5; x}, 15)
    expect_equal({x %/=% 3; x}, 5)
    expect_equal({x %mod=% 3; x}, 2)
    expect_equal({x %^=% 3; x}, 8)
    expect_equal({x %root=% 2; x}, sqrt(8))
})