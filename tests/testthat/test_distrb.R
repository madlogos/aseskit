context("Test distrb")

test_that("parse clipboard", {
    writeClipboard("abc")
    expect_null(parse_clipb())
    
    writeClipboard("a\tb\tc\n1\t2\t3")
    expect_equal(parse_clipb(), tibble(a=1, b=2, c=3))
    expect_equal(parse_clipb(0), tibble(X1=c("a", "1"), X2=c("b", "2"), X3=c("c", "3")))
    expect_equal(suppressWarnings(parse_clipb(0, col_types=readr::cols("i", "i", "i"))),
                 tibble(X1=c(NA, 1L), X2=c(NA, 2L), X3=c(NA, 3L)))
})

test_that("cal distribution", {
    cal_mtcars <- cal_df_distrib(mtcars)
    cal_mtcars_mtx <- cal_df_distrib(as.matrix(mtcars))
    cal_mtcars_part <- cal_df_distrib(mtcars, vars(vs), vars(hp))
    cal_mtcars_null <- cal_df_distrib(mtcars, NULL, NULL)
    
    expect_identical(cal_mtcars, cal_mtcars_mtx)
    expect_equal(dim(cal_mtcars$qual), c(10, 4))
    expect_equal(dim(cal_mtcars$quan), c(11, 11))
    expect_equal(dim(cal_mtcars_part$qual), c(2, 4))
    expect_equal(dim(cal_mtcars_part$quan), c(1, 11))
    expect_equal(cal_mtcars$qual %>% filter(varname=="vs", value==1) %>% ungroup 
                 %>% select(freq) %>% unlist %>% unname, 14)
    expect_equal(cal_mtcars$quan %>% filter(varname=="hp") %>% ungroup %>% 
                 select(sd) %>% unlist %>% unname %>% round(2), 68.56)
    expect_equal(cal_mtcars_null$qual, "No non-numeric columns matched.")
    expect_equal(cal_mtcars_null$quan, "No numeric columns matched.")
})

test_that("vis distribution", {
    setup(Sys.setlocale("LC_CTYPE", "C"))
    vis_mtcars <- vis_df_distrib(mtcars)
    vdiffr::expect_doppelganger("mtcars-quan-distrib", vis_mtcars[[1]])
    vdiffr::expect_doppelganger("mtcars-qual-distrib", vis_mtcars[[2]])
    teardown(Sys.setlocale("LC_CTYPE", "chs"))
})