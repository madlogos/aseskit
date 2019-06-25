context("Test dep_pkg")

test_that("dep_pkgs", {
    skip("Too time consuming. manually test it when necessary.")
    expect_equal(colnames(dep_pkg(MASS, kernlab)), c("pkg", "pkg_type", "dep_pkg", "dep_ver"))
    expect_equal(dep_pkg(MASS) %>% filter(pkg_type=="Imports") %>% dplyr::select(dep_pkg) 
                 %>% unlist, c(dep_pkg="methods"))
    expect_warning(dep_pkg(Not_Available, MASS), "not matched in")
    expect_warning(dep_pkg(Not_Available), "cannot be found in")
    expect_equal(suppressWarnings(dep_pkg(Not_Available, MASS)), dep_pkg(MASS))
    expect_true(exists("avpkg", envir=aseshms_env))
    expect_equal(colnames(aseshms_env$avpkg),
                 c("Package", "Version", "Priority", "Depends", "Imports", 
                   "LinkingTo", "Suggests", "Enhances", "License", "License_is_FOSS",
                   "License_restricts_use", "OS_type", "Archs", "MD5sum", 
                   "NeedsCompilation", "File", "Repository"))
    expect_equal(names(attributes(aseshms_env$avpkg)), c("dim", "dimnames", "updated"))
})