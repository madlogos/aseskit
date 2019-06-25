context("Test generic_pkg")

test_that("load and unload packages", {
    loadPkgs(car)
    expect_true("car" %in% loadedNamespaces())
    expect_message(unloadPkgs(car, quiet=FALSE), "Detach")
    
    loadPkgs(car, cluster)
    expect_true(all(c("car", "cluster") %in% loadedNamespaces()))
    unloadPkgs(car, cluster)
    
    loadPkgs("car", "cluster")
    expect_true(all(c("car", "cluster") %in% loadedNamespaces()))
    unloadPkgs(car, cluster)
    
    loadPkgs(c("car", "cluster"))
    expect_true(all(c("car", "cluster") %in% loadedNamespaces()))
    unloadPkgs(car, cluster)
    
    expect_error(loadPkgs(not_exist, repo="w"), "'arg' should be one of")
})
