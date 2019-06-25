context("Test generic_pwd")

test_that("get pwd from pem", {
    pemfile <- paste0(Sys.getenv("HOME"), "/acckey.pem")
    setup({
        if (file.exists(pemfile)) unlink(pemfile)
        writeRawToPem("5e262a616263292a")
    })
    pwd <- capture.output(print(get_pwd()))
    
    expect_equal(get_pwd(), structure(as.raw(as.hexmode(c(
        "5e", "26", "2a", "61", "62", "63", "29", "2a"))), 
        class=c("encrypt", "raw")))
    expect_equal(pwd, "[1] \"****************\"")
    
    teardown(unlink(pemfile))
})

test_that("decrypt and encrypt", {
    keystr <- "Abc"
    out1 <- encrypt_it(keystr)
    out2 <- encrypt_it(keystr, IV.char="TestChar")
    
    expect_s3_class(out1, "encrypt")
    expect_equal(decrypt_it(out1), keystr)
    expect_false(identical(decrypt_it(out2), keystr))
    expect_equal(decrypt_it("Abc"), "Abc")
    
})