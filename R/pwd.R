#' Input password key
#'
#' Input a password phrase. It returns nothing unless you assign the output to a
#' variable.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param prompt character, the prompt label of the GUI window.
#' @param caption character, the caption of the GUI window.
#' @param encrypt logical, if the password will be encrypted (a raw vector).
#' Default TRUE.
#' @param gui 'Rstudioapi', or 'Gwidgets', default 'Rstudioapi'. If
#' 'Rstudioapi' and Rstudio version >= 1.1.419, then call
#' \code{\link[rstudioapi]{askForPassword}}, otherwise use a GUI wizard with gWidget2.
#' @param gen_pem logical, indicating whether generate a acckey.pem file to store
#' the cypher key of the password in under \code{\link{tempdir}()}. Default TRUE.
#' @param ... other arguments, \itemize{
#'  \item force: set \code{force} = TRUE to overwrite the .pem file forcefully \cr
#'  \item pem: assign a name to generate the .pem file in \code{dir}. Default
#'    'acckey'.
#' }
#' @return invisible
#'
#' @export
#' @importFrom stringr str_to_title
#'
#' @seealso \code{\link[rstudioapi]{askForPassword}}  \code{\link[rstudioapi]{askForSecret}}
#' You can also use \code{\link{getPwd}} directly
#' @examples
#' \dontrun{
#' input_pwd(prompt="Input the db key", caption="Password")
#' }
#'
inputPwd <- function(prompt=NULL, caption=NULL, encrypt=TRUE,
                     gui=c("Rstudioapi", "Gwidgets"), gen_pem=TRUE, ...) {
    gui <- str_to_title(gui[[1]])
    guiClassObj <- structure(list(), class=match.arg(gui))
    pwd <- guiInputPwd(prompt=prompt, caption=caption, encrypt=encrypt,
                       guiClassObj=guiClassObj, ...)
    if (is.null(pwd)) return(NULL)
    if ("encrypted" %in% class(pwd)) {
        pwd <- encrypt_it(pwd)
        if (class(pwd)[1] != "encrypt") class(pwd) <- c("encrypt", class(pwd))
    }
    if (gen_pem) if (is.raw(pwd))
        invisible(writeRawToPem(pwd, ...))
    return(invisible(pwd))
}

#' @export
#' @rdname inputPwd
input_pwd <- inputPwd

#' @export
print.encrypt <- print.encrypted <- function(x)
    print.default(strrep("*", 16))


#' Get ACC password (in raw) from the acckey.pem file
#'
#' If the acckey.pem file already exists, it extracts the
#' cipher text from it and decrypt it into key string. Otherwise, it will call
#' \code{inputPwd} to enter the password. \cr
#' Note that this function returns \strong{an encrypted raw vector} which needs
#' to be decrypted using \code{\link{decrypt_it}()}.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#'
#' @param pem character, the name of the .pem file. Default 'acckey'.
#' @param overide logical, if overide existing acckey.pem and input password again.
#' Default FALSE.
#' @return invisible (an encrypted raw vector)
#' @export
#' @importFrom jsonlite base64_dec
#'
#' @seealso \code{\link{inputPwd}}  \code{\link{decrypt_it}}
#'
#' @examples
#' \dontrun{
#' qry_acc <- function(dbname, query) {
#'     con <- DBI::dbConnect(odbc::odbc(), .connection_string=paste0(
#'         "Driver={Microsfot Access Driver (*.mdb, *.accdb)};",
#'         "Dbq=", dbname, ";uid=;pwd=", decrypt_it(get_raw_pwd())))
#'     return(DBI::dbGetQuery(con, query))
#' }
#' qry_acc("testdb.accdb", query="select * from table1")
#' }
#'
getRawPwd <- function(pem='acckey', overide=FALSE){
    dir <- tempdir()
    pemfile <- paste0(dir, "/", pem, ".pem")
    if (!overide && file.exists(pemfile)){
        keystr <- readLines(pemfile)
        # remove starting and ending line
        keystr <- keystr[2:(length(keystr)-1)] %>% base64_dec

        # seq_from <- seq(1, nchar(keystr), 2)
        # seq_to <- seq(2, nchar(keystr), 2)
        # if (length(seq_to) < length(seq_from)) seq_to <- c(seq_to, nchar(keystr))

        # keystr <- substring(keystr, seq_from, seq_to)
    }else{
        keystr <- inputPwd(encrypt=TRUE, prompt=paste("Input", pem),
                           pem=pem, gen_pem=TRUE)
    }
    if (is.raw(keystr)) if (class(keystr)[1] != "encrypt")
        class(keystr) <- c("encrypt", class(keystr))

    return(invisible(keystr))
}

#' @export
#' @rdname getRawPwd
get_raw_pwd <- getRawPwd

# ----------encrpyt and decrypt----------

get_cipher_iv <- function(length.out=8){
    # make up IV using options('aseshms.loaded.at')

    stopifnot(is.numeric(length.out) && length.out %in% 2^(0:4))

    loaded <- unclass(as.POSIXlt(getOption("aseskit")$`loaded.at`))
    loaded <- abs(c(
        unlist(loaded[c('sec', 'min', 'hour')]) * 255/60,
        mday=loaded[['mday']] * 255/31,
        mon=loaded[['mon']] * 255/12,
        year=loaded[['year']]))
    # seconds part
    options(scipen=999)
    sec <- substring(as.character(loaded[['sec']]* 10^32), seq(1, 32, 2),
                     seq(2, 33, 2))
    options(scipen=0)

    if (length.out == 1){
        out <- sec[1]
    }else if (length.out <= 6){
        out <- c(sec[1], loaded[2:length.out])
    }else{
        out <- c(rev(sec[1:(length.out-5)]), loaded[2:length(loaded)])
    }

    return(as.integer(unname(out)))
}


builtinKey <- function(){
    # return
    #   a raw vector length 32 (AES256)

    loaded <- get_cipher_iv(16)
    key <- charToRaw(paste(
        Sys.info()[c('sysname', 'machine', 'effective_user')], collapse="+"))
    key <- c(key, as.raw(loaded))
    if (length(key) < 32){
        key <- c(key, as.raw(rep(charToRaw(' '), 32 - length(key) %% 32)))
    }else{
        key <- key[1:32]
    }
    return(key)
}

#' @importFrom digest AES
builtinAES <- function(mode=c('CBC', 'ECB'), IV=NULL){
    stopifnot(is.null(IV) || is.numeric(IV))
    mode <- match.arg(mode)

    if (is.numeric(IV)) stopifnot(length(IV) %in% 2^(0:5) && all(IV>=0 & IV<=255))
    if (mode == 'CBC') if (is.null(IV)) IV <- get_cipher_iv(16)

    aes <- AES(builtinKey(), mode=mode, IV=IV)
    return(aes)

    # # solution #2
    # if (is.null(pkgenv$IV))
    #     pkgenv$IV <<- sample(0:255, 16, replace=TRUE)
    # return(AES(builtinKey(), mode="CBC", IV=pkgenv$IV))
}

writeRawToPem <- function(x, ...){
    UseMethod(".writeRawToPem", x)
}

#' @importFrom jsonlite base64_enc
.writeRawToPem.raw <- function(x, pem='acckey', force=FALSE, dir=tempdir()){
    pemfile <- paste0(dir,'/', pem, '.pem')
    x <- base64_enc(x)
    if (file.exists(pemfile)){
        if (force) {
            unlink(pemfile)
        }else{
            invisible(warning(
                "Not updated: the ", pem, ".pem already exists.\n",
                "Set force=TRUE to overwrite it."))
            return(invisible())
        }
    }
    cat(c(paste0("-----START PRIVATE KEY ", toupper(pem), "-----"),
          x[x != ""],
          paste0("-----END PRIVATE KEY ", toupper(pem), "-----")),
        file=pemfile, sep="\n")
    invisible(message(pem, ".pem generated in ", dir, "."))
}

.writeRawToPem.encrypt <- .writeRawToPem.raw

.writeRawToPem.default <- function(x, pem='acckey', force=FALSE, dir=tempdir()){
    x <- as.character(x)
    .writeRawToPem.raw(x, pem=pem, force=force, dir=dir)
}


# ----encrypt a string----
#' @importFrom magrittr %>%
hex2raw <- function(text) {
    # extracted from example of digest::AES
    stopifnot(is.character(text))
    text <- paste(unlist(text), collapse='')
    if (!all(unlist(strsplit(text, '')) %in% c(0:9, letters[1:6], LETTERS[1:6])))
        stop('text must be hex character(s).')
    vals <- matrix(as.integer(as.hexmode(strsplit(text, "")[[1]])), ncol=2,
                   byrow=TRUE)
    vals <- vals %*% c(16, 1)
    as.raw(vals)
}

raw2text <- function(x){
    stopifnot(is.raw(x))
    return(paste(x, collapse=''))
}

#' @export
#' @examples
#' \dontrun{
#' encrypt_it('my key')
#' }
encryptIt <- function(x, ...){
    UseMethod(".encryptIt", x)
}

#' @export
encrypt_it <- encryptIt

#' @importFrom digest AES
.encryptIt.character <- function(x, mode=c('CBC', 'ECB'), IV=NULL, ...){
    # split x and insert joint signs
    mode <- match.arg(mode)

    x <- enc2native(paste(unlist(strsplit(x, "")), collapse="\uff0b"))
    # concat raw vector
    x <- c(charToRaw(pkgenv$START_TAG), charToRaw(x),
           charToRaw(pkgenv$END_TAG))
    # add spaces
    x <- c(x, rep(charToRaw(' '), 32 - length(x) %% 32))

    encrypter <- builtinAES(mode=mode, IV=IV)
    o <- encrypter$encrypt(x)
    if (is.raw(o)) if (class(o)[1] != "encrypt")
        class(o) <- c("encrypt", class(o))
    return(o)
}

.encryptIt.default <- function(x) {
    .encryptIt.character(as.character(x))
}

#' Encrypt a character and decrypt a raw vector
#'
#' It calls a builtin algorithm to encrypt a character or decrypt a raw vector.
#' If \code{x} is not of the \code{encrypt} class, the value will not be decrypted.
#' @param x the raw vector to decrypt
#' @param mode character, 'ECB" or 'CBC'. Default 'CBC', which is more secure.
#' @param IV NULL or a numeric vector, the initial vector for 'CBC' or initial
#'  counter for 'CTR' mode in \pkg{digest}::\code{\link[digest]{AES}()}. It is
#'  bypassed when \code{mode} == 'ECB'. Default NULL.
#' @param ... other arguments.
#' @return invisible
#' @export
#' @rdname encryptIt
#'
#' @examples
#' \dontrun{
#' decryptIt("Pwd")
#' }
decryptIt <- function(x, ...){
    UseMethod(".decryptIt", x)
}

#' @export
#' @rdname encryptIt
decrypt_it <- decryptIt

#' @export
.decryptIt.default <- function(x, ...) invisible(unclass(x))

#' @export
.decryptIt.encrypt <- function(x, mode=c('CBC', 'ECB'), IV=NULL, ...){
    # key <- unlist(strsplit(x, ""))
    # key <- gsub("-", "01", gsub("\\.", "00", key))
    # key <- rawToChar(packBits(as.integer(key)))
    mode <- match.arg(mode)

    decrypter <- builtinAES(mode=mode, IV=IV)
    o <- raw2text(decrypter$decrypt(x, raw=TRUE))

    start <- raw2text(charToRaw(pkgenv$START_TAG))
    end <- raw2text(charToRaw(pkgenv$END_TAG))
    joint <- raw2text(charToRaw(enc2native("\uff0b")))

    o <- unlist(strsplit(o, paste0(start, "|", end, "|", joint)))
    if (length(o) > 1)
        o <- trimws(o[if (o[1] == '') 2:(length(o)-1) else 1])

    o <- hex2raw(o)
    o <- rawToChar(o[o != '00'])
    invisible(o)
}

#' @export
.decryptIt.plain <- .decryptIt.default
