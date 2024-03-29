#' Get the API key
#'
#' Get the API key(s). It is usually used in a function call chain that makes use
#' of Google or Baidu map APIs, e.g., \code{geocode}.
#' @details If the encrpyted API key(s) have been previously stored in the package
#' hidden environment \code{pkgenv$API_KEY}, the function will extract it after an
#' auto decryption. If not previously stored in \code{pkgenv}, a GUI wizard
#' (\code{inputChar}) will be launched for you to enter the key(s). \cr
#' When you encounter an API error, please either \itemize{
#' \item set \code{overide=TRUE} to re-enter the key, or \cr
#' \item directly call \code{set_api_key} to do so. Each time you load \pkg{aseshms},
#'  you only need to set the API keys once.
#' }
#'
#' @note Do not disclose the key to anyone as it may cause data leakage.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param app character vector of the application name for the API, either 'google',
#' 'baidu', 'gaode', 'ipstack', 'ipinfo' or 'ipify'.
#' @param overide logical, whether flush the stored API key and call a GUI wizard
#' to enter a new one. Default FALSE to enable reusing the keys.
#' @param ... other arguments. For instance, \describe{
#' \item{Args of \code{\link{input_char}()} to call a GUI wizard for input}{
#'  \itemize{
#'   \item prompt: prompt label of the GUI frame \cr
#'   \item caption: caption of the GUI window \cr
#'   \item default: default value for the input box \cr
#'   \item gui: 'Rstudioapi', 'Gwidgets' or 'WinGui'.
#'  }}
#' }
#' @param drop logical, whether coerce the result to a named vector. Default TRUE.
#' If FALSE, you will get a named list instead.
#'
#' @return invisible. You must assign it to an object.
#' @export
#' @importFrom jsonlite base64_dec base64_enc
#' @aliases get_api_key
#'
#' @seealso \code{\link{input_char}()}, \code{\link{get_raw_pwd}()}
#' @examples
#' \dontrun{
#' get_api_key('google', gui='WinGui')
#' get_api_key()  # you will get all the api keys
#'
#' # if the api keys have been written to .pem files
#' set_api_key('baidu', <YOUR BAIDU API KEY>, gen_pem=TRUE)
#' key <- decrypt_it(get_raw_pwd(pem='baidu'))
#' }
getApiKey <- function(app=c("google", "baidu", "gaode", "ipstack",
                            "ipify", "ipinfo"),
                      overide=FALSE, drop=TRUE, ...){
    # check args
    app <- match.arg(app, several.ok=TRUE)
    stopifnot(is.logical(overide))
    stopifnot(is.logical(drop))

    keys <- pkgenv$API_KEY
    null_key <- vapply(app, function(ap) is.null(keys[[ap]]),
                       FUN.VALUE=logical(1L))
    # input key
    if (overide){
        setApiKey(app, ...)
    }else if (any(null_key)){
        setApiKey(app[null_key], ...)
    }
    key <- lapply(app, function(ap){
        structure(base64_dec(pkgenv$API_KEY[[ap]]), class='encrypt') %>%
        decrypt_it})
    names(key) <- app
    if (drop) key <- unlist(key)

    return(invisible(key))
}

#' @export
#' @rdname getApiKey
get_api_key <- getApiKey


#' @param key character vector or list, value(s) of the API key(s). You can directly
#' assign corresponding values here. Default NULL (gui mode), indicating that a
#' GUI wizard will be launched for you to enter the API key respectively.
#' Generally, gui mode is more secure.
#' @param gen_pem logical, whether generate .pem files in the \code{\link{tempdir}()}.
#' Default FALSE.
#' @aliases set_api_key
#' @export
#' @rdname getApiKey
#' @examples
#' \dontrun{
#' #' key <- <YOUR BAIDU API KEY>  # not recommended: keys are exposed
#' setApiKey('baidu', key=key)
#' setApiKey('baidu', key=NULL)  # recommended
#'
#' # set multiple API keys
#' set_api_key(app=c("google", "baidu", "gaode", "ipstack", "ipinfo", "ipify"),
#'             key=<THE CORRESPONDING KEY CHARACTER VECTOR>)  # not recommended
#' set_api_key()  # pop up a series of GUI windows to input all the API keys
#' }
setApiKey <- function(app=c("google", "baidu", "gaode", "ipstack",
                            "ipify", "ipinfo"), key=NULL, gen_pem=FALSE, ...){
    app <- match.arg(app, several.ok=TRUE)
    stopifnot(is.null(key) || is.character(key))
    stopifnot(is.logical(gen_pem))

    if (is.null(pkgenv$API_KEY)) pkgenv$API_KEY <- list()
    keys <- pkgenv$API_KEY

    if (! is.null(key)){  # input key not gui
        stopifnot(length(app) == length(key))
        key <- ifna(key, '')
        # key nchar valid?
        invalid_key <- nchar(key) == 0
        if (any(invalid_key))
            stop("Invalid key value for ", paste(app[invalid_key], collapse=', '),
                 ". Empty keys are not accepted.")
        pkgenv$API_KEY[app] <- lapply(key, function(k)
            base64_enc(encrypt_it(k)))
    }else{
        # input key gui
        invisible(lapply(seq_along(app), function(i){
            k <- input_char(paste("Input your", app[[i]], "API key. \n",
                                  "DO NOT disclose it to anyone!"),
                            caption=paste("API Key for", app[[i]]), ...)
            if (is.null(k)){
                stop("You did not input any characters for ", app[[i]], " API.")
            }else if (nchar(k) > 0){
                pkgenv$API_KEY[[app[[i]]]] <<- base64_enc(encrypt_it(k))
            }else{
                stop("The key for ", app[[i]],
                     "is not accepted: should not be an empty character.")
            }
        }))
    }
    if (gen_pem){
        invisible(lapply(app, function(app_nm){
            writeRawToPem(encrypt_it(get_api_key(app_nm)), pem=app_nm)
        }))

    }
    invisible()
}


#' @export
#' @rdname getApiKey
set_api_key <- setApiKey
