# ------------Read API------------------

#' Get data from given API URL(s)
#'
#' @param api_urls character list comprising of API URL lists that are of 'url' class.
#' If a vector is given, the function will wrap it into a list.
#' @param use_curl logical, whether use \code{curl}. Default TRUE. You can use
#' \code{\link{check_curl}()} to check if curl is available for the api_urls before
#' you begin to read data.
#' @param parse_json logical, whether parse the output JSON character to R data.
#'  If TRUE, \code{\link[jsonlite]{fromJSON}()} will be called to parse the character.
#'  Default TRUE.
#' @param time numeric, the time interval to access the api_urls, in seconds.
#'  It is used to avoid overuse of the APIs. Default 0.
#' @param encoding character, the encoding of the API. Default NULL, indicating
#' that the function will automatically select an encoding for specific APIs.
#' The accepted 'encoding' is consistent with that in \code{\link{readLines}()}.
#' You can set it to 'unknown', while in some cases, you need to assign a valid
#' encoding value, e.g., 'UTF-8'.
#' @param messaging logical, whether print messages when processing. Default FALSE.
#' @param name_out character vector, the names of the output list. Default is set to
#' \code{api_url}'s 'name_out' attribute. If NULL, the output will have no names.
#' @param drop logical, whether unlist the result when there is only one list returned.
#' Default FALSE, which indicates that you will need to extract the result first.
#' Note that if you set \code{drop} = TRUE, then the result will be \code{unclass}ed.
#' @param ... other arguments to pass to the function.
#'
#' @return a list comprising of 'api_data' objects. The 'api_data' class includes
#' several useful subclasses, e.g. 'google_geocode', 'google_revgeocode', 'baidu_geocode',
#' 'baidu_revgeocode', 'gaode_geocode', 'gaode_revgeocode', and 'ipify_geohost',
#' 'ipinfo_geohost', 'ipstack_geohost', ... in \pkg{\link{asesgeo}}.
#' \itemize{
#'  \item parse_json == FALSE: a list containing JSON strings
#'  \item parse_json == TRUE: a list containing parsed data
#' }
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @export
#'
#' @examples
#' \dontrun{
#' api_urls <- c('<API 1>', '<API 2>', ...)
#' get_api_data(api_urls)
#' }
get_api_data <- function(api_urls, use_curl=TRUE, parse_json=TRUE, time=0,
                         encoding=NULL, messaging=FALSE,
                         name_out=vapply(api_urls, attr, which='name_out',
                                         FUN.VALUE=character(1L)),
                         drop=FALSE,
...){
    # loop over api_urls and read the data

    # check args
    stopifnot(is.logical(use_curl))
    stopifnot(is.logical(parse_json))
    stopifnot(is.numeric(time))
    stopifnot(is.null(encoding) || is.character(encoding))
    stopifnot(is.null(name_out) || is.character(name_out))
    if (! is.list(api_urls)) {
        if (! inherits(api_urls, 'url')) {
            stop('api_urls must be of "url" class.')
        }else{
            api_urls <- unlist(api_urls) %>% as.vector %>% unname %>%
                lapply(structure, class=class(api_urls))
        }
    }

    # msg
    if (messaging) {
        head_msg <- paste0('calling ', length(api_urls), ' APIs: ', gsub(
            "([Kk]ey|ak)=([^\\?&]+)", "\\1=*********", api_urls[1]), ' ... ')
        invisible(message(head_msg, appendLF = FALSE))
    }

    # result
    if (! is.null(name_out)) names(api_urls) <- name_out
    opts <- list(use_curl=use_curl, parse_json=parse_json, time=time, ...)
    if (! is.null(encoding)) opt$encoding <- encoding

    o <- mapply(read_api, api_url=api_urls, name_out=name_out, MoreArgs=opts,
                SIMPLIFY=FALSE)

    if (messaging) invisible(message('done.'))

    # drop the list if is length 1?
    if (length(o) == 1) if (drop) o <- unlist(o, recursive=FALSE)

    return(o)
}

#' General function to parse 'api_data' objects
#'
#' It is a generic function to parse 'api_data' object ('xxx_geocode', 'xxx_revgeocode',
#' or 'xxx_geohost'). You can mix 'api_data' objects of different subclasses for
#' parsing, while the three 'api_data' parsers mentioned in 'seealso' can only parse
#' objects of the same subclass.
#'
#' @param x 'api_data' objects, typically lists yieled by \code{\link{get_api_data}()}.
#' @param ... other arguments to pass to the function. Refer to the functions in
#' 'seealso' to know more about available optional arguments.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @return a list comprising of \code{\link[dplyr]{tibble}}s. Since the 'api_data'
#' objects may be of different subclasses, it is not reasonable to coerce the
#' results into a single data.frame. If the mixed parsing fails somewhere, you
#' will not get any result.
#' @export
#'
#' @seealso \code{\link[asesgeo]{parse_geohosts}()}, \code{\link[asesgeo]{parse_geocodes}()} and
#' \code{\link[asesgeo]{parse_revgeocodes}()}
#'
#' @examples
#' \dontrun{
#' # get a bunch of 'api_data' objects
#' library(asesgeo)
#' x1 <- geohost('<ip1>', output='raw')
#' x2 <- geocode('<addr1>', output='raw')
#' x3 <- revgeocode('<latlng1>', output='raw')
#'
#' parse_api_data(c(x1, x2, x3))
#' }
#'
parse_api_data <- function(x, ...){
    lapply(x, .parse_api_data, ...)
}

.parse_api_data <- function(x, ...){
    stopifnot(inherits(x, 'api_data'))
    UseMethod('.parse_api_data')
}

.parse_api_data.default <- function(x, ...){
    stop('x must be of a valid subclass of "api_data" class.')
}

# ==============Read API for data============

#' @importFrom curl curl
#' @importFrom jsonlite fromJSON
extract_api_data <- function(api_url, use_curl=TRUE, parse_json=TRUE,
                             time=0, encoding='unknown', url_encode=TRUE,
                             name_out=NULL, ...){
    # generic working function to read raw string from api_url
    # return a list with subclass google_geocode, google_revgeocode, ...
    # of the class 'api_data'
    if (is.na(api_url)){  # if synthesize_api gets NA, then return a list()
        o <- list()
    }else{
        use_curl <- check_curl(api_url, use_curl=use_curl)
        if (use_curl) {
            con <- if (url_encode) curl(URLencode(api_url)) else curl(api_url)
        }else{
            con <- if (url_encode) url(URLencode(api_url)) else url(api_url)
        }
        name_out <- ifnull(name_out, attr(api_url, 'name_out'))
        o <- paste(readLines(con, warn = FALSE, encoding = encoding))
        close(con)
        if (parse_json) o <- tryCatch(fromJSON(o), error=function(e) o)
        if (time > 0) Sys.sleep(time)
    }
    o_cls <- c(sub('_api', '', class(api_url)[[1]]), 'api_data')
    return(structure(o, class=o_cls, name_out=as.vector(name_out)))
}

read_api <- function(api_url, ...){
    stopifnot(inherits(api_url, 'url'))
    UseMethod(".read_api", api_url)
}

.read_api.default <- function(
    # default method: use curl and encoding is 'unknown'.
    api_url, use_curl=TRUE, parse_json=TRUE, time=0, encoding='unknown',
    url_encode=TRUE, name_out=NULL, ...){

    if (inherits(api_url, 'url')){
        return(extract_api_data(
            api_url=api_url, use_curl=use_curl, parse_json=TRUE, time=time,
            encoding=encoding, url_encode=url_encode, name_out=name_out, ...))
    }else{
        stop('read_api fails. api_url must be of a subclass of "url" class, ',
             'e.g., xxx_geocode, xxx_revgeocode, xxx_geohost, xxx_getip, ....')
    }
}


# ============Synthesize API=================

#' Synthesize API URLs
#'
#' A wrapper function to synthesize API URL(s) for extraction of data. You can then
#' use \code{\link{get_api_data}()} to extract the data.
#' @param url_body character, the main body of the caller API.
#' @param provider character, 'google', 'baidu', 'gaode', 'ipstack', 'ipinfo', or 'ipify'
#' @param api character, 'geocode', 'revgeocode', 'convcoord', or 'geohost'
#' @param ... other arguments to pass to the function
#'
#' @return A list of API URLs, each of which is a \code{url} object.
#' @export
#'
#' @examples
#' \dontrun{
#' synthesize_apis('https://maps.googleapis.com/maps/api/', 'google', 'geocode')
#' }
#'
synthesize_apis <- function(
    url_body, provider=c('google', 'baidu', 'gaode', 'ipstack', 'ipinfo', 'ipify'),
    api=c('geocode', 'revgeocode', 'convcoord', 'geohost'), ...){
    # generic function

    provider <- match.arg(provider)
    api <- match.arg(api)

    fun_dict <- list(google = asesgeo:::synthesize_google_api,
                     baidu  = asesgeo:::synthesize_baidu_api,
                     gaode  = asesgeo:::synthesize_gaode_api,
                     ipstack= asesgeo:::synthesize_ipstack_api,
                     ipify  = asesgeo:::synthesize_ipify_api,
                     ipinfo = asesgeo:::synthesize_ipinfo_api)
    fun <- fun_dict[[provider]]
    fun(url_body=url_body, api=api, ...)
}

#' @export
#' @rdname synthesize_apis
synthesize_api <- synthesize_apis

# ================Check if curl can be used=========================

#' Check if curl is avaiable for given API URL(s)
#'
#' @param url character vector of API URLs
#' @param app_name character vector of API app name. If missing, \code{url} will be
#' used as the name(s)
#' @param use_curl logical, whether to use curl. The function will check the url(s)
#' only when use_curl = TRUE. Default TRUE.
#' @param messaging logical, whether print messages while processing. Default FALSE.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#'
#' @return a logical vector corresponding to url.
#'
#' @importFrom curl curl_fetch_memory
#' @export
#' @examples
#' \dontrun{
#' check_curl(c("http://www.baidu.com",'https://www.google.com'))  # returns
#'
#' # www.baidu.com www.google.com
#' #         FALSE           TRUE
#' }
check_curl <- function(url, app_name=gsub('^[^/]+//(.*)$', '\\1', url),
                       use_curl=TRUE, messaging=FALSE){
    # check if curl can access the url
    stopifnot(is.logical(use_curl))
    stopifnot(is.logical(messaging))
    stopifnot(is.character(url))
    stopifnot(is.character(app_name))

    app_name <- aline(as.vector(app_name), length(url))
    names(url) <- app_name
    if (use_curl){
        use_curl <- vapply(url, function(url_elem) {
            tryCatch(is.numeric(curl_fetch_memory(url_elem)[['status_code']]),
                     error=function(e) FALSE)
        }, FUN.VALUE=logical(1L))
        if (messaging)
            if (! all(use_curl)) invisible(warning(
                '`curl` does not work for ', paste(app_name[! use_curl], collapse=', '),
                '. use_curl has been set to FALSE.'))
    }
    return(use_curl)
}

# ============Check IP address===============
validate_ip <- function(ip, ipv=c(4, 6), messaging=FALSE){
    # validate ip v4 | v6, return logical values
    ipv <- as.numeric(ipv)
    stopifnot(all(ipv %in% c(4, 6)))

    # identify ipv4 and ipv6
    ipv4_regex <- '^((25[0-5]|2[0-4]\\d|[01]?\\d\\d?)\\.){3}(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)$'
    ipv6_regex <- '^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$'

    valid_ipv4 <- if (4 %in% ipv) grepl(ipv4_regex, ip) else rep(FALSE, length(ip))
    valid_ipv6 <- if (6 %in% ipv) grepl(ipv6_regex, ip) else rep(FALSE, length(ip))
    valid_ip <- valid_ipv4 | valid_ipv6

    if (! all(valid_ip)) if (messaging)
        invisible(warning("Not all the IP addresses are valid."))
    return(valid_ip)
}
