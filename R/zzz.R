#' aseskit: An R General Analytics Toolkit for ASES
#'
#' An analytic toolkit comprising of a series of generic work functions,
#' e.g., partial evaluation, distribution analysis, package load/unload, GUI,
#' iif, ...
#'
#' @details This package is comprised of \cr \describe{
#'   \item{API helpers}{\code{\link{get_api_data}()}, \code{\link{parse_api_data}()},
#'     \code{\link{synthesize_api}()}, \code{\link{get_api_key}()},
#'     \code{\link{set_api_key}()}}
#'   \item{Password}{\code{\link{input_pwd}()}, \code{\link{get_raw_pwd}()},
#'     \code{\link{decrypt_it}()}, \code{\link{encrypt_it}()}}
#'   \item{Package mgmt}{\code{\link{load_pkgs}()}, \code{\link{unload_pkgs}()},
#'     \code{\link{dep_pkgs}()}, \code{\link{addRtoolsPath}()}}
#'   \item{Vectorized \code{\link{ifelse}}}{\code{\link{iif}()} family}
#'   \item{Format convertion}{\code{\link{to_pct}()}, \code{\link{to_num}()}}
#'   \item{Distribution analysis}{\code{\link{describe_df}()},
#'     \code{\link{cal_df_distrib}()}, \code{\link{vis_df_distrib}()}}
#'   \item{Helpers}{\itemize{
#'     \item nchar: \code{\link{count_char}()}
#'     \item Relabel number range: \code{\link{relab_range}()}
#'     \item Clock-wise position: \code{\link{clock_to_pos}()}
#'     \item Parse clipboard: \code{\link{parse_clipb}()}
#'     \item JS-style operators: \code{\link{\%+=\%}()}
#'     \item Align vectors: \code{\link{aline}()}
#'     \item Enquo arguments: \code{\link{enquo_arg}()}
#'     \item Parse and extract formula: \code{\link{parse_formula}()}
#'   }}
#' }
#' @author \strong{Creator, Maintainer}: Yiying Wang, \email{wangy@@aetna.com}
#'
#' @importFrom magrittr %>%
#' @export %>%
#' @docType package
#' @keywords internal
#' @name aseskit
NULL


.onLoad <- function(libname, pkgname="aseskit"){

    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
    if (Sys.info()[['machine']] == "x64") if (Sys.getenv("JAVA_HOME") != "")
        Sys.setenv(JAVA_HOME="")

	addRtoolsPath()

    # pkgenv is a hidden env under pacakge:aseskit
    # -----------------------------------------------------------
    assign("pkgenv", new.env(), envir=parent.env(environment()))

	# constants for encrpytion
    pkgenv$START_TAG <- paste(
        "\u2016\u251c\u0432\u0259\u03b4\u03b9\u03bd",
        "\u0113\u0418\u03ba\u03c1\u03b9\u03c0\u0442\u03b9\u25cb\u0438\u2524\u2016")
    pkgenv$END_TAG <- paste(
        "\u2016\u251c\u30e8\u03b7\u03b4",
        "\u0113\u0418\u03ba\u03c1\u03b9\u03c0\u0442\u03b9\u25cb\u0438\u2524\u2016")

    # constants for char2num conversion
    pkgenv$CHAR2NUM_SCI <- get_char2num_pattern(c("e", ",", "$", "%"))
    pkgenv$CHAR2NUM_NOSCI <- get_char2num_pattern(c(",", "$", "%"))  # in num_pct.R
    pkgenv$WIDE_NUMCHAR <- structure(
        c(".", "-", "+", "*", "e", "%", as.character(0:9)),
        names=c("\uff0e", "\uff0d", "\uff0b", "\uff45", "\uff05", "\uff0a",
                "\uff10", "\uff11", "\uff12", "\uff13", "\uff14", "\uff15",
                "\uff16", "\uff17", "\uff18", "\uff19"))

    # ----------------------------------------------------------

    # options
    assign('op', options(), envir=pkgenv)
    options(stringsAsFactors=FALSE)

    pkgParam <- .getPkgPara(pkgname)
    toset <- !(names(pkgParam) %in% names(pkgenv$op))
    if (any(toset)) options(pkgParam[toset])
}

.onUnload <- function(libname, pkgname="aseskit"){
    op <- .resetPkgPara(pkgname)
    options(op)

    # clear .pem files in temp files
    tmpfiles <- list.files(tempdir(), pattern="\\.pem$", full.names=TRUE)
    if (length(tmpfiles) > 0) invisible(unlink(tmpfiles))
}


.onAttach <- function(libname, pkgname="aseskit"){
    ver.warn <- ""
    latest.ver <- getOption(pkgname)$`latest.version`
    current.ver <- getOption(pkgname)$version
    if (!is.null(latest.ver) && !is.null(current.ver))
        if (latest.ver > current.ver)
            ver.warn <- paste0("\nThe most up-to-date version of ", pkgname, " is ",
                               latest.ver, ". You are currently using ", current.ver)
    packageStartupMessage(paste("Welcome to", pkgname, current.ver,
                                 ver.warn))
}

