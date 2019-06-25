
# ------------widget list------------
widgetList <- function(...) {
    as.widgetList(list(...))
}

as.widgetList <- function(x) {
    if (! is.list(x)) stop("'x' must be a list")
    #       for (i in seq_along(x)) {
    #             if (!inherits(x[[i]], c('htmlwidget', 'html', 'shiny.tag', 'shiny.tag.list')))
    #                   stop("The element ", i, " in 'x' is not an HTML widget or tag")
    #       }
    structure(x, class = 'widgetList')
}

#' @importFrom knitr knit_print
knit_print.widgetList <- function(x, ..., options = NULL) {
    structure(lapply(x, knit_print, ..., options = options), class = 'knit_asis_list')
}

##----------pre-resiquite functions---------
evalFormula = function(x, data) { # by yihui xie
    if (!inherits(x, 'formula')) return(x)
    if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
    eval(x[[2]], data, environment(x))
}
mergeList = function(x, y, keep_null=FALSE) { # by yihui xie
    if (!is.list(y) || length(y) == 0) return(x)
    yn <-  names(y)
    if (length(yn) == 0 || any(yn == '')) {
        warning('The second list to be merged into the first must be named')
        return(x)
    }
    for (i in yn) {
        xi <-  x[[i]]
        yi <-  y[[i]]
        if (is.list(xi)) {
            if (is.list(yi)) x[[i]] <-  mergeList(xi, yi)
        } else {
            if (length(yi) == 0 && keep_null) x[i] <- list(NULL) else
                x[[i]] <-  yi
        }
    }
    return(x)
}


is.DateChar <- function(x, format=NULL){
    if (length(x) > 1) warning("Only the 1st element will be used.")
    x <- x[[1]]
    if (is.null(x)) return(FALSE)
    if (is.na(x)) return(NA)
    if (is.null(format)) {
        x <- try(as.Date(x), silent=TRUE)
    }else{
        x <- try(as.Date(x, format=format), silent=TRUE)
    }
    return(is(x, "Date"))
}

is.TimeChar <- function(x, origin=NULL, tz=""){
    if (length(x) > 1) warning("Only the 1st element will be used.")
    x <- x[[1]]
    if (is.null(x)) return(FALSE)
    if (is.na(x)) return(NA)
    if (is.null(origin)){
        x <- try(as.POSIXlt(x, tz=tz), silent=TRUE)
    }else{
        x <- try(as.POSIXlt(x, origin=origin, tz=tz), silent=TRUE)
    }
    return(is(x, "POSIXt"))
}

#' @importFrom stringi stri_enc_isascii
isLatin <- function(x){
    if (is.factor(x)) x <- as.character(x)
    return(stri_enc_isascii(x))
}

.getPkgPara <- function(thispkg){
    is_aetna <- .isAetnaBuild()

    if (is_aetna){
        init.dir <- "//ship-oa-001/China_Health_Advisory/Analytics/"
        toolkit.dir <- "//ship-oa-001/China_Health_Advisory/Analytics/GUIDE And TOOLS/"
        remote.pkg.dir <- "//ship-oa-001/China_Health_Advisory/Analytics/R_scripts/"
        init.pal <- "aetnagreen"
    } else {
        init.dir <- Sys.getenv("HOME")
        toolkit.dir <- Sys.getenv("HOME")
        remote.pkg.dir <- NULL
        init.pal <- NULL  # default ggplot2 theme
    }
    na.string <- c(cn="\u{4E0D}\u{8BE6}", en="NA")
    mach.arch <- switch(Sys.info()[['machine']], `x86-32` = '32', `x86-64` = '64')
    pkgversion <- tryCatch(
        packageVersion(thispkg, lib.loc=dirname(system.file(package=thispkg))),
        error=function(e) NULL, finally=invisible())
    lastver <- .checkRemoteVer(thispkg)
    return(structure(
        list(init.dir, toolkit.dir, init.pal, na.string, mach.arch, remote.pkg.dir,
             'tcltk', list(version=pkgversion, `latest.version`=lastver,
                           loaded.at=Sys.time())),
        names=c("init.dir", "toolkit.dir", "init.pal", "na.string",
                "mach.arch", "remote.pkg.dir", "guiToolkit", thispkg))
    )
}

.resetPkgPara <- function(thispkg){
    structure(
        list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
        names=c("init.dir", "toolkit.dir", "init.pal", "na.string",
                "mach.arch", "remote.pkg.dir", "guiToolkit", thispkg))
}

.checkRemoteVer <- function(
    thispkg,
    remote.pkg.dir="//ship-oa-001/China_Health_Advisory/Analytics/R_scripts/"){

    if (!is.null(remote.pkg.dir)){
        files.remote <- list.files(remote.pkg.dir[[1]])
        latest.ver <- try(sub(
			paste0("^", thispkg, "_(\\d.+)\\.zip$"), "\\1",
			files.remote[grep(paste0("^", thispkg, ".+\\.zip"), files.remote)]),
			silent=TRUE)
        if (inherits(latest.ver, "try-error")) latest.ver <- NULL
        if (length(latest.ver) == 0) latest.ver <- NULL
    }else{
        latest.ver <- NULL
    }
	return(latest.ver)
}

.isAetnaBuild <- function(){
    if (Sys.info()['sysname'] == "Windows"){
        if (grepl("^>= 8|^10", Sys.info()['release'])){
            return(any(Sys.getenv() %in% c("AETH", "AETH.AETNA.COM")))
        }else if (grepl("^7", Sys.info()['release'])){
            return(any(names(Sys.getenv()) %in% c("Aetna_Build_Info")))
        }else{
            return(FALSE)
        }
    }else if (Sys.info()['sysname'] == "Darwin"){
        return(FALSE)
    }else{
        return(FALSE)
    }
}


##-------------enquo arguments of a function----------------------

#' Enquo function argument(s) to quosure
#'
#' This function can be used inside a function to \code{\link[rlang]{enquo}()} an
#' argument to a \code{\link[rlang]{quosure}}. By this means, you can work with
#' non-standard evaluation functions (e.g., \pkg{ggplot2}, \pkg{dplyr}) with ease. \cr
#' You can feed character, symbol, name or formula to the function. But if a value
#' instead of an expression is given, quasi-quotation will not take effect, and
#' thus, NULL will be returned.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param ... argument list, either one or a list of character (\code{"x"}),
#' name (\code{x}) or formula (\code{~x}).
#' @param datamask environment for \code{\link[rlang]{eval_tidy}()} to parse.
#' Default NULL. You are encouraged to pass the mask ennvironment using
#' \code{\link[rlang]{as_data_mask}()}.
#' @param named logical, whether name the \code{enquo}-ed dots. Default FALSE.
#' @param flatten_mono logical, whether break the structure and only retain the
#' first element of the output list when there is only one argument passed in.
#' If has no effect when there are multiple arguments. \itemize{
#' \item if \code{enquo_arg} is called, default TRUE \cr
#' \item if \code{enquo_args} is called, default FALSE
#' }
#' @export
#' @importFrom rlang quos eval_tidy caller_env quo_text is_formula as_quosure
#' @seealso Quasi-quotation in \pkg{\link{rlang}}
#' @return A quosure (\code{...} contains only one argument) or a list of quosure
#' (\code{...} contains multiple arguments), depending on the length of argument
#'  list and \code{flatten_mono}.
#' @examples
#' \dontrun{
#' enquo_arg(a)  # returns a single quosure
#' # <quosure>
#' # expr: ^a
#' # env:  empty
#'
#' enquo_args(a)  # returns a list of single quosure
#' # [[1]]
#' # <quosure>
#' # expr: ^a
#' # env:  empty
#'
#' enquo_arg(a, b)  # retuns a quosure list, equal to enquo_args(a, b)
#' # [[1]]
#' # <quosure>
#' # expr: ^a
#' # env:  empty
#' # [[2]]
#' # <quosure>
#' # expr: ^b
#' # env:  empty
#'
#' enquo_arg(6)  # returns NULL
#'
#' # ------ A real-world use case -------
#' library(dplyr)
#' fun <- function(data, by, on){
#'     by <- enquo_arg(by)
#'     on <- enquo_arg(on)
#'     data %>% group_by(!! by) %>%
#'         summarise(avg=mean(!! on, na.rm=TRUE), sd=sd(!! on, na.rm=TRUE))
#' }
#' fun(mtcars, am, mpg)  # yields
#' ##  A tibble: 2 x 3
#' #      am   avg    sd
#' #   <dbl> <dbl> <dbl>
#' # 1     0  17.1  3.83
#' # 2     1  24.4  6.17
#'
#' # can also be written as
#' fun(mtcars, 'am', 'mpg')  # or
#' fun(mtcars, ~am, ~mpg)
#'
#' # Without enquo_arg, fun will encounter an "object not found" error
#' # due to non-standard evaluation.
#' }
#'
enquo_arg <- function(..., datamask=NULL, named=FALSE,
                      flatten_mono=match.call()[1L]=="enquo_arg()") {
    if (! (is.null(datamask) || is.environment(datamask))){
        stop("datamask should be either NULL or an environment. ",
             "You are encouraged to apply rlang::as_data_mask to yield proper datamask.")
    }
    dots <- quos(..., .named=named)
    tryfun <- function(expr, data, env) {
        tryCatch(if (is.null(expr)) {
                NULL
            }else{
                if (is_formula(expr)) {
                    expr_fml <- quo_text(expr)
                    if (grepl("^~", expr_fml))
                        expr <- as_quosure(
                            as.formula(paste0("~", sub("^~+(.*)$", "\\1", expr_fml))),
                            env=attr(expr, ".Environment"))
                }
                eval_tidy(expr, data=data, env=env)
            }, error=function(e) e)
    }
    chkdots <- lapply(dots, tryfun, data=datamask, env=caller_env(1L))
    o <- lapply(seq_along(chkdots), function(i) {
        if (is(chkdots[[i]], "simpleError")) {
            x <- sym(sub(".*object '(.+)' not found.*", "\\1", chkdots[[i]]$message))
        }else{
            x <- chkdots[[i]]
            cond1 <- ! any(class(x) %in% c("formula", "name", "quosure", "character", "call")) &&
                     ! (is.na(x) || length(x) == 0)
            cond2 <- is(x, "character") && (length(x) > 1L || length(x) == 0)
            if (cond1 || cond2)
                if (! identical(quo_text(dots[[i]]), deparse(chkdots[[i]])))
                    x <- dots[[i]]
        }
        return(enquoArg(x))
    })
    if (length(o) == 0) o <- list(NULL)
    if (flatten_mono) if (length(o) == 1) o <- o[[1]]
    if (named) names(o) <- names(dots)
    return(o)
}

#' @export
#' @rdname enquo_arg
enquo_args <- enquo_arg

# # Depercated
# enquo_sgl_arg <- function(arg) {
#     # convert one arg to quosure
#     arg_class <- try(class(arg), silent=TRUE)
#     if (! any(arg_class %in% c("formula", "name", "quosure", "character", "call")))
#         return(NULL)
#
#     x <- tryCatch(if (is.null(arg)) NULL else eval_tidy(arg, env=caller_env(1L)),
#                   error=function(e) e)
#     x_ <- substitute(arg, env=environment())
#     # sequence: x_ ==> arg ==> x
#     if (is(x, "simpleError"))
#         x <- sub(".*object '(.+)' not found.*", "\\1", x$message)
#     if (! any(class(x) %in% c("formula", "name", "quosure", "character", "call")) ||
#         (is(x, "character") && length(x) > 1L))
#         x <- if (deparse(x_) == deparse(arg)) x_ else arg
#     return(enquoArg(x))
# }

enquoArg <- function(x) UseMethod(".enquoArg", x)

.enquoArg.quosure <- function(x) return(x)

#' @importFrom rlang sym
.enquoArg.character <- function(x){
    o <- sym(x)
    return(enquo(o))
}

#' @importFrom rlang sym
.enquoArg.formula <- function(x){
    o <- parse_formula(x)
    ox <- if (is.na(o[['x']])) NULL else sym(o[['x']])
    oy <- if (is.na(o[['y']])) NULL else sym(o[['y']])
    if (is.null(ox)) stop("formula must be in y~x or ~x form.")
    if (! is.na(o[["y"]])){
        return(list(x=enquo(ox), y=enquo(oy)))
    }else{
        return(enquo(ox))
    }
}

.enquoArg.name <- function(x)  return(enquo(x))

.enquoArg.call <- function(x){
    o <- quote(x)
    return(enquo(o))
}

#' @importFrom rlang caller_env
.enquoArg.default <- function(x){
    if (is.null(x)) return(NULL)
    x <- substitute(x, env=caller_env())
    o <- tryCatch(enquo(o), error=function(e) e)
    if (is(o, "simpleError")) return(NULL)
    return(enquo(o))
}


flatten_df <- function(df, output=c("list", "vector", "data.frame")){
    # flatten a data.frame to a list or vector
    output <- match.arg(output)
    stopifnot(inherits(df, "data.frame"))
    if (nrow(df) * ncol(df) > 100)
        warning("You will yield a large list/vector of length > 100. \n",
                "This function is designed to flatten small data.frames.")
    out <- df %>% t %>% as.data.frame %>% unlist
    names(out) <- expand.grid(names(df), seq_len(nrow(df))) %>%
        apply(1, paste, collapse='')
    if (output %in% c('list', 'data.frame')){
        out <- as.list(out)
        col_class <- vapply(df, function(col) class(col)[[1]], FUN.VALUE=character(1L))
        col_class <- rep(col_class, nrow(df))
        invisible(lapply(seq_along(out), function(i) class(out[[i]]) <<- col_class[i]))
        if (output == 'data.frame') out <- as.data.frame(out, stringsAsFactors=FALSE)
    }
    return(out)
}

#' Align a vector to match the certain length
#'
#' When the input vector is longer than the expected length, the vector will be
#' truncated. When the input vector is shorter than the expected length, part or
#' the whole body of it will be replcated to match the length. The overhead part will
#' then also be truncated.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x input vector
#' @param length.out numeric, the length of the expected result vector. It is
#' consistent with \code{length.out} in \code{\link{rep}()}
#' @param rep.which NULL or an integer vector, indicating the element index in \code{x}
#' that will be replicated to extend the vector. Default -1. Only effective
#' when append is NULL. \itemize{
#'  \item NULL: all the elements will be replicated. \cr
#'  \item integer vector (other than zero): the index of the elements to replicate.
#'   only the valid elements will be replicated (remove invalid indices). \itemize{
#'    \item positive: the index from top, e.g., 1, 2 in 1:10 means #1, #2. \cr
#'    \item negative: the index from bottom, e.g., -1, -2 in 1:10 means #10, #9.
#'   }
#' }
#' @param rep.extend character, the method to extend \code{x}. Default 'times'.
#' \itemize{
#'   \item times: \code{rep(x, times)} will be called. When \code{rep.which} is
#'    not continuous, it is equivalent to \code{each}. When \code{append} is not NULL,
#'    \code{append} will be replicated using \code{rep(append, times)} to append
#'    to \code{x}.\cr
#'   \item each: \code{rep(x, each)} will be called. When \code{append} is not
#'    NULL, \code{append} will be replicated using \code{rep(append, each)}
#'    to append to \code{x}.
#' }
#' @param append a vector for replication to append to \code{x}. Default NULL.
#' \itemize{
#'  \item NULL: \code{rep.which} will be used to apply \code{\link{rep}()} to extend
#'   \code{x}.
#'  \item others: \code{append} instead of \code{rep.which} will be used to apply
#'   \code{\link{rep}()} to extend \code{x}.
#' }
#'
#' @return a vector of length \code{length.out}
#' @export
#'
#' @examples
#' \dontrun{
#' x <- letters[1:5]
#'
#' aline(x, length.out=9, rep.which=2:3)
#' # [1] "a" "b" "c" "b" "c" "b" "c" "d" "e"
#'
#' aline(x, length.out=9, rep.which=2:3, rep.extend='each')
#' # [1] "a" "b" "b" "b" "c" "c" "c" "d" "e"
#'
#' aline(x, 9, rep.which=2:3, rep.extend='times', append=NA)
#' # [1] "a" "b" "c" "d" "e" NA  NA  NA  NA
#'
#' aline(x, 9, rep.extend='each', append=c('m', 'n'))
#' # [1] "a" "b" "c" "d" "e" "m" "m" "n" "n"
#'
#'
#' y <- structure(1:5, names=letters[1:5])
#'
#' aline(y, length.out=9, rep.which=2:3)
#' # a b c b c b c d e
#' # 1 2 3 2 3 2 3 4 5
#'
#' aline(y, length.out=9, rep.which=c(2, 4))
#' # a b b b c d d d e
#' # 1 2 2 2 3 4 4 4 5
#' }
aline <- function(x, length.out, rep.which = -1,
                  rep.extend=c('times', 'each'), append=NULL){
    # check arts
    stopifnot(is.null(dim(x)) && is.vector(x))
    stopifnot(is.numeric(length.out))
    stopifnot(is.null(rep.which) || (is.numeric(rep.which) && rep.which != 0))
    rep.extend <- match.arg(rep.extend)
    rep_idx <- rep.which

    if (length(x) >= length.out){
        out <- x[seq_len(length.out)]
    }else{
        if (! is.null(append)){
            append <- unlist(append)
            rep.times <- ceiling((length.out - length(x))/length(append))
            if (rep.extend == 'times'){
                out <- c(x, rep(append, times=rep.times, length.out=length.out-length(x)))
            }else if (rep.extend == 'each'){
                out <- c(x, rep(append, each=rep.times, length.out=length.out-length(x)))
            }
        }else{
            if (is.null(rep.which)) {
                rep_idx <- seq_along(x)
            }else{
                rep_idx[rep_idx < 0] <- length(x) + rep_idx[rep_idx < 0] + 1
                if (! all(rep_idx %in% seq_along(x)))
                    warning('Elements #', paste(
                        which(! rep_idx %in% seq_along(x)), collapse=', '),
                        ' are not in x.')
                rep_idx <- rep_idx[rep_idx %in% seq_along(x)]
            }
            keep_idx <- seq_along(x)[! seq_along(x) %in% rep_idx]
            names(keep_idx) <- keep_idx
            rep.times <- ceiling(
                (length.out-length(keep_idx)) / (length(x)-length(keep_idx)))

            if (rep.extend == 'times'){
                cut_grp <- which(rep_idx != c((rep_idx-1)[-1], Inf))
                cut_len <- cut_grp-c(0, cut_grp[-length(cut_grp)])
                cut_grp <- mapply(rep, rep_idx[cut_grp], each=cut_len) %>% unlist
                rep_lst <- lapply(split(rep_idx, cut_grp), function(lst)
                    rep(lst, times=rep.times))
                idx <- c(as.list(keep_idx), rep_lst)
                idx <- idx[order(as.numeric(names(idx)))] %>% unlist
                out <- x[idx]

            }else if (rep.extend == 'each'){
                rep_idx <- rep(rep_idx, each=rep.times)
                idx <- c(keep_idx, rep_idx)
                out <- x[sort(as.numeric(idx))]
            }
            out <- out[seq_len(length.out)]
        }
    }
    return(out)
}

# -----------Deprecated----------------
# #' Pipe Binary Operation
# #'
# #' \code{\link{\%>\%}} is a great tool to chain a series of operations in a concise format.
# #' Here provides binary operators combined with pipeline.
# #' @param lhs Left hand side object
# #' @param rhs Right hand side object
# #'
# #' @return A new object with the value \code{rhs opr rhs}
# #' @importFrom magrittr %>%
# #' @export
# #' @export %>%
# #' @seealso \code{\link{magrittr}}
# #' @examples
# #' ## simple object
# #' a <- 3
# #' a %>+% 1 %>-% 2 %>*% 3 %>/% 4
# #'
# #' ## vector
# #' a <- 1:4
# #' a %>+% 1 %>-% 2 %>*% 3 %>/% 4
# #'
# #' ## object operates object
# #' a <- 1:4
# #' b <- 1:2
# #' c <- 3:4
# #' a %>-% b %>+% c %>*% b %>/% c %>^% b %>root% c
# #'
# #' @rdname Pipeline.Binary.Operators
# #'
# `%>+%` <- function(lhs, rhs){
#     opr <- deparse(match.call()[[1]])
#     #browser()
#     lhs <- deparse(substitute(lhs))
#     rhs <- deparse(substitute(rhs))
#     # combine into one string
#     call <- paste(lhs, opr, rhs)
#
#     # match correct operator
#     call <- gsub("%>\\*\\*%", "%>\\^%", call)
#     call <- gsub("%>mod%", "%>%%%", call)
#     call <- gsub("%>root%", "%>\\^1/%", call)
#
#     # format call
#     call <- gsub("%>(\\S+)%\\s+(\\S+)", "%>% `\\1`\\(\\2\\) ", call)
#     call <- gsub("%>\\^%\\s+(\\S+)", "%>% `\\^`\\(\\2\\)", call)
#     call <- gsub("%>\\*%\\s+(\\S+)", "%>% `\\*`\\(\\2\\)", call)
#     call <- gsub("`\\^1/`\\((\\S+)\\)", "`\\^`\\(1/\\1\\)", call) # root x^(1/y)
#
#     # evaluate
#     eval(parse(text=call), envir=parent.frame())
# }
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>-%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>*%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>/%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>^%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>**%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>root%` <- `%>+%`
#
# #' @export
# #' @rdname Pipeline.Binary.Operators
# `%>mod%` <- `%>+%`

