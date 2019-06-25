#------------percent format---------------

#' Convert numbers to percents
#'
#' Convert a vector, list or matrix of numeric values to percentage values.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x A numeric vector, list, or matrix.
#' @param digits Digits of the percentage numbers. Default 0.
#' @param value_asis Logical, default TRUE. Whether keep the actual value. If FALSE,
#' the result will keep the numeric part as is (100 times smaller, e.g., 0.5 --> 0.5\%).
#'
#' @return Character in percentage format, as with the same structure with \code{x}.
#' @importFrom scales percent
#' @export
#' @aliases to_pct
#'
#' @seealso \code{\link[scales]{number_format}}
#'
#' @examples
#' \dontrun{
#' ## Multiply the numbers with 100 and *100%
#' to_pct(c(0.04, 0.3, NA), digits=1)  # return
#' # c("4.0%", "30.0%", NA)
#'
#' to_Pct(c(0.04, 0.3, NA), digits=1, value_asis=FALSE)  # returns
#' # c("0.04%", "0.3%", NA)
#'
#' to_pct(list(0.04, -0.9, list(0.65, NULL)))
#' [[1]]
#' [1] "4%"
#'
#' [[2]]
#' [1] "-90%"
#'
#' [[3]]
#' [[3]][[1]]
#' [1] "65%"
#' [[3]][[2]]
#' NULL
#' }
convNum2Pct <- function(x, digits=0, value_asis=TRUE, ...)
    UseMethod(".toPct", x)

#' @export
.toPct.vector <- function(x, digits=0, value_asis=TRUE, ...){
    if (! is.numeric(digits)) digits <- 0

    idx <- which(vapply(x, function(xx) is.numeric(xx) & ! is.na(xx),
                        FUN.VALUE=logical(length=1)))
    coef <- if (value_asis) 100 else 1
    if (length(idx) > 0)
        x[idx] <- sprintf(paste0("%.", digits, "f%%"), coef * x[idx])

    return(x)
}

#' @export
.toPct.list <- function(x, digits=0, value_asis=TRUE, ...){
    lapply(x, function(lst){
        if (is.list(lst))
            lst <- .toPct.list(lst, digits=digits, value_asis=value_asis, ...)
        else lst <- to_pct(lst, digits=digits, value_asis=value_asis, ...)
        return(lst)
    })
}

#' @export
.toPct.data.frame <- function(x, digits=0, value_asis=TRUE, ...){
    as.data.frame(.toPct.list(x, digits=digits, value_asis=value_asis, ...),
                  stringsAsFactors=FALSE)
}

#' @export
.toPct.matrix <- function(x, digits=0, value_asis=TRUE, ...){
    xdim <- dim(x)
    o <- apply(x, 2, .toPct.vector, digit=digits, value_asis=value_asis, ...)
    if (is.null(dim(o))) dim(o) <- xdim
    return(o)
}

#' @export
.toPct.default <- .toPct.vector

#' @export
#' @rdname convNum2Pct
to_pct <- convNum2Pct

#' Convert numeric-like characters back to numbers
#'
#' Convert a vector, list or matrix of numeric-like characters back to numeric values.
#' E.g., percentage, currency, scientific numbers or comma-delimited numbers.
#' Non-numeric-like characters are coerced to NA.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x Character in numeric-like values. Either vector, list, matrix or array.
#' @param strict Logical, if only convert strict numeric-like characters, e.g, "4.5\%"
#' instead of "about 56\%". Default TRUE. If FALSE, it will extract the first numeric-
#' like character parts.
#' @param type Character vector to define which type of numeric-like characters
#' to convert. Default \code{c("scientific", "comma", "percent", "currency", "wide",
#' "", "e", ",", "\%", "$", "_")}, of which "e", ",", "\%", "$" , "_" are short for
#' "scientific", "comma", "percent", "currency", "wide", respectively. If you don't
#' want to apply any conversion, set \code{type=""}.
#'
#' @return Numeric values as in the same strcture as \code{x}. Non-numeric-like
#' values will be coerced to NA.
#' @export
#'
#' @aliases to_num
#' @importFrom stringr str_detect str_replace_all
#'
#' @examples
#' \dontrun{
#' vec <- c("_84%", "32.9%", "33", "a", " -5", "0.3e2", "1,200", "$67")
#' to_num(vec)  # return
#' # [1]     NA   0.329   33.000    NA   -5.000   30.000 1200.000   67.000
#'
#' to_num(vec, strict=FALSE)  # return
#' # [1]  0.840   0.329   33.000    NA   -5.000   30.000 1200.000   67.000
#'
#' to_num('about 56 pounds', strict=FALSE)  # return
#' # [1]  56
#'
#' to_num(matrix(vec, nrow=2))  # return
#' #       [,1] [,2] [,3] [,4]
#' # [1,]    NA   33   -5 1200
#' # [2,] 0.329   NA   30   67
#'
#' to_num(list(list("56%"), list()))  # return
#' }
convChar2Num <- function(x, strict=TRUE, type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"), ...)
    UseMethod(".toNum", x)

#' @export
.toNum.vector <- function(x, strict=TRUE, type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"), ...){
    type <- match.arg(type, several.ok=TRUE)
    out <- as.numeric(rep(NA, length(x)))
    strict <- if (strict) "strict" else "unstrict"
    PTN <- if (any(c("scientific", "e") %in% type)) pkgenv$CHAR2NUM_SCI else
        pkgenv$CHAR2NUM_NOSCI

    if (! is.character(x)) return(x)

    # -----replace natrual numbers-----
    # replacd wide char
    if (any(c("wide", "_") %in% type)){
        x <- vapply(x, function(chr){
            chr_ <- unlist(strsplit(chr, ""))
            chr <- pkgenv$WIDE_NUMCHAR[chr_]
            chr[is.na(chr)] <- chr_[is.na(chr)]
            paste(chr, collapse="")
        }, FUN.VALUE=character(length=1))
    }
    # remove decimal separators
    if (any(c("comma", ",") %in% type)){
        idx1 <- vapply(x, function(xx) str_detect(xx, PTN$comma[[strict]]$find),
                       FUN.VALUE=logical(length=1)) %>% which
        x[idx1] <- str_replace_all(x[idx1], PTN$comma[[strict]]$patn,
                                   PTN$comma[[strict]]$repl)
    }
    # scientific --> num
    if (any(c("scientific", "e") %in% type)){
        idx2 <- vapply(x, function(xx) str_detect(xx, PTN$scientific[[strict]]$find),
                       FUN.VALUE=logical(length=1)) %>% which
        x[idx2] <- str_replace_all(
            x[idx2], PTN$scientific[[strict]]$patn, PTN$scientific[[strict]]$repl)
    }
    # -----replace unnatural numbers-----
    # percent --> num
    if (any(c("percent", "%") %in% type)){
        idx3 <- vapply(x, function(xx) str_detect(xx, PTN$percent[[strict]]$find),
                       FUN.VALUE=logical(length=1)) %>% which
        out[idx3] <- as.numeric(str_replace_all(
            x[idx3], PTN$percent[[strict]]$patn, PTN$percent[[strict]]$repl))/100
    }
    # currency --> num
    if (any(c("currency", "$") %in% type)){
        idx4 <- vapply(x, function(xx) str_detect(xx, PTN$currency_prfx[[strict]]$find),
                       FUN.VALUE=logical(length=1)) %>% which
        out[idx4] <- as.numeric(str_replace_all(
            x[idx4], PTN$currency_prfx[[strict]]$patn, PTN$currency_prfx[[strict]]$repl))
        idx5 <- vapply(x, function(xx) str_detect(xx, PTN$currency_sufx[[strict]]$find),
                       FUN.VALUE=logical(length=1)) %>% which
        out[idx5] <- as.numeric(str_replace_all(
            x[idx5], PTN$currency_sufx[[strict]]$patn, PTN$currency_sufx[[strict]]$repl))
    }
    suppressWarnings({
        supp_conv <- intersect(which(is.na(out)), which(! is.na(as.numeric(x))))
        out[supp_conv] <- as.numeric(x)[supp_conv]
    })
    return(out)
}

#' @export
.toNum.list <- function(x, strict=TRUE, type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"), ...){
    type <- match.arg(type, several.ok=TRUE)
    lapply(x, function(lst){
        if (is.list(lst)) lst <- .toNum.list(lst, strict=strict, ...)
        else lst <- to_num(lst, strict=strict, ...)
        return(lst)
    })
}

#' @export
.toNum.matrix <- function(x, strict=TRUE, type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"), ...){
    type <- match.arg(type, several.ok=TRUE)
    attr_x <- attributes(x)
    o <- apply(x, 2, .toNum.vector, strict=strict, ...)
    attributes(o) <- attr_x
    return(o)
}

#' @export
.toNum.data.frame <- function(x, strict=TRUE, type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"), ...){
    type <- match.arg(type, several.ok=TRUE)
    as.data.frame(.toNum.list(x, strict=strict, ...), stringsAsFactors=FALSE)
}

#' @export
.toNum.default <- .toNum.vector

#' @export
#' @rdname convChar2Num
to_num <- convChar2Num


get_char2num_pattern <- function(type=c(
    "scientific", "comma", "percent", "currency", "wide", "", "e", ",", "%", "$", "_"),  ...){
    type <- match.arg(type, several.ok=TRUE)
    o <- list()

    # scientific
    if (any(c("scientific", "e") %in% type)){
        ptnNum <- "\\s*((\\d+\\.*\\d*)([Ee]\\-*\\d+)*)"
    }else{
        ptnNum <- "\\s*(\\d+\\.*\\d*)"
    }
    o$scientific <- list(
        strict=list(find=paste0("^\\s*(-*)\\s*", ptnNum, "$"), repl="\\1\\2"),
        unstrict=list(find=paste0("\\D*?(-*)\\s*", ptnNum, "[^%]*$"), repl="\\1\\2")
    )

    # percentage
    o$percent <- list(
        strict=list(find=paste0("^\\s*(-*)\\s*", ptnNum, "%$"), repl="\\1\\2"),
        unstrict=list(find=paste0("\\D*?(-*)\\s*", ptnNum, "%.*$"), repl="\\1\\2")
    )

    # currency
    curPref <- c("\u060b", "\u20b3", "\u0e3f", "\u20bf", "\u20b5", "\uffe0",
                 "\u20a1", "\u20a2", "\u20ab", "\u20af", "\u058f", "\u20a0",
                 "\u20ac", "\u0192", "\u20a3", "\u20b2", "\u20b4", "\u20ad",
                 "\u20ba", "\u20be", "\u20bc", "\u2133", "\u20a5", "\u20a6",
                 "\u20a7", "\u20b1", "\u20b0", "\uffe1", "\ufdfc", "\u17db",
                 "\u20bd", "\u20b9", "\u20a8", "\u20aa", "\u09f3", "\u20b8",
                 "\u20ae", "\u20a9", "\uffe5", "\uff04", "\\$")
    curSuff <- c("\u5186", "\u5143", "\u5706")
    o$currency_prfx <- list(
        strict=list(find=paste0("^\\s*(-*)\\s*[", paste(curPref, collapse=""), "]", ptnNum, "\\s*$"),
                    repl="\\1\\2"),
        unstrict=list(find=paste0("\\D*?(-*)\\s*[", paste(curPref, collapse=""), "]", ptnNum, "\\s*.*$"),
                      repl="\\1\\2")
    )
    o$currency_sufx <- list(
        strict=list(find=paste0("^\\s*(-*)\\s*", ptnNum, "[", paste(curSuff, collapse=""), "]\\s*$"),
                    repl="\\1\\2"),
        unstrict=list(find=paste0("\\D*?(-*)\\s*", ptnNum, "[", paste(curSuff, collapse=""), "]\\s*.*$"),
                      repl="\\1\\2")
    )

    invisible(lapply(c(
        "percent", "scientific", "currency_prfx", "currency_sufx"), function(nm){
        o[[nm]]$strict$patn <<- o[[nm]]$strict$find
        o[[nm]]$unstrict$patn <<- o[[nm]]$unstrict$find
    }))

    o$comma <- list()
    o$comma$strict <- o$comma$unstrict <- list(
        find="\\d{1,3}(,\\d{3})+\\.*\\d*", patn=",(\\d{3})", repl="\\1")

    return(o)
}
