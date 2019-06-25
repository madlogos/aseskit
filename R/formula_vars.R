#' Parse a formula to get the variable list
#'
#' Get \code{x}, \code{y}, \code{weight}, \code{series} from a formula expression. 
#' If \code{expr} cannot be interpreted as formula, it will be returned as a character
#' vector as-is.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param expr An expression to coerce to formula.
#' @param ... Other arguments to pass to the function.
#'
#' @return \itemize{
#' \item If the parsing succeeds, then a named vector of \code{x, y, series} and 
#' \code{weight}, the missing element will be output as NA \cr
#' \item If the parsing fails, then the \code{expr} as a character \cr
#' \item If \code{expr} is NULL or NA/NaN, then NULL or NA
#' }
#' If more than one asterisks or pipes are detected, the function will only extract 
#' the last match as series or weight.
#' @details There are two forms of arguments: \itemize{
#' \item Put all the arguments in \code{expr} and ignore \code{...}, then the function
#' will analyze the format and parse elements automatically: left-hand side to "~" is
#' \code{y}, right-hand side to "~" is independent variables, of which symbols 
#' connected with + or - is \code{x}, symbols to the right of * is \code{weight}
#' and symbols to the right of | is \code{series}. \cr
#' \item Put arguments along with \code{expr} and \code{...}, then the function
#' will wrap them up as-is and output in a named vector. In this case, the result
#' totally depends on the order you input the arguments.
#' }
#' @export
#' @importFrom stringr str_replace_all str_extract_all
#' @aliases parse_formula
#'
#' @examples
#' \dontrun{
#' ## Identify ingredients in a formula.
#' getVarsFromFormula("y+z~x*w+m|d")  ## return
#'   \tabular(cccc){
#'    x \tab y \tab series \tab weight \cr
#'    "x + m" \tab "y + z" \tab "d"  \tab  "w"
#'   }
#'   
#' ## it is equivalent to
#' parse_formula(y+z~x*w+m|d)
#' 
#' ## another form
#' parse_formula(y+z, x+m, d, w)
#' 
#' }
getVarsFromFormula <- function(expr, ...){
    arg <- try(deparse(expr), silent=TRUE)
    if (is(arg, "try-error")) arg <- deparse(substitute(expr))
    dots <- substitute(list(...))[-1]
    varlst <- unlist(c(arg, sapply(dots, deparse)))
    
    if (identical(varlst, "NULL")) return(NULL)
    if (identical(varlst, "NA") || identical(varlst, "NaN")) return(NA)
    
    varlst_ <- tryCatch(as.formula(varlst), error=function(e) invisible(e),
                        finally=invisible())
    if (! inherits(varlst_, "simpleError")) varlst <- varlst_

    if (inherits(varlst, c('name', 'language', 'symbol'))){
        output <- deparse(varlst)
        output[output %in% c("NULL", "NA", "NaN")] <- NA
        if (is.null(dots))
            invisible(warning("Not parsed: expr cannot be coerced to formula."))
        return(structure(output[1:4], names=c("x", "y", "series", "weight")))
    }else if (is.character(varlst)) {
        varlst[varlst %in% c("NULL", "NA", "NaN")] <- NA
        if (is.null(dots))
            invisible(warning("Not parsed: expr cannot be coerced to formula."))
        return(structure(varlst[1:4], names=c("x", "y", "series", "weight")))
    }else if (inherits(varlst, 'formula')){
        varlst <- unlist(as.character(varlst))
        dep <- ""
        indep <- varlst[length(varlst)]
        if (length(varlst) == 3) dep <- varlst[2]
        
        y <- if (is.null(dep)) "" else str_replace_all(dep, "^\\((.*)\\)$","\\1")
        
        # indep <- substr(deparse(indep), 1, nchar(deparse(indep))-2)
        indep <- str_replace_all(indep, "^\\((.*)\\)$", "\\1")
        s <- w <- ""
        x <- trimws(str_replace_all(indep, "[\\*\\|][^+]+", ""))
        if (str_detect(indep, "\\|.+")){
            s <- trimws(str_replace_all(indep, ".*\\|([^+\\*]+).*", "\\1"))
            if (length(unlist(str_extract_all(indep, "\\*"))) > 1)
                invisible(warning("More than 1 asterisks found in expr. ",
                                  "Only to extract the last one as series."))
        }
        if (str_detect(indep, "\\*.+")){
            w <- trimws(str_replace_all(indep, ".*\\*([^+\\|]+).*", "\\1"))
            if (length(unlist(str_extract_all(indep, "\\|"))) > 1)
                invisible(warning("More than 1 pipes found in expr. ",
                                  "Only to extract the last one as weight."))
        }
        output <- c(x, y, s, w)
        output[output == ""] <- NA
        return(structure(str_replace_all(output, "[\\(\\)]", ""),
                         names=c("x", "y", "series", "weight")))
    }else {
        if (is.null(dots))
            invisible(warning("Not parsed: expr cannot be coerced to formula."))
        if (is.null(varlst)) return(NULL)
        else return(structure(deparse(substitute(varlst))[1:4],
                              names=c("x", "y", "series", "weight")))
    }
}

#' @export
#' @rdname getVarsFromFormula
parse_formula <- getVarsFromFormula
