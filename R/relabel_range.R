#' Rename data range labels
#'
#' We usually use \code{\link{cut}} to cut continuous vectors into factors. But the levels
#' are typically presented as \code{'[l, u)'} or \code{'(l, u]'}. This function can reformat
#' the cut results.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x The factor vector yielded by \code{\link{cut}} function.
#' @param quick.form A string to define the new format.
#' \describe{
#'  \item{-}{equivalent to \code{format="x-y"}: 0-10, 10-20, 20-30}
#'  \item{~}{equivalent to \code{foramt="x~y"}: 0~10, 10~20, 20~30}
#'  \item{[)}{equivalent to \code{foramt="[x, y)"}: [0, 0), [10, 0), [20, 0)}
#'  \item{(]}{equivalent to \code{foramt="(x, y]"}: (0, 0], (10, 0], (20, 0]}
#'  \item{<=}{equivalent to \code{foramt=c("<=y", "x-y", ">x")}: <=9, 10-19, 20-29, >29}
#'  \item{>=}{equivalent to \code{foramt=c("<y", "x-y", ">=x")}: <10, 10-20, 20-30, >=30}
#' }
#' @param format Self-defined character vector length 1-3 to format the label. If 
#' more than 3 character elements are given, the function will only use the first 3.
#' Default NULL, and \code{quick.form} is applied. See details.
#' @param left.subtract Integer which is subtracted from the left limit of
#' each range. Default 0.
#' @param right.subtract Integer which is subtracted from the right limit of
#' each range. Default 0.
#' @param ... ignore
#' @aliases relab_range
#'
#' @details All the \code{format} definitions should use "x" to represent the lower
#' limit and "y" to reresent the upper limit of each range label. For instance, 
#' 'x-y' coereces '(0, 1]' to '0-1', '<y' coerces '(0, 1]' to '<1'. \cr 
#' 
#' By principle, it should be of length 3: beginning label, middle label(s) and 
#' end label. But there could be shortened forms: \itemize{
#'  \item length 1: all of the same format. E.g., \code{'x-y'} results in '0-1',
#'  '1-2', '2-3'. \cr
#'  \item length 2: beginning label and middle label(s). End label is the same with
#'  middle label. E.g, \code{c("<x", "x-y") results in "<0", "1-2", "2-3".} \cr
#'  \item length 3: full-form \code{format}.
#' }
#' 
#' @return A new factor vector with reformatted levels.
#' @importFrom crayon blue silver yellow bold
#' @importFrom stringr str_detect str_split str_replace_all
#' @export
#' 
#' @examples
#' \dontrun{
#' x = cut(1:100, c(0, 20, 40, 60, 80, 100))
#' renameRange(levels(x), "-")
#' # [1] "0-20"   "20-40"  "40-60"  "60-80"  "80-100"
#'
#' renameRange(levels(x), "[)")
#' # [1] "[0, 20)"   "[20, 40)"  "[40, 60)"  "[60, 80)"  "[80, 100)"
#'
#' renameRange(levels(x), ">=", left.substract = -1)
#' # [1] "<20"   "21-40"  "41-60"   "61-80"  "\u226581"
#'  
#' relab_range(levels(x), format=c(" ~ y", "x ~ y", "x ~ "))
#' # [1] " ~ 20"   "20 ~ 40" "40 ~ 60" "60 ~ 80" "80 ~ "  
#' }
renameRange <- function(x, quick.form=c("-", "~", "[)", "(]", "<=", ">="),
                        format=NULL, left.subtract=0, right.subtract=0,
...){
    # x is the factors of the vector yielded by `cut()`
    if (! all(str_detect(x, "^\\(\\d+,\\d+\\]$")))
        stop("x must be the level lables yielded by cut(), in the format of ",
             "(0,1].")
    if (any(duplicated(x)))
        warning("There are duplicated entries in x. ", 
                "Make sure x is a factor vector yielded by cut().")
    
    quick.form <- match.arg(quick.form)
    
    if (! is.character(format)){
        invisible(cat(
            bold$blue("Range vector quick formatter: ") %+% 
            silver("(You can assign `format` on your own)\n") %+%
            ifelse(quick.form == '-', '-> ', '   ') %+% 
            yellow("-: ") %+% "0-10, 10-20, 20-30 " %+% 
            silver("(i.e. format='x-y')\n") %+%
            ifelse(quick.form == '~', '-> ', '   ') %+%
            yellow("~: ") %+% "0~10, 10~20, 20~30 " %+% 
            silver("(i.e. foramt='x~y')\n") %+%
            ifelse(quick.form == '[)', '-> ', '   ') %+% 
            yellow("[): ") %+% "[0, 0), [10, 0), [20, 0) " %+% 
            silver("(i.e. foramt='[x, y)')\n") %+%
            ifelse(quick.form == '(]', '-> ', '   ') %+% 
            yellow("(]: ") %+% "(0, 0], (10, 0], (20, 0] " %+% 
            silver("(i.e. format='(x, y]')\n") %+%
            ifelse(quick.form == '<=', '-> ', '   ') %+% 
            yellow("<=: ") %+% "\u{2264}9, 10-19, 20-29, >29 " %+%
            silver("(i.e. foramt=c('\u{2264}y', 'x-y', '>x'))\n") %+%
            ifelse(quick.form == '>=', '-> ', '   ') %+% 
            yellow(">=: ") %+% "<10, 10-20, 20-30, \u{2265}30 " %+%
            silver("(i.e. format=c('<y', 'x-y', '\u{2265}x'))\n")
        ))
        format <- list(
            "-" = c("x-y"),
            "~" = c("~y", "x~y", "x~"),
            "[)" = c("[x, y)"),
            "(]" = c("(x, y]"),
            "<=" = c("\u{2264}y", "x-y", ">x"),
            ">=" = c("<y", "x-y", "\u{2265}x"))[[quick.form]]
    }
    if (length(format) > 3) {
        format <- format[1:3]
    }
    if (! all(str_detect(
        format, "^([^xy]*x[^xy]*y?[^xy]*|[^xy]*x?[^xy]*y[^xy]*|[^xy]*[xy])$")))
        stop("The format is not properly defined. Should be a character like ",
             "'x - y', '<x', '>=y', '(x, y]', ...")
    if (length(format) == 2){
        format <- c(format, format[2])
    }else if (length(format) == 1){
        format <- rep(format, 3)
    }

    dt <- matrix(as.numeric(unlist(str_split(x, "[\\(,\\]]"))), 
                 byrow=TRUE, ncol=4)[, 2:3]
    dt[, 1] <- dt[, 1] - as.numeric(left.subtract)
    dt[, 2] <- dt[, 2] - as.numeric(right.subtract)
    dt[] <- apply(dt, 2, as.character)
    
    out <- apply(dt, 1, function(row){
        str_replace(format[2], "x", row[1]) %>% 
            str_replace("y", row[2])
        })
    out[1] <- str_replace(format[1], "x", dt[1, 1]) %>% 
        str_replace("y", dt[1, 2])
    if (length(out) > 1){
        out[length(out)] <- str_replace(format[3], "x", dt[nrow(dt), 1]) %>% 
            str_replace("y", dt[nrow(dt), 2])
    }

    return(out)
}

#' @export
#' @rdname renameRange
relab_range <- renameRange

