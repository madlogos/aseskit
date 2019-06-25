#' Get Number of Characters
#'
#' Get the number of characters in an object. It supports recursive counting for 
#' lists.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x Object. Vector, matrix, list or data frame.
#' @param rm.blank Logical: default TRUE, and '' will be coerced to NA.
#' @param ... Other arguments to pass to \code{.countChar}
#' @aliases count_char
#’ @rdname count_char
#' 
#' @return An object of the same structure with \code{x}.
#' @export
#'
#' @examples
#' \dontrun{
#' count_char(c("dws", 23)) # return c(3, 2)
#' 
#' count_char(matrix(c("dws", 23, "2332", 1), ncol=2))  # return
#' ##       [,1] [,2]
#' ## [1,]    3    4
#' ## [2,]    2    1
#' 
#' count_char(list("dws", 23, c("2332", 1)))  # return
#' ## [[1]]
#' ## [1] 3
#' ##
#' ## [[2]]
#' ## [1] 2
#' ##
#' ## [[3]]
#' ## [1] 4 1
#' }
countChar <- function(x, rm.blank=TRUE, ...){
    UseMethod(".countChar", x)
}

#' @export
#' @rdname countChar
count_char <- countChar

#' @export
.countChar.matrix <- function(x, rm.blank=TRUE, ...){
    if (rm.blank) x <- ifblank(x, NA)
    return(nchar(x, ...))
}

#' @export
.countChar.data.frame <- function(x, rm.blank=TRUE, ...){
    if (rm.blank) x <- ifblank(x, NA)
    dim <- dim(x)
    vname <- names(x)
    out <- matrix(nchar(unlist(x), ...), nrow=dim[1])
    out <- as.data.frame(out, stringsAsFactors=FALSE)
    names(out) <- vname
    return(out)
}

#' @export
.countChar.list <- function(x, rm.blank=TRUE, ...){
    lapply(x, function(v){
		if (is.list(v)){
		    v <- .countChar.list(v, rm.blank=rm.blank, ...) 
	    }else{
			if (rm.blank) v <- nchar(ifblank(v, NA), ...) else
			    v <- nchar(v, ...)
		}
        return(v)
	})
}

#' @export
.countChar.default <- .countChar.matrix

