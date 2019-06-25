#' Quick vectorized call of \code{ifelse}
#'
#' \code{ifelse(is.null(x), y, x)} is less handy in some situations. You can use 
#' \code{iif(x, y, criteria)} instead. Inspired by \code{Nz} function in VBA, 
#' it is a vectorized implementation of \code{ifelse} equivalents. Moreover, 
#' it supports recursive conversion (especially useful for lists).
#' @details The philosophy of \code{iif} is to replace elements in \code{x} that
#' matches \code{criteria} (must be a vectorized function returning logical outputs)
#' with scalar value \code{y}. For instance, \code{iif(x, y, is.null)} will convert
#' all the NULL values in x with y. The short-hand call is \code{ifnull(x, y)}. \cr
#' The \code{iif} function family contains some quick calls: \describe{
#' \item{\code{ifzero(x, y)}}{replace elements in x with y where x==0}
#' \item{\code{ifna(x, y)} or \code{ifnan(x, y)}}{replace elements in x with y
#' where x is NA or NaN}
#' \item{\code{ifnull(x, y)}}{replace elements in x with y where x is NULL}
#' \item{\code{ifblank(x, y)}}{replace elements in x with y where nchar(x)==0}
#' \item{\code{ifempty(x, y)}}{replace elements in x with y where length(x)==0}
#' \item{\code{ifspace(x, y)}}{replace elements in x with y where x is white spaces}
#' \item{\code{ifpositive(x, y)}}{replace elements in x with y where x>0}
#' \item{\code{ifnegative(x, y)}}{replace elements in x with y where x<0}
#' }
#' 
#' @param x Vector, matrix, data.frame, array or list. The elements of x that do 
#' not meet the criteria function (e.g., is.null, is.na, ...), will be returned as-is. 
#' The elements of x that meet the \code{criteria} function, will be converted to y.
#' @param y Scalar, if a vector is given, only the first element will be used. 
#' The elements of x that meet the \code{criteria} function, will be converted to y.
#' @param criteria A function (prefer a vectorized one) that returns a logical value, e.g., 
#' \code{is.null}, \code{is.na}. You can also compose a function on your own.
#' 
#' @return The same structure as \code{x}, with those elements which meet \code{criteria} 
#' replaced with \code{y}. Those elements which do not meet \code{criteria} will
#' be left as-is.
#' 
#' @export
#' @seealso If you have two options for replacement, you may need the vectorized
#' implementation of \code{ifelse}: \code{\link[dplyr]{if_else}()}.
#' @examples 
#' \dontrun{
#' ifna(c(1, 4, NA), 0)  # returns
#' # [1] 1  4  0
#' 
#' # iif also supports recursive conversion
#' ifnull(list(3, list(NULL), c(3, 5)), 0)  # returns
#' # [[1]]
#' # [1] 3
#' # 
#' # [[2]]
#' # [[2]][[1]]
#' # [1] 0
#' # 
#' # [[3]]
#' # [1] 3 5
#' 
#' ifzero(data.frame(A=c(1, 0, -2), B=c(-1, 0, 3)), 99)  # returns
#' #    A   B
#' # 1  1  -1
#' # 2 99  99
#' # 3 -2   3
#' 
#' # User-defined function 
#' iif(matrix(c(1, 0, -2, -1, 0, 3), nrow=2), 0, function(v) v < 0)
#' ## Replace all the negative values with 0
#' #      [,1] [,2] [,3]
#' # [1,]    1    0    0
#' # [2,]    0    0    3
#'
#' # Also works for high-dimensional array
#' iif(array(1:8, dim=c(1, 4, 2)), NA, function(v) v %% 2 == 0)
#' ## Replace all the even numbers with NA
#' # ,, 1
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    1   NA    3   NA
#' #
#' # , , 2
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    5   NA    7   NA
#' }
iif <- function(x, y, criteria=is.null){
    stopifnot(inherits(criteria, "function"))
    if (! "logical" %in% class(criteria(1)))
        stop("'criteria' must be a function that yields logical values.")
    UseMethod(".iif", x)
}

#' @export
.iif.list <- function(x, y, criteria=is.null){
    if (length(x) == 0){
        if (criteria(x)) return(y[[1]]) else return(x)
    }else{
        lapply(x, function(v) {
            if (is.list(v)){  # recursive
                v <- .iif.list(v, y, criteria=criteria)
            }else{
                which.matches <- tryCatch(
                    criteria(v), error=function(e) e, final="Error occured.")
                if (! inherits(which.matches, "error")){
                    which.matches <- which(which.matches)
                    if (length(which.matches) > 0) v[which.matches] <- y[[1]]
                }else{
                    warning(which.matches)
                }
            }
            return(v)
        })
    }
}

#' @export
.iif.data.frame <- function(x, y, criteria=is.null){
    o <- lapply(x, function(lst) unlist(.iif.list(x=lst, y=y, criteria=criteria)))
    return(as.data.frame(o, stringsAsFactors=FALSE))
}

#' @export
.iif.matrix <- function(x, y, criteria=is.null){
    xdim <- dim(x)
    o <- apply(x, 2, function(v) {
        which.matches <- tryCatch(
            criteria(v), error=function(e) e, final="Error occured.")
        which.matches[is.na(which.matches)] <- FALSE
        if (! inherits(which.matches, "error")){
            if (any(which.matches)) v[which.matches] <- y[[1]]
        }else{
            warning(which.matches)
        }
        return(v)
    })
    if (is.null(dim(o))){
        dim(o) <- xdim
    }else if (xdim[1] != xdim[2] && all(dim(o) == rev(xdim))) {
        o <- t(o)
    }
    return(o)
}

#' @export
.iif.vector <- function(x, y, criteria=is.null){
    if (any(criteria(x))) 
        if (length(y) < 1L) x <- x[!criteria(x)] else x[criteria(x)] <- y[[1]]
    return(x)
}

#' @export
.iif.default <- .iif.vector

#' @export
#' @rdname iif
ifnull <- function(x, y) iif(x, y, criteria=is.null)
#' @export
#' @rdname iif
ifna <- function(x, y) iif(x, y, criteria=is.na)
#' @export
#' @rdname iif
ifnan <- function(x, y) iif(x, y, criteria=is.nan)
#' @export
#' @rdname iif
ifblank <- function(x, y) iif(x, y, criteria=Vectorize(function(v) 
    nchar(as.character(v)) == 0))
#' @export
#' @rdname iif
ifempty <- function(x, y) iif(x, y, criteria=function(v) length(v) == 0)
#' @export
#' @rdname iif
ifzero <- function(x, y) iif(x, y, criteria=function(v) identical(v, 0))
#' @export
#' @rdname iif
ifpositive <- function(x, y) iif(x, y, criteria=function(v) is.numeric(v) & v > 0)
#' @export
#' @rdname iif
ifnegative <- function(x, y) iif(x, y, criteria=function(v) is.numeric(v) & v < 0)
#' @export
#' @rdname iif
ifspace <- function(x, y) iif(x, y, criteria=function(v) grepl("^\\s*$", v))
