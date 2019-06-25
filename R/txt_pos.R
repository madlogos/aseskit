#' Clock digits to text position and direction
#'
#' Converts text postion from clock digits to vector (horizontal, vertical, direction).
#' It was inspired by Stata, and will be useful when applying \pkg{recharts} functions.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param pos integer 0-25, clock digits.
#' @param drop logical, whether coerce the result to vector when \code{pos} lengths 1.
#' Default TRUE.
#' @param unname logical, whether unname the result. Default TRUE.
#' @param ... other arguments
#' 
#' @details The function is designed to map clock digits to the 3*3 grids of the lyaout. 
#' The corresponding position vectors (horizontal, vertical, direction) to the 
#' clock integers are as follows. \describe{
#' \item{0-12 referes to:}{the favorable position definitions on the clock
#' \tabular{lllll}{
#'              \tab 11(l, t, h) \tab 12(c, t, h) \tab 1(r, t, h) \tab \cr
#'  10(l, t, v) \tab             \tab             \tab            \tab 2(r, t, v) \cr
#'  9(l, c, v)  \tab             \tab  0(c, c, h) \tab            \tab 3(r, c, v) \cr
#'  8(l, b, v)  \tab             \tab             \tab            \tab 4(r, b, v) \cr
#'              \tab  7(l, b, h) \tab 6(c, b, h)  \tab 5(r, b, h) \tab
#' }}
#' \item{13-25 refers to:}{the same position as 0-12, with the direction reverted
#' \tabular{lllll}{
#'              \tab 23(l, t, v) \tab 24(c, t, v) \tab 13(r, t, v) \tab \cr
#'  22(l, t, h) \tab             \tab             \tab             \tab 14(r, t, h) \cr
#'  21(l, c, h) \tab             \tab 25(c, c, v) \tab             \tab 15(r, c, h) \cr
#'  20(l, b, h) \tab             \tab             \tab             \tab 16(r, b, h) \cr
#'              \tab 19(l, b, v) \tab 18(c, b, v) \tab 17(r, b, v) \tab
#' }}}
#' To make it more intuitive, please check the charts below:
#' \figure{clock_pos.png} \cr
#' Note: "=" refers to horizontal direction, "||" refers to vertical direction.
#' 
#' @return A vector or 3-column matrix of \code{horizontal} (x-alignment), \code{vertical}
#' (y-alignment) and \code{direction} (direction).
#' 
#' @export
#' @aliases clock_to_pos
#'
#' @examples
#' \dontrun{
#' convClock2Pos(2) ## returns c("right", "top", "vertical")
#' 
#' clock_to_pos(2:3, unname=FALSE)  # returns
#' #   horizontal  vertical  direction 
#' # 2 "right"     "top"     "vertical"
#' # 3 "right"     "center"  "vertical"
#' }
#'
convClock2Pos <- function(pos, drop=TRUE, unname=TRUE, ...){
    stopifnot(all(is.numeric(pos)))
    stopifnot(all(pos <= 25 & pos >= 0))
    tbl.pos <- CLOCKPOS[as.character(as.integer(pos)), , ..., drop=drop]
    if (unname) return(unname(tbl.pos)) else return(tbl.pos)
}

#' @export
#' @rdname convClock2Pos
clock_to_pos <- convClock2Pos

