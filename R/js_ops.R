#' \code{Javascript}-style Binary Operator Assignment
#'
#' If you are familiar with \code{Javascript}, you may miss +=, -=, *=, /= very much.
#' The operator can be \code{+, -, *, /, ^, **, \\, mod, root}.
#' @param lhs Left hand side argument.
#' @param rhs Right hand side argument.
#' @param envir Environment for operation to take place. Default \code{parent.frame()}
#'
#' @return Nothing, but lhs is already modified.
#' @importFrom stringr str_replace_all str_split
#' @rdname JS.Operator.Assignment
#' @export
#'
#' @seealso You can also use the compound operator \code{\link[magrittr]{\%<>\%}} and
#' the operator aliases provided in \pkg{magrittr} to realize simiar effects.
#' @examples
#' \dontrun{
#' ## update an object entirely
#' a <- 3
#' a %+=% 2     # 3 --> (3 + 2) --> 5, equivalent to a %<>% add(2)
#' a %-=% 2     # 5 --> (5 - 2) --> 3, equivalent to a %<>% subtract(2)
#' a %*=% 2     # 3 --> (3 * 2) --> 6, equivalent to a %<>% multiply_by(2)
#' a %/=% 2     # 6 --> (6 / 2) --> 3, equivalent to a %<>% divide_by(2)
#' a %^=% 2     # 3 --> (3 ^ 2) --> 9, equivalent to a %<>% raise_to_power(2)
#' a %**=% 2    # 9 --> (3 ** 2) --> 81, same as above
#' a %\=% 30    # 81 --> (81 %% 30) --> 21, equivalent to a %<>% mod(30)
#' a %mod=% 8   # 21 --> (21 %% 8) --> 5, equivalent to a %<>% mod(8)
#' a %root=% 2  # 5 --> (5 ^ (1/2)) --> 2.236, equivalent to a %<>% sqrt
#'
#' ## object and object
#' a <- 1:4
#' b <- 4:1
#' a %+=% b     # a --> c(5,5,5,5)
#' a %*=% b     # a --> c(20, 15, 10, 5)  (c(5,5,5,5) * c(4,3,2,1))
#'
#' ## update an object partially
#' b <- 1:4
#' b[1] %+=% 1     # b --> c(2,2,3,4)
#'
#' c <- list(list(A=1, B=2:3), list(C=4))
#' c[[1]]$A %-=% 1 # c[[1]]$A --> 0, rest elements are not changed
#'
#' 1 %+=% b        # simply print 3 (1 + b[2]), but no variable is changed
#' 1 %+=% 1        # simply print 2 (1 + b[1]), but no variable is changed
#' }
#'
`%+=%` <- function(lhs, rhs, envir=parent.frame()){
    opr <- str_replace_all(deparse(match.call()[[1]]), "^%(.+)=%$", "\\1")
    if (opr %in% c('\\', '%%', '%', 'mod')) opr <- '%%'
    if (opr %in% c('root')) transformRHS <- "1/" else transformRHS <- ""
    if (opr %in% c('^', '**', 'root')) opr <- '^'

    oLHS <- deparse(substitute(lhs))
    oRHS <- deparse(substitute(rhs))

    nameLHS <- str_split(oLHS, pattern="[$\\[\\]]")[[1]][1]
    nameRHS <- str_split(oRHS, pattern="[$\\[\\]]")[[1]][1]
    fullLHS <- deparse(substitute(lhs))
    fullRHS <- deparse(substitute(rhs))

    # mirror the object in function envir
    if (! identical(lhs, substitute(lhs)))
        assign(nameLHS, get(nameLHS, envir), envir=environment())
    if (! identical(rhs, substitute(rhs)))
        assign(nameRHS, get(nameRHS, envir), envir=environment())

    # assign value to the object within function envir
    if (identical(lhs, substitute(lhs)))  {
        out <- eval(parse(text=paste0(
            "`", opr, "`(", lhs, ",", transformRHS, fullRHS, "[[1]])")))
        return(out)
    }else{
        eval(parse(text=paste0(
            fullLHS, " <- `", opr, "`(", fullLHS, ", ", transformRHS,
            fullRHS, ")")))
        assign(nameLHS, eval(parse(text=nameLHS)), envir=envir)
    }
}

#' @export
#' @rdname JS.Operator.Assignment
`%-=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%*=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%/=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%^=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%**=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%\\=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%mod=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%root=%` <- `%+=%`
