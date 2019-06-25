#' Parse the contents in clipboard to form a tibble
#'
#' Tables copied from spreadsheet or similar software into clipboard are usually
#' a character vector containing '\\t'. This function parses the clipboard and tries
#' to coerce the string vector to a \pkg{\link{tibble}}. It is a wrapper of
#' \pkg{readr}:\code{\link{read_table2}}. The arguments directly come from
#' \code{read_table2}.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param var_row integer, the row index of the header row. Default 1. If no header
#' is needed, put it 0. If NULL, it will guess the colnames.
#' @param col_names	either TRUE, FALSE or a character vector of column names.
#' @param col_types	either of NULL, a cols() specification, or a string.
#' See vignette("readr") for more details.
#' @param locale the locale controls defaults that vary from place to place.
#' @param n_max	maximum number of records to read.
#' @param guess_max	maximum number of records to use for guessing column types.
#' @param progress display a progress bar? By default it will only display in an
#' interactive session and not while knitting a document.
#' @param skip_empty_rows should blank rows be ignored altogether? i.e. If this
#' option is TRUE then blank rows will not be represented at all. If it is FALSE
#' then they will be represented by NA values in all the columns.
#'
#' @return A tibble (coerced using \code{\link{read_table2}}). If fails, return NULL
#' @importFrom stringr str_detect
#' @importFrom readr read_table2 clipboard
#' @importFrom rlang is_quosure eval_tidy
#' @seealso \code{\link[readr]{read_table2}} and \code{\link[readr]{clipboard}}
#' @examples
#' \dontrun{
#' parse_clipb()
#' }
#'
parse_clipb <- function(var_row=1L, col_names = var_row>0, col_types = NULL,
                        locale = readr::default_locale(), na = "NA",
                        n_max = Inf, guess_max = min(n_max, 1000),
                        progress = readr::show_progress(), comment = "",
                        skip_empty_rows = TRUE){
    # return NULL, or a matrix
    stopifnot(is.numeric(var_row) || is.null(var_row))
    if (is.logical(col_names))
        skip <- if (col_names) max(ifnull(var_row, 0) - 1) else ifnull(var_row, 0)
    else
        skip <- var_row

    x <- try(read_table2(
        clipboard(), col_names=col_names, col_types=col_types,
        locale=locale, na=na, skip=skip, n_max=n_max,
        guess_max=guess_max, comment=comment,
        skip_empty_rows=skip_empty_rows), silent=TRUE)
    if (is(x, "try-error")) return(NULL)
    return(x)
}


#' Describe and visulaize the statistic distribution of a data.frame
#'
#' @description A family of functions to analyze a data.frame and output the
#' descriptive analysis results and/or visualization.
#'
#' @details \code{describe_df} is a wrapper of \code{cal_df_distrib} and
#' \code{vis_df_distrib}. \itemize{
#'  \item \code{cal_df_distrib}, built on top of \code{describe_num_cols} and
#'   \code{describe_chr_cols}, returns two lists: one for quantitative variables
#'   and the other for qualitative ones. \cr
#'  \item \code{vis_df_distrib}, built on top of \code{vis_num_cols} and
#'   \code{vis_chr_cols}, returns two sets of plots: one for quantitative variables
#'   and the other for qualitative ones, each of which contains one or multiple
#'   \pkg{ggplots} objects.
#' }
#'
#' @param data a data.frame
#' @param qual_vars variable(s) indicating the qualitative variable(s).
#' It accepts five forms: \itemize{
#' \item missing (default), the function will automatically identify the qualitative variable
#' \item character vector indicating variable names, e.g., \code{c("cyl", "gear")}
#' \item integer vector indicating variable index, e.g., \code{c(2, 10)}
#' \item quosures yielded using \code{\link[dplyr]{vars}}(), e.g, \code{vars(cyl, gear)}
#' \item NULL indicating that the function will not process qualitatitive variables
#' }
#' @param quan_vars variable(s) indicating the quantitative variable(s).
#' It accepts five forms: \itemize{
#' \item missing (default), the function will automatically identify the quantitative variables
#' \item character vector indicating variable names, e.g., \code{c("drat", "mpg")}
#' \item integer vector indicating variable index, e.g., \code{c(1, 5)}
#' \item quosures yielded using \code{\link[dplyr]{vars}}(), e.g, \code{vars(drat, mpg)}
#' \item NULL indicating that the function will not process quantitative variables
#' }
#' @param nrow NULL or positive integer. Only applicable for \code{vis_df_distrib},
#' determining the number of rows in the plot grid. See \code{\link[ggplot2]{facet_wrap}}
#' for more details.
#' @param ncol NULL or positive integer. Only applicable for \code{vis_df_distrib},
#' determining the number of columns in the plot grid. See \code{\link[ggplot2]{facet_wrap}}
#' for more details.
#' @param ... other arguments to pass to \code{describe_df}, \code{cal_df_distrib}
#' and \code{vis_df_distrib}. Accepts the following:
#' \itemize{
#'  \item pass to \code{cal_df_distrib} \describe{
#'   \item{argument for \code{choose_col_idx}}{diversity_threshold}
#'   }
#'  \item pass to \code{vis_df_distrib} \describe{
#'   \item{\code{help_on_dots}}{logical. If TRUE, you can get help info for
#'     \code{...} argument.}
#'   \item{elipsis arguments for \code{\link[ggplot2]{geom_histogram}}}{hist.title,
#'     hist.tag, hist.subtitle, hist.stat, hist.position, hist.bins, hist.color,
#'     hist.inherit.aes}
#'   \item{elipsis arguments for histograms and \code{\link[ggplot2]{geom_bar}}}{
#'     \code{bar.stat, bar.position, bar.width, bar.binwidth, bar.fill, bar.color},
#'     \code{bar.size, bar.alpha, bar.na.rm, bar.show.legend, bar.inheirt.aes}}
#'   \item{elipsis arguments for \code{\link[ggplot2]{geom_vline}}}{
#'     \code{vline.color, vline.size, vline.alpha, vline.show.legend, vline.na.rm}}
#'   }
#' }
#' @return \code{describe_df}(): a list of results by \code{cal_df_distrib} and
#' \code{vis_df_distrib}
#'
#' @export
#' @importFrom rlang ensyms
#'
#' @examples
#' \dontrun{
#' describe_df(mtcars, vars(am, cyl, mpg, wt), NULL)
#'
#' # you can also detect the distribution of the data in clipboard
#' describe_df(parse_clipb())
#' }
describe_df <- function(data, qual_vars, quan_vars, nrow=1, ncol=1, ...){
    datnm <- tryCatch(ensyms(data), error=function(e) exprs(data))
    hist.sub <- ifnull(list(...)$hist.subtitle, paste("Source:", datnm))
    bar.sub <- ifnull(list(...)$bar.subtitle, paste("Source:", datnm))

    out_df <- cal_df_distrib(data, qual_vars, quan_vars, ...)
    out_gg <- vis_df_distrib(data, qual_vars, quan_vars, nrow, ncol,
                             hist.subtitle=hist.sub, bar.subtitle=bar.sub, ...)
    invisible(lapply(out_gg, function(gg){
        new_dev(...)
        print(gg)
    }))
    return(out_df)
}


#' @export
#' @return \code{cal_df_distrib}(): two lists of tibbles \describe{
#'   \item{'qual'}{a tibble with 4 columns: \code{<varname>, 'value', 'freq', 'prop'};
#'   or a character \code{"no match columns"}}
#'  \item{'quan'}{a tibble with 11 columns: \code{<varname>, 'count', 'n_na', 'p_na'},
#'    \code{'mean', 'sd', 'min', 'lower', 'median', 'higher', 'max'}; or a character
#'    \code{"no match columns"}}
#' }
#' @examples
#' \dontrun{
#' cal_df_distrib(iris)  ## or
#' library(dplyr)
#' cal_df_distrib(iris, quan_vars=vars(Sepal.Length, Petal.Length))
#' }
#' @rdname describe_df
cal_df_distrib <- function(data, qual_vars, quan_vars, ...){
    if (is.matrix(data)) data <- as.data.frame(data, stringsAsFactors=FALSE)
    stopifnot(inherits(data, "data.frame"))

    if (missing(quan_vars)) {
        quan_cols <- choose_num_col_idx(data, NULL)
    }else{
        quan_cols <- if (length(quan_vars) == 0) character(0) else
            choose_num_col_idx(data, quan_vars)
    }
    if (missing(qual_vars)) {
        qual_cols <- choose_chr_col_idx(data, NULL)
    }else{
        qual_cols <- if (length(qual_vars) == 0) character(0) else
            choose_chr_col_idx(data, qual_vars)
    }
    out_quan <- describe_num_cols(data, quan_cols)
    out_qual <- describe_chr_cols(data, qual_cols)
    return(list(`qual`=out_qual, `quan`=out_quan))
}

#' @export
#' @importFrom rlang ensyms exprs
#' @return \code{vis_df_distrib}(): two sets of plots \itemize{
#'  \item \code{nrow} and \code{ncol} are 1 respectively, then the distribution
#'   plots (histogram or bar plots) will be shown one by one. \cr
#'  \item either \code{nrow} or \code{ncol} is more than 1, then the plots will
#'   be organized in facetted plots (with \code{\link[ggplot2]{facet_wrap}()}). \cr
#'  \item If both \code{nrow} and \code{ncol} are NULL, then the plots will be
#'   organized into one plot
#' }
#' @examples
#' \dontrun{
#' library(dplyr)
#' vis_df_distrib(mtcars, qual=vars(am, cyl), quan=vars(mpg, wt))
#' }
#' @rdname describe_df
vis_df_distrib <- function(data, qual_vars, quan_vars, nrow=NULL, ncol=NULL, ...){
    datnm <- tryCatch(ensyms(data), error=function(e) exprs(data))
    if (is.matrix(data)) data <- as.data.frame(data, stringsAsFactors=FALSE)
    stopifnot(inherits(data, "data.frame"))

    hist.sub <- ifnull(list(...)$hist.subtitle, paste("Source:", datnm))
    bar.sub <- ifnull(list(...)$bar.subtitle, paste("Source:", datnm))

    if (missing(quan_vars)) {
        quan_cols <- choose_num_col_idx(data, NULL, ...)
    }else{
        quan_cols <- if (length(quan_vars) == 0) character(0) else
            choose_num_col_idx(data, quan_vars, ...)
    }
    if (missing(qual_vars)) {
        qual_cols <- choose_chr_col_idx(data, NULL)
    }else{
        qual_cols <- if (length(qual_vars) == 0) character(0) else
            choose_chr_col_idx(data, qual_vars)
    }
    out_quan <- vis_num_cols(data, quan_cols, nrow=nrow, ncol=ncol,
                             hist.subtitle=hist.sub, ...)
    out_qual <- vis_chr_cols(data, qual_cols, nrow=nrow, ncol=ncol,
                             bar.subtitle=bar.sub, ...)

    return(append(out_quan, out_qual))
}


#' @importFrom rlang is_quosures !!!
choose_col_idx <- function(data, vars=NULL, type=c("qual", "quan"),
                           diversity_threshold=function(x) max(5, log10(x))){
    # choose column index from a data.frame, depending on type
    # used for describe_df
    stopifnot(is.data.frame(data) || is.matrix(data))
    stopifnot(is.null(vars) || is.character(vars) || is.numeric(vars) ||
                  is_quosures(vars))
    stopifnot(is.function(diversity_threshold))
    type <- match.arg(type)

    if (! is.null(vars)){
        cols <- if (is.numeric(vars)){
                round(vars, 0)
            }else if (is_quosures(vars)){
                which(names(data) %in% (data %>% select(!!! vars) %>% names))
            }else{
                vapply(vars, function(v){
                        which(grepl(paste0("^", v), names(data)))[1L]
                    }, FUN.VALUE=integer(1L))
            }
    }else{
        cols <- vapply(data, function(col) {
            if (type == "quan"){
                class(col)[length(class(col))] %in% c("integer", "numeric")
            }else{
                (! class(col)[length(class(col))] %in% c("integer", "numeric")) |
                    length(unique(col)) < diversity_threshold(length(col))
            }
        }, FUN.VALUE=logical(1L)) %>% which
    }
    return(sort(unique(cols)))
}


choose_chr_col_idx <- function(data, vars=NULL, diversity_threshold=function(x){
    max(5, log10(x))})
    choose_col_idx(data, vars, type="qual", diversity_threshold)

choose_num_col_idx <- function(data, vars=NULL, diversity_threshold=function(x){
    max(5, log10(x))})
    choose_col_idx(data, vars, type="quan", diversity_threshold)

#' @importFrom tidyr gather
#' @importFrom rlang is_quosures !!!
describe_given_cols <- function(data, cols, type=c("qual", "quan")){
    type <- match.arg(type)
    stopifnot(is.numeric(cols) || is.character(cols) || is_quosures(cols))

    fun <- switch(type, quan=describe_numcol, qual=describe_chrcol)
    if (length(cols) == 0) {
        return(paste0("No ", switch(type, qual="non-", quan=""), "numeric ",
                      "columns matched."))
    }else{
        o <- if (is_quosures(cols)) data %>% select(!!! cols) else
            data[, cols, drop=FALSE]
        return(o %>% cbind(rowid=rownames(data)) %>%
                   gather(varname, value, -rowid) %>%
                   fun(varname, value))
    }
}


describe_chr_cols <- function(data, cols)
    describe_given_cols(data, cols, "qual")

describe_num_cols <- function(data, cols)
    describe_given_cols(data, cols, "quan")


#' @importFrom rlang is_quosures !!! ensyms
vis_given_cols <- function(data, cols, type=c("qual", "quan"), nrow=NULL, ncol=NULL,
                           ...){
    # return a list
    datnm <- tryCatch(ensyms(data), error=function(e) exprs(data))
    type <- match.arg(type)
    stopifnot(is.numeric(cols) || is.character(cols) || is_quosures(cols))
    stopifnot(is.null(nrow) || (is.numeric(nrow) && nrow > 0))
    stopifnot(is.null(ncol) || (is.numeric(ncol) && ncol > 0))
    hist.sub <- ifnull(list(...)$hist.subtitle, paste("Source:", datnm))
    bar.sub <- ifnull(list(...)$bar.subtitle, paste("Source:", datnm))

    fun <- switch(type, quan=vis_numcol, qual=vis_chrcol)
    if (length(cols) == 0) {
        return(paste0("No ", switch(type, qual="non-", quan=""), "numeric ",
                      "columns matched."))
    }else{
        dat <- if (is_quosures(cols)) data %>% select(!!! cols) else
            data[, cols, drop=FALSE]
        dat <- dat %>% cbind(rowid=rownames(data)) %>%
            gather(varname, value, -rowid)

        if (! (is.null(nrow) || is.null(ncol))) {
            varnms <- unique(dat$varname)
            splitter <- mapply(rep, seq_len(ceiling(length(varnms)/(nrow*ncol))),
                               nrow*ncol) %>% as.vector
            varnms <- split(varnms, splitter[seq_len(length(varnms))])
            o <- lapply(varnms, function(varnm){
                dat <- dat %>% filter(varname %in% varnm)
                fun(dat, value, varname, nrow=nrow, ncol=ncol,
                    hist.subtitle=hist.sub, bar.subtitle=bar.sub, ...)
            })
        }else{
            o <- list(fun(dat, value, varname, nrow=nrow, ncol=ncol,
                          hist.subtitle=hist.sub, bar.subtitle=bar.sub, ...))
        }
        return(o)
    }
}

#' @param cols either integer (variable index), character (variable names) or
#' quosures yielded by \code{\link{vars}}. This argument is for \code{vis_chr_cols},
#' \code{vis_num_cols}, \code{describe_num_cols} and \code{describe_chr_cols}.
vis_chr_cols <-  function(data, cols, nrow=NULL, ncol=NULL, ...)
    vis_given_cols(data, cols, type="qual", nrow, ncol, ...)

vis_num_cols <-  function(data, cols, nrow=NULL, ncol=NULL, ...)
    vis_given_cols(data, cols, type="quan", nrow, ncol, ...)


#' @importFrom dplyr group_by summarize mutate
#' @importFrom rlang sym enquo !! as_data_mask
describe_numcol <- function(data, group, value){
    # data: a data.frame gather-ed by tidyr
    # group_by: var name to group_by, chr or sym
    # value: var name to summarize, chr or sym
    stopifnot(inherits(data, 'data.frame'))

    v <- enquo_args(group, value, datamask=as_data_mask(data)) %>%
        setNames(c("by", "on"))

    if (!is.null(v$by)) data <- data %>% group_by(!! v$by)
    out <- data %>% summarize(
        count=n(), n_na=sum(is.na(!! v$on)), p_na=0,
        mean=mean(!! v$on, na.rm=TRUE), sd=sd(!! v$on, na.rm=TRUE),
        min=fivenum(!! v$on, na.rm=TRUE)[1],
        lower=fivenum(!! v$on, na.rm=TRUE)[2],
        median=fivenum(!! v$on, na.rm=TRUE)[3],
        higher=fivenum(!! v$on, na.rm=TRUE)[4],
        max=fivenum(!! v$on, na.rm=TRUE)[5]) %>%
        mutate(p_na=n_na/count)
    return(out)
}


#' @importFrom dplyr group_by summarize mutate
#' @importFrom rlang sym enquo as_data_mask !!
describe_chrcol <- function(data, group, value){
    # data: a data.frame gather-ed by tidyr
    # group_by: var name to group_by, chr or sym
    # value: var name to summarize, chr or sym
    stopifnot(inherits(data, 'data.frame'))

    v <- enquo_args(group, value, datamask=as_data_mask(data)) %>%
        setNames(c("by", "on"))

    if (!is.null(v$by)) data <- data %>% group_by(!! v$by, add=TRUE)
    if (!is.null(v$on)) data <- data %>% group_by(!! v$on, add=TRUE)

    out <- data %>%
        summarize(freq=n()) %>% mutate(prop=freq/sum(freq)) %>%
        arrange(-freq, .by_group=TRUE)
    return(out)
}

#' @importFrom ggplot2 ggplot geom_histogram geom_vline facet_wrap aes_string aes vars labs
#' @importFrom ggplot2 waiver
#' @importFrom rlang as_data_mask !! quo_text ensyms
#' @importFrom crayon silver blue
vis_numcol <- function(data, stat_by, facet_by, xlab="Value", ylab="Count",
                        statdf=NULL, ...){
    # ... Accepts: hist.title, hist.tag, hist.subtitle, hist.stat, hist.position,
    # hist.bins, hist.color, hist.inherit.aes, bar.size, bar.alpha, bar.na.rm,
    # bar.show.legend, vline.color, vline.size, vline.alpha,
    # vline.show.legend, vline.na.rm, nrow, ncol

    args <- tryCatch(ensyms(data, stat_by, facet_by), error=function(e)
        exprs(data, stat_by, facet_by))
    v <- enquo_args(args[[2]], args[[3]], datamask=as_data_mask(data)) %>%
        setNames(c("on", "by"))

    if (is.null(statdf)) statdf <- describe_numcol(data, v$by, v$on)
    stopifnot(all(c("count", "n_na", "p_na", "mean", "sd", "median", "higher") %in%
                      names(statdf)))

    size_coef <- 1.5 / sqrt(nrow(statdf))
    dots_list <- unlist(list(
        hist=list(stat="bin", position="identity", bins=13, fill="deepskyblue",
                  color="White", inherit.aes=TRUE),
        bar=list(size=size_coef, alpha=0.5, na.rm=FALSE, show.legend=NA),
        vline=list(color="tomato3", size=1.2*size_coef, alpha=0.8,
                   na.rm=FALSE, show.legend=TRUE),
        nrow=NULL, ncol=NULL),
        recursive=FALSE)
    dots <- mergeList(dots_list, list(...), keep_null=TRUE)
    if (ifnull(dots$help_on_dots, FALSE))
        invisible(cat(c(blue("... accepts the following arguments:\n"),
                        silver(paste(names(dots_list), collapse=", ")), "\n")))

    gg <- if (is.null(dots$mapping)) ggplot(data=data) else
        ggplot(data=data, mapping=dots$mapping)
    gg <- gg +
        labs(x=xlab, y=ylab, caption=if (is.null(dots$hist.caption)) paste(c(
            "Stats of ", quo_text(v$on), " (faceted by ", quo_text(v$by), ")"),
            collapse="") else dots$hist.caption,
            title=if (is.null(dots$hist.title))
                "Plot: distribution of quantitative variables" else dots$hist.title,
            tag=if (is.null(dots$hist.tag)) waiver() else dots$hist.tag,
            subtitle=if (is.null(dots$hist.subtitle)) paste(
                "Source:", args[[1]]) else dots$hist.subtitle) +
        geom_histogram(
            aes(x=!! v$on), stat=dots$hist.stat,
            position=dots$hist.position, bins=dots$hist.bins,
            fill=dots$hist.fill,         color=dots$hist.color,
            size=dots$bar.size,          alpha=dots$bar.alpha,
            na.rm=dots$bar.na.rm,        show.legend=dots$bar.show.legend,
            inherit.aes=dots$hist.inherit.aes) +
        geom_vline(
            aes(xintercept=mean), data=statdf,
            color=dots$vline.color,      size=dots$vline.size,
            alpha=dots$vline.alpha,      na.rm=dots$vline.na.rm,
            show.legend=dots$vline.show.legend) +
        geom_vline(
            aes(xintercept=mean+sd),     data=statdf,
            color=dots$vline.color,      size=dots$vline.size,
            alpha=dots$vline.alpha,      linetype=2,
            na.rm=dots$vline.na.rm,      show.legend=dots$vline.show.legend) +
        geom_vline(
            aes(xintercept=mean-sd),     data=statdf,
            color=dots$vline.color,      size=dots$vline.size,
            alpha=dots$vline.alpha,      linetype=2,
            na.rm=dots$vline.na.rm,      show.legend=dots$vline.show.legend)
    if (length(v$by) > 0)
        gg <- gg + facet_wrap(vars(!! v$by), nrow=dots$nrow, ncol=dots$ncol)
    return(gg)
}

#' @importFrom ggplot2 ggplot geom_bar facet_wrap coord_flip aes_string aes vars labs waiver
#' @importFrom rlang as_data_mask !! quo_text ensyms
#' @importFrom crayon silver blue
vis_chrcol <- function(data, stat_by, facet_by, xlab="Value", ylab="Count",
                        ...){
    # accepts ...: bar.stat, bar.position, bar.width, bar.binwidth, bar.fill
    # bar.color, bar.size, bar.alpha, bar.na.rm, bar.show.legend, bar.inheirt.aes
    # nrow ncol
    args <- tryCatch(ensyms(data, stat_by, facet_by), error=function(e)
        exprs(data, stat_by, facet_by))
    v <- enquo_args(args[[2]], args[[3]], datamask=as_data_mask(data)) %>%
        setNames(c("on", "by"))

    if (! all(c("freq", "prop") %in% names(data)))
        data <- describe_chrcol(data, v$by, v$on)

    size_coef <- 1 / sqrt(nrow(data))
    dots_list <- unlist(list(bar=list(
        stat="identity", position="stack", width=NULL, binwidth=NULL,
        fill="tomato2", color="White", size=size_coef, alpha=0.5, na.rm=FALSE,
        show.legend=NA, inherit.aes=TRUE), nrow=NULL, ncol=NULL),
        recursive=FALSE)
    dots <- mergeList(dots_list, list(...), keep_null=TRUE)
    if (ifnull(dots$help_on_dots, FALSE))
        invisible(cat(c(blue("... accepts the following arguments:\n"),
                        silver(paste(names(dots_list), collapse=", ")), "\n")))

    gg <- if (is.null(dots$mapping)) ggplot(data=data) else
        ggplot(data=data, mapping=dots$mapping)
    gg <- gg + labs(x=xlab, y=ylab, caption=if (is.null(dots$bar.caption)) paste(c(
        "Stats of ", quo_text(v$on), " (faceted by ", quo_text(v$by), ")"),
        collapse="") else dots$bar.caption,
        title=if (is.null(dots$bar.title))
            "Plot: distribution of qualitative variables" else dots$bar.title,
        tag=if (is.null(dots$bar.tag)) waiver() else dots$bar.tag,
        subtitle=if (is.null(dots$bar.subtitle)) paste(
            "Source:", args[[1]]) else dots$bar.subtitle) +
        geom_bar(
            aes(x=!! v$on, y=freq), stat=dots$bar.stat,
            position=dots$bar.position,       width=dots$bar.width,
            binwidth=dots$bar.binwidth,       color=dots$bar.color,
            fill=dots$bar.fill,               size=dots$bar.size,
            alpha=dots$bar.alpha,             na.rm=dots$bar.na.rm,
            show.legend=dots$bar.show.legend, inherit.aes=dots$bar.inherit.aes) +
        coord_flip()
    if (length(v$by) > 0)
        gg <- gg + facet_wrap(vars(!! v$by), nrow=dots$nrow, ncol=dots$ncol)
    return(gg)
}

new_dev <- function(title="", width=8, height=6, xpos=125, ypos=125, ...){
    # OS-specific new device for plotting
    dev_fun <- switch(Sys.info()[['sysname']],
                      `Windows` = windows, `Darwin` = x11, `Linux` = quartz)
    dev_fun(width=width, height=height, xpos=xpos, ypos=ypos, title=title, ...)
}

