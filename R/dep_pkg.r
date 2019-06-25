#' Get dependent packages for the given packages
#'
#' Get a tibble of packages that the given packages depend on or import.
#' It makes use of \pkg{utils}::\code{\link[utils]{available.packages}} to get the full
#' package meta info list.
#'
#' This function requires internet connection.
#' @param ... package names for process. Can be in the following forms: \describe{
#' \item{character vector/list}{E.g., c("tidyr", "dplyr") or list("tidyr", "dplyr")}
#' \item{characters}{E.g., "tidyr", "dplyr"}
#' \item{symbols}{E.g., tidyr, dplyr}
#' }
#' @param dep_type character, type of dependency, accecpts c("Depends", "Imports",
#' "LinkingTo", "Suggests", "Enhances"). Default c('Depends', 'Imports', 'LinkingTo').
#' Character string "all" is shorthand for that vector, character string "most"
#' for the same vector without "Enhances".
#' @param repos repos address, default \code{getOption("repos")}
#' @param recursive logical, whether recursive when fetching package dependencies.
#' Default FALSE.
#'
#' @return a tibble of dependent/import packages of the given \code{packages}. It has
#' 4 columns: \describe{
#' \item{package}{dependent package names}
#' \item{cran}{package version on CRAN}
#' \item{local}{package version locally}
#' \item{behind}{whether local version is behind CRAN}
#' }
#' @importFrom remotes available_packages
#' @export
#' @seealso \code{\link[tools]{package_dependencies}}
#' @examples
#' \dontrun{
#' dep_pkgs(c("dplyr", "devtools"))  # or
#' dep_pkgs("dplyr", "devtools")
#' }
dep_pkgs <- function(..., dep_type=c("Depends", "Imports", "LinkingTo"),
                     repos=getOption("repos"), recursive=FALSE){
    # Args
    pkgs <- as.character(substitute(list(...)))[-1]
    if (pkgs[1] %in% c("c", "list")) pkgs <- pkgs[-1]

    dep_type <- match.arg(dep_type, c(
        "Depends", "Imports", "LinkingTo", "Suggests",  "Enhances", "most", "all"),
        several.ok=TRUE)
    if ("all" %in% dep_type)
        dep_type <- unique(c(dep_type[-which(dep_type=="all")], c(
            "Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
    if ("most" %in% dep_type)
        dep_type <- unique(c(dep_type[-which(dep_type=="most")], c(
            "Depends", "Imports", "LinkingTo", "Suggests")))

	# available_pkgs
    avpkg <- remotes::available_packages(repos=repos)
    avpkg <- add_local_dep_pkg(pkgs, avpkg)

	if (! any(pkgs %in% rownames(avpkg))) {
		warning(pkgs, " cannot be found in available.packags(), and no package ",
		        "dependencies info can be found in DESCRIPTION.")
		return(NULL)
	}else{
	    matched_pkgs <- intersect(pkgs, avpkg[, "Package"])
		if (length(pkgs) != length(matched_pkgs))
			warning(pkgs[! pkgs %in% matched_pkgs], " not matched in ",
			        "available.packages() or not being valid packages.")
		pkgs <- matched_pkgs
	}

    .dep_pkgs(pkgs, dep_type=dep_type, available_pkgs=avpkg, recursive=recursive)
}

#' @export
#' @rdname dep_pkgs
dep_pkg <- dep_pkgs

#' @importFrom dplyr if_else
#' @importFrom purrr map2_lgl map_chr
#' @importFrom tibble tibble
#' @importFrom tools package_dependencies
.dep_pkgs <- function(pkgs, dep_type=c("Depends", "Imports", "LinkingTo"),
                      available_pkgs=add_local_dep_pkg(pkgs), recursive=FALSE){
    # inspired by tidyverse::tidyverse_deps()

    stopifnot(is.character(pkgs))

    deps <- tools::package_dependencies(pkgs, available_pkgs, which=dep_type,
                                        recursive=recursive)
    pkg_deps <- unique(sort(unlist(deps)))

    base_pkgs <- c(
        "base", "compiler", "datasets", "graphics", "grDevices", "grid",
        "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
        "utils"
    )
    pkg_deps <- setdiff(pkg_deps, base_pkgs)

    # sometimes pkg_deps are not in available_packages(), return '0.0.0'
    avpkg_ver <- structure(as.list(rep('0.0.0', length(pkg_deps))),
                           names=pkg_deps)
    vld_ver <- pkg_deps %in% rownames(available_pkgs)
    if (any(vld_ver)){
        avpkg_ver[vld_ver] <- available_pkgs[pkg_deps[vld_ver], "Version"]
    }
    cran_version <- lapply(avpkg_ver, base::package_version)

    # sometimes the pkg_deps are not installed locally, return '0.0.0'
    local_version <- lapply(pkg_deps, function(pkgnm)
        tryCatch(packageVersion(pkgnm), error=function(e) '0.0.0',
                 warn=function(e) '0.0.0', finally=invisible()))

    behind <- purrr::map2_lgl(cran_version, local_version, `>`)

    tibble::tibble(
        package = pkg_deps,
        cran = cran_version %>% purrr::map_chr(function(x)
            dplyr::if_else(x=='0.0.0', NA_character_, as.character(x))),
        local = local_version %>% purrr::map_chr(function(x)
            dplyr::if_else(x=='0.0.0', NA_character_, as.character(x))),
        behind = behind
    )
}


#' @importFrom dplyr bind_rows filter
add_local_dep_pkg <- function(pkgs, available_pkgs=remotes::available_packages()){
    # Add local available_pkgs from DESCRIPTION to available.packages()
    dcf <- lapply(pkgs, function(pkg)
        suppressWarnings(tryCatch(
            as.data.frame(read.dcf(system.file("DESCRIPTION", package=pkg))),
            error=function(e) NULL, warn=function(e) NULL, finally=invisible())))
    dcf <- bind_rows(dcf)
    if (ncol(dcf) > 0){
        dcf <- dcf[, intersect(names(dcf), colnames(available_pkgs)), drop=FALSE] %>%
            filter(! Package %in% rownames(available_pkgs))
        out <- as.matrix(bind_rows(as.data.frame(available_pkgs), dcf))
        rownames(out) <- out[, "Package"]
    }else{
        out <- available_pkgs
    }
    return(out)
}



