#------------Require packages, if not installed, install it--------------
#' Load/install or unload packages from various sources
#'
#' Load/unload one or more packages. When loading one or more packages,
#' if not already installed, try install it/them from various \code{repos}. If 
#' \code{url} is assigned, the package will also be downloaded and installed from 
#' that alternative url.
#' @details There are several groups of repos that you can apply: \describe{
#' \item{c}{\code{c} or \code{cran}, you can also use \code{install.packages()}}
#' \item{g}{\code{g} or \code{gh} or \code{github}, you can also use \pkg{remotes}::
#' \code{install_github}}
#' \item{b}{\code{b} or \code{bioc} or \code{bioconductor}, you can also use \pkg{remotes}::
#' \code{install_bioc}}
#' \item{k}{\code{k} or \code{bitbucket}, you can also use \pkg{remotes}::
#' \code{install_bitbucket}}
#' \item{l}{\code{l} or \code{gl} or \code{gitlab}, you can also use \pkg{remotes}::
#' \code{install_gitlab}}
#' \item{o}{\code{o} or \code{oh} or \code{omegahat}, you can also use \code{install.packages}
#' with \code{repos}='http://www.omegahat.org/R', \code{type}="source"}
#' } \cr
#' When you don't want to use the \code{repo}, you can directly assign \code{url} to 
#' download and install the pacakge(s).
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param ... Strings or expressions representing packages to load/install or 
#' unload. In the function \code{unloadPkgs}, if \code{...} is not assigned, then
#' all the non-default packages (\pkg{base, methods, datasets, utils, grDevices, 
#' graphics, stats}) will be unloaded.
#' @param repo Currently only accepts 'cran'/'c', 'github'/'gh'/'g', 'gitlab'/'gl'/'l', 
#' 'bioconductor'/'bioc'/'b', 'bitbucket'/'k', and 'omegahat'/'oh'/'o'. Refer to 
#' Details.
#' @param url If any of \code{pkgs} was not installed yet, and \code{url} is 
#' specified, \code{loadPkgs} will try to download and install the package from 
#' that url (It must point to a valid file on internet). 
#' Default to NULL.
#' @param quiet Logical, default TRUE for \code{loadPkgs} and FALSE for {unloadPkgs}.
#' If TRUE, the packages will be installed/loaded or unloaded(detached) quietly.
#'
#' @return \link{invisible}. If you set \code{quiet = FALSE}, the verbose
#' information will be printed out.
#' @export
#' @aliases load_pkgs
#' @importFrom remotes install_bioc install_cran install_github install_gitlab
#'
#' @examples
#' \dontrun{
#' ## Load existing package(s)
#' loadPkgs(car)
#' loadPkgs(car, cluster)  # identical with
#' loadPkgs("car", "cluster")  # or
#' loadPkgs(c("car", "cluster"))
#'
#' ## Unload them simultaneously
#' unloadPkgs(car, cluster)
#'
#' ## Install a new package explcitly from CRAN and then load it
#' loadPkgs("tidyr", repo="c")
#'
#' ## Install a new package from other sources and load it
#' loadPkgs(hadley/svglite, hadley/purrr, repo="github")
#' loadPkgs(a4Base, repo="bioc")
#' }
loadPkgs <- function(
    ...,
    repo=c('cran', 'c', 'github', 'g', 'gh', 'gitlab', 'l', 'gl', 'bioconductor', 
           'bioc', 'b', 'bitbucket', 'k', 'omegahat', 'oh', 'o'),
    url=NULL, quiet=TRUE
){
    
    pkgs <- lapply(substitute(list(...)), as.character)
    pkgs <- pkgs[2:length(pkgs)]
    pkgs <- lapply(pkgs, function(v) if (v[1] %in% c("list", "c")) v[2:length(v)]
                   else v)
    pkgs <- unlist(pkgs)
    
    repo <- tolower(match.arg(repo, several.ok = TRUE))
    if (! is.null(url)) {
        stopifnot(length(pkgs) == length(url))
        mapply(loadPkg, pkg=pkgs, MoreArgs=list(repo=repo, url=url, quiet=quiet))
    }else{
        mapply(loadPkg, pkg=pkgs, MoreArgs=list(repo=repo, quiet=quiet))
    }
    invisible()
}

#' @export
load_pkgs <- loadPkgs

#' @importFrom remotes install_bioc install_cran install_github install_gitlab
#' @importFrom remotes install_bitbucket
#' @importFrom devtools loaded_packages
#' @importFrom stringr str_split
loadPkg <- function(pkg, repo = c(
    'cran', 'c', 'github', 'g', 'gh', 'gitlab', 'l', 'gl', 'bioconductor', 
    'bioc', 'b', 'bitbucket', 'k', 'omegahat', 'oh', 'o'), 
    url = NULL, quiet = TRUE,
...){
    stopifnot(is.character(pkg))
    if (length(pkg) > 1){
        pkg <- pkg[[1]]
        warning(paste("Only load the first package you required. ",
                      "Use loadPkgs() if you want to load multiple packages."))
    }
    repo <- tolower(match.arg(repo, several.ok=TRUE))
    
    if (str_detect(pkg, ".+/.+")){
        pkg_type <- "github"
        pkgurl <- unlist(str_split(pkg, "/"))[1]
        pkg <- unlist(str_split(pkg, "/"))[2]
    }else{
        pkg_type <- "cran"
    }
    
    chk_inst <- function(pkgname=pkg)
        return(pkgname %in% installed.packages()[, "Package"])
    need_inst <- ! chk_inst(pkg)
    
    # if not installed
    if (pkg_type == "cran"){
        if (! chk_inst(pkg) && any(repo %in% c('cran', 'c')))
            try(install_cran(pkg, quiet = quiet), silent=TRUE)
        if (! chk_inst(pkg) && any(repo %in% c('bioconductor', 'bioc', 'b')))
            try(install_bioc(pkg), silent=TRUE)
        if (! chk_inst(pkg) && any(repo %in% c('omegahat', 'oh', 'o')))
            try(install.packages(pkg, repos = "http://www.omegahat.org/R",
                                 type="source", quiet=quiet), silent=TRUE)
    }else if (pkg_type == "github"){
        if (! chk_inst(pkg) && any(repo %in% c('github', 'gh', 'g')))
            try(install_github(paste(pkgurl, pkg, sep="/"), quiet = quiet),
                silent=TRUE)
        if (! chk_inst(pkg) && any(repo %in% c('gitlab', 'l', 'gl')))
            try(install_gitlab(paste(pkgurl, pkg, sep="/"), quiet = quiet),
                silent=TRUE)
        if (! chk_inst(pkg) && any(repo %in% c('bioconductor', 'bioc', 'b')))
            try(install_bioc(paste(pkgurl, pkg, sep="/")), silent=TRUE)
        if (! chk_inst(pkg) && any(repo %in% c('bitbucketr', 'k')))
            try(install_bitbucket(paste(pkgurl, pkg, sep="/")), silent=TRUE)
    }
    
    if (! chk_inst(pkg) && length(url)>0){
        to_inst <- try(install_url(url, quiet=quiet), silent=TRUE)
        if (inherits(to_inst, "character")){
            if (to_inst != pkg)
                warning(to_inst, "instead of", pkg, 
                        "was installed via the url given")
        }
    }
    
    if (! chk_inst(pkg))
        warning(pkg, "cannot be installed from the repos and url you assigned.")
    
    if (! pkg %in% loaded_packages()[, "package"])
        library(pkg, character.only = TRUE, quietly = quiet)
    if (! quiet) {
        if (need_inst) message("Install and load package: ", pkg) else
        message("Load package:\t\t\t ", pkg)
    }
}

#' @param force Logical. Should a package be detached even though other attached
#' packages depend on it?
#' @importFrom devtools loaded_packages
#' @export
#' @aliases unload_pkgs
#' @rdname loadPkgs
unloadPkgs <- function(..., force=FALSE, quiet=FALSE){
    pkgs <- lapply(substitute(list(...)), as.character)
    pkgs <- pkgs[2:length(pkgs)]
    pkgs <- lapply(pkgs, function(v) if (v[1] %in% c("list", "c")) v[2:length(v)]
                   else v)
    pkgs <- unlist(pkgs)
    
    if (length(pkgs) == 0){  # NULL
        pkgs <- loaded_packages()[["package"]]
        pkgs <- pkgs[! pkgs %in% c("base", getOption("defaultPackages"))]
    }
    mapply(unloadPkg, pkg=pkgs, MoreArgs=list(force=force, quiet=quiet))
    invisible()
}

#' @export
unload_pkgs <- unloadPkgs


#' @importFrom devtools loaded_packages
unloadPkg <- function(pkg, force=FALSE, quiet=TRUE) {
    stopifnot(all(is.character(pkg)))
    if (length(pkg) > 1){
        pkg <- pkg[[1]]
        warning(paste("Only unload the first package you required. ",
                      "Use unloadPkgs() if you want to unload multiple packages."))
    }
    if (pkg %in% loaded_packages()[['package']]){
        detach(paste0("package:", pkg), unload=TRUE, character.only=TRUE,
               force=force)
        if (! quiet) message("Detach package: ", pkg)
    }
}



#' Add Rtools to System PATH
#'
#' Rtools is very important. If it is not imported to PATH, you need to
#' run this function to apply. You can manually download and install Rtools and
#' export them to System PATH.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param rtools.root Where Rtools is installed.
#'
#' @return Nothing
#'
#' @importFrom stringr str_detect
#' @importFrom pkgbuild has_rtools rtools_path
#' @export
#'
#' @examples
#' \dontrun{
#' addRtoolsPath()
#' }
addRtoolsPath <- function(rtools.root=NULL){  # in order to use pkg working with Rtools
    if (Sys.info()['sysname'] != 'Windows') invisible(return(NULL))
    if (! has_rtools()){
        stop("You should installr::install_rtools() first.")
    }else{
        # -------ensure Rtools is included in Sys.Path-------
        machine <- if (Sys.info()[['machine']] == "x86") "32" else "64"
        if (is.null(rtools.root)) {
            keys <- NULL
            try(keys <- utils::readRegistry(
                "SOFTWARE\\R-core\\Rtools", hive = "HCU",
                view = paste0(machine, "-bit"), maxdepth = 2), silent = TRUE)
            if (is.null(keys))
                try(keys <- utils::readRegistry(
                    "SOFTWARE\\R-core\\Rtools", hive = "HLM",
                    view = paste0(machine, "-bit"), maxdepth = 2), silent = TRUE)
            if (!is.null(keys)){
                rtools.root <- keys[[keys$`Current Version`]]$InstallPath
            }else{
                rtools.root <- gsub("/", "\\\\", dirname(rtools_path()))
            }
        }
        root.bin.path <- paste(rtools.root, "bin", sep="\\")

        if (R.Version()$os == "mingw32"){
            mingw.path <- paste(
                rtools.root, "mingw_32\\bin", sep="\\")
            gcc.path <- paste(
                rtools.root, "mingw_32\\libexec\\gcc\\i686-w64-mingw32\\4.9.3",
                sep="\\")
        }else if (R.Version()$os == "mingw64"){
            mingw.path <- paste(
                rtools.root, "mingw_64\\bin", sep="\\")
            gcc.path <- paste(
                rtools.root, "mingw_64\\libexec\\gcc\\x86_64-w64-mingw32\\4.9.3",
                sep="\\")
        }
        PATH <- Sys.getenv("PATH")
        if (! str_detect(PATH, ";$"))
            PATH <- paste0(PATH, ';')  # remove dirmark at the end
        if (! str_detect(PATH, paste0("\\Q", rtools.root, "\\E")))
            Sys.setenv(PATH=paste(Sys.getenv("PATH"), rtools.root, sep=";"))
        if (! str_detect(PATH, paste0("\\Q", root.bin.path, "\\E")))
            Sys.setenv(PATH=paste(Sys.getenv("PATH"), root.bin.path, sep=";"))
        if (! str_detect(PATH, paste0("\\Q", gcc.path, "\\E")))
            Sys.setenv(PATH=paste(Sys.getenv("PATH"), gcc.path, sep=";"))
        if (! str_detect(PATH, paste0("\\Q", mingw.path, "\\E")))
            Sys.setenv(PATH=paste(Sys.getenv("PATH"), mingw.path, sep=";"))
    }
}
