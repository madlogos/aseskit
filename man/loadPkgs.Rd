% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg.R
\name{loadPkgs}
\alias{loadPkgs}
\alias{load_pkgs}
\alias{unloadPkgs}
\alias{unload_pkgs}
\title{Load/install or unload packages from various sources}
\usage{
loadPkgs(..., repo = c("cran", "c", "github", "g", "gh", "gitlab", "l",
  "gl", "bioconductor", "bioc", "b", "bitbucket", "k", "omegahat", "oh",
  "o"), url = NULL, quiet = TRUE)

unloadPkgs(..., force = FALSE, quiet = FALSE)
}
\arguments{
\item{...}{Strings or expressions representing packages to load/install or 
unload. In the function \code{unloadPkgs}, if \code{...} is not assigned, then
all the non-default packages (\pkg{base, methods, datasets, utils, grDevices, 
graphics, stats}) will be unloaded.}

\item{repo}{Currently only accepts 'cran'/'c', 'github'/'gh'/'g', 'gitlab'/'gl'/'l', 
'bioconductor'/'bioc'/'b', 'bitbucket'/'k', and 'omegahat'/'oh'/'o'. Refer to 
Details.}

\item{url}{If any of \code{pkgs} was not installed yet, and \code{url} is 
specified, \code{loadPkgs} will try to download and install the package from 
that url (It must point to a valid file on internet). 
Default to NULL.}

\item{quiet}{Logical, default TRUE for \code{loadPkgs} and FALSE for {unloadPkgs}.
If TRUE, the packages will be installed/loaded or unloaded(detached) quietly.}

\item{force}{Logical. Should a package be detached even though other attached
packages depend on it?}
}
\value{
\link{invisible}. If you set \code{quiet = FALSE}, the verbose
information will be printed out.
}
\description{
Load/unload one or more packages. When loading one or more packages,
if not already installed, try install it/them from various \code{repos}. If 
\code{url} is assigned, the package will also be downloaded and installed from 
that alternative url.
}
\details{
There are several groups of repos that you can apply: \describe{
\item{c}{\code{c} or \code{cran}, you can also use \code{install.packages()}}
\item{g}{\code{g} or \code{gh} or \code{github}, you can also use \pkg{remotes}::
\code{install_github}}
\item{b}{\code{b} or \code{bioc} or \code{bioconductor}, you can also use \pkg{remotes}::
\code{install_bioc}}
\item{k}{\code{k} or \code{bitbucket}, you can also use \pkg{remotes}::
\code{install_bitbucket}}
\item{l}{\code{l} or \code{gl} or \code{gitlab}, you can also use \pkg{remotes}::
\code{install_gitlab}}
\item{o}{\code{o} or \code{oh} or \code{omegahat}, you can also use \code{install.packages}
with \code{repos}='http://www.omegahat.org/R', \code{type}="source"}
} \cr
When you don't want to use the \code{repo}, you can directly assign \code{url} to 
download and install the pacakge(s).
}
\examples{
\dontrun{
## Load existing package(s)
loadPkgs(car)
loadPkgs(car, cluster)  # identical with
loadPkgs("car", "cluster")  # or
loadPkgs(c("car", "cluster"))

## Unload them simultaneously
unloadPkgs(car, cluster)

## Install a new package explcitly from CRAN and then load it
loadPkgs("tidyr", repo="c")

## Install a new package from other sources and load it
loadPkgs(hadley/svglite, hadley/purrr, repo="github")
loadPkgs(a4Base, repo="bioc")
}
}
\author{
Yiying Wang, \email{wangy@aetna.com}
}
