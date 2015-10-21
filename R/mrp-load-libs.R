#' Quickly and quietly load required libraries for MRP
#'
#' Loads all required libraries necessary for conducting multilevel regression
#' and poststratification analysis.
#'
#' @param install logical: if \code{TRUE} installs all required packages.
#'   Default is \code{FALSE}
#' @param quiet logical: if \code{TRUE} suppresses all package startup messages.
#' @export
#' @examples
#' \donttest{
#' ## Load libraries
#' mrp_load_libs()
#' ## with startup messages
#' mrp_load_libs(quiet = FALSE)
#' ## and install packages
#' mrp_load_libs(install = TRUE)
#' }
mrp_load_libs <- function (install = FALSE, quiet = TRUE)
{
  pkgs <- c('car', 'lme4', 'arm', 'foreign',
            'blme', 'openintro', 'plyr', 'dplyr')
  if (install) {
    install.packages(pkgs)
  }
  for (i in seq_along(pkgs)) {
    if (quiet)
      suppressPackageStartupMessages(library(pkgs[i], character.only = TRUE))
    else library(pkgs[i], character.only = TRUE)
  }
}