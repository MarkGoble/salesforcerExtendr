#'  Add ons for the excellent salesforcer package
#'
#' A collection of additional helpers functions to extend the salesforcer package
#'
#' @docType package
#' @name salesforcerExtendr
#' @author Mark Goble \email{mark.goble@@goble.co.uk}
"_PACKAGE"

# added to resolve warning no visible binding for global variable ‘.’
# see: https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
