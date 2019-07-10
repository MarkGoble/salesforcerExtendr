#' Gets all salesfroce objects from an org
#'
#' This function will pull back the information on all sObjects in the salesforce org.
#'
#' Notes and Limitations
#'
#' Objects such as Activity are not returned as these aren't Sobjects (they
#' are more akin to views). Also note that the 'URLs' are stripped from out as
#' these are nested and also not required. The function will only return objects
#' which it has permission to see.
#'
#'
#' Requires a connection to be established to salesforce.
#'
#' @return A \code{data frame} of objects and their fields
#'
#' @importFrom magrittr %>%
#' @importFrom salesforcer sf_list_objects
#' @importFrom qdapTools list2df
#' @importFrom tidyr separate
#' @importFrom dplyr filter
#' @importFrom tidyr spread
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sf_objects_info()
#' }


sf_objects_info <- function() {
  sf_objects <- salesforcer::sf_list_objects()
  # get the field information
  sf_object <- sf_objects[[3]]
  names(sf_object) <- 1:length(sf_object)
  sf_object <- unlist(sf_object, recursive = FALSE)
  # convert to dataframe and re-organise the data
  sf_object_df <- qdapTools::list2df(sf_object)
  sf_object_df <- sf_object_df %>% tidyr::separate(.data$X2, c("X", "X2"))
  sf_object_df <- sf_object_df %>% dplyr::filter(.data$X2 != "urls")

  t <- tidyr::spread(sf_object_df, .data$X2, .data$X1)
  return(t)
}
