#' Binds a salesforce object to it's fields
#'
#' This helper function will bind the fields to their object descriptions to get the final data frame.
#'  For internal use within the package
#'
#' Notes and Limitations:
#' none
#'
#' @param objects_data_frame a \code{data frane} of object information
#' @param fields_data_frame a \code{data frame} of the field information
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import checkmate
#'
#'
#' @examples
#' \dontrun{
#' to add
#' }


sf_bind_object_fields <- function(objects_data_frame, fields_data_frame) {

  checkmate::assert_data_frame(objects_data_frame)
  checkmate::assert_data_frame(fields_data_frame)

  # get the name and label from the objects
  object_names <- objects_data_frame %>%
    dplyr::select(object_name = .data$name, object_label = .data$label)

  joined_info <- dplyr::inner_join(object_names, fields_data_frame) %>%
    #shunt the object label to the first column
    dplyr::select(.data$object_label, everything())

  return(joined_info)

}
