#' creates a data frame of unpopulated salesforce objects
#'
#' This function will reformat the data from an object list into a data frame.
#'
#' Notes and Limitations:
#'
#' Takes the output and filter to just get those list items which DON'T contain
#' object details
#'
#' @param objects_fields_list a \code{list} of the fields of the objects
#' @param objects_data_frame a \code{data frame} of the details of the objects
#'
#' @return a \code{data frame} of all the unpopulated objects and their fields
#'
#' @importFrom qdapTools list2df
#' @import dplyr
#' @importFrom magrittr %>%
#' @import checkmate
#'
#'
#' @examples
#' \dontrun{
#' to add
#' }

sf_unpopulated_objects <- function(objects_fields_list, objects_data_frame) {

  checkmate::assert_list(objects_fields_list)
  checkmate::assert_data_frame(objects_data_frame)

  objects_fields_list_idx <- which(lapply(objects_fields_list, typeof) != 'list')

  unpopulated_objects_fields_list <- objects_fields_list[objects_fields_list_idx]

  #rm(objects_fields_list_idx)

  fields_data_frame <- qdapTools::list2df(unpopulated_objects_fields_list)

  fields_data_frame <- dplyr::bind_rows(fields_data_frame)  %>%
    dplyr::select(object_name = .data$X2,
                  result = .data$X1)

  fields_data_frame  <- sf_bind_object_fields(objects_data_frame, fields_data_frame )

  return(fields_data_frame )
}
