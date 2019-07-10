#' creates a data frame of populated salesforce objects and their fields
#'
#' This function will reformat the data from an object list into a data frame
#'
#' Notes and Limitations:
#'
#' Takes the output and filter to just get those list items which contain object details
#'
#' @param objects_fields_list a \code{list} of the objects fields
#' @param objects_data_frame a \code{data frame} of object information
#'
#' @return a \code{data frame} of all the populated objects and their fields
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

# 03/jul/19
# - corrected typo in field_name (was field_lame)

sf_populated_objects <- function(objects_fields_list, objects_data_frame) {

  checkmate::assert_list(objects_fields_list)
  checkmate::assert_data_frame(objects_data_frame)

  objects_fields_list_idx <- which(lapply(objects_fields_list, typeof) == 'list')

  populated_objects_fields_list <- objects_fields_list[objects_fields_list_idx]

  rm(objects_fields_list_idx)

  # get the populated objects and join the label
  fields_data_frame <- dplyr::bind_rows(populated_objects_fields_list) %>%
    # reorder the columns
    dplyr::select(object_name = .data$object,
           field_label = .data$label,
           field_name = .data$fullName,
           field_description = .data$description,
           data_type = .data$type,
           data_length = .data$length,
           data_precision = .data$precision)

  fields_data_frame <- sf_bind_object_fields(objects_data_frame, fields_data_frame)

  return(fields_data_frame)
}
