#' oterates over a collection of salesforce objects to get the fields for each
#'
#' This function will iterate over each object to get the object fields.
#'
#' Notes and Limitations
#'
#' See also sf_object_fields. This creates a list containing an entry for each
#' queried object. If sf_object_fields doesn't return any info (for
#' example if all the fields are hidden / not return) then the list entry will be
#' "empty".
#'
#'
#' @param objects_data_frame the data frame containing the object list
#' @param name_column the column containing the names of the objects
#'
#' @return A \code{list} of objects and their fields
#'
#' @import checkmate
#'
#'
#' @examples
#' \dontrun{
#' sf_objects_iteration(sobjects)
#' }

sf_objects_iteration <- function(objects_data_frame, name_column = "name") {

  checkmate::assert_data_frame(objects_data_frame)
  checkmate::assert_string(name_column)

  ddat <- as.list(rep("", nrow(objects_data_frame)))

  for (i in 1:nrow(objects_data_frame)) {
    name_to_use = objects_data_frame[[name_column]][[i]]

    # note: suppressing warnings as the code later on copes with these
    ddat[[i]] <- suppressWarnings(sf_object_fields(name_to_use))

  }

  # add the names of the objects as the list entry names

  names(ddat) <- objects_data_frame[[name_column]]

  return(ddat)
}
