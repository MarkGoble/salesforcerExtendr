#' Gets all the fields for a salesforce object
#'
#' This function will pull back the information for a named Object.
#'
#' Notes and Limitations:
#'
#' Note: this function can actually pull back info for objects such as 'Activity'
#' which aren't Sobjects.
#'
#' This version doesn't retrieve some field details for standard fields - for
#' example the data type. This is down to the way that the Force.com query works.
#' The SF query doesn't return the 'created' and 'modified' fields.
#'
#' Also note that valueset (which contains picklist values) and
#' summaryfilteritems are also removed as these are nested lists.
#'
#' for v2 changed from 'CustomObject' to 'CustomDatatyoeComponent' as we don't
#' need all the data which is returned under 'customObject'
#'
#' @param sf_object_name the name of the salesforce object to query
#'
#' @return A \code{data frame} of the fields in the object or 'empty' if no readable fields
#'
#' @importFrom magrittr %>%
#' @importFrom salesforcer sf_read_metadata
#' @importFrom tidyr separate
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom qdapTools list2df
#' @import checkmate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sf_object_fields("contact")
#' }


sf_object_fields <- function( sf_object_name) {

  checkmate::assert_string(sf_object_name)

  object_list <- salesforcer::sf_read_metadata(metadata_type = 'CustomObject',
                                 object_names = c(sf_object_name))

  # grab the fields
  fields <- which(names(object_list[[1]]) == 'fields')

  if (length(fields) == 0) {
    return("empty")
  } else {
    fields_list <- object_list[[1]][fields]
    names(fields_list) <- 1:length(fields_list)
    fields_list <- unlist(fields_list, recursive = TRUE)
    fields_df <- qdapTools::list2df(fields_list)
    fields_df  <- fields_df %>% tidyr::separate(.data$X2, c("X", "X2"))
    fields_df  <- fields_df  %>% dplyr::filter(!.data$X2 %in%  c("valueSet",
                                  "summaryFilterItems"))

    # for the moment only returning these fields
    fields_df <- fields_df  %>%
      dplyr::filter(.data$X2 %in%  c("fullName",
                                 "label",
                                 "type",
                                 "length",
                                 "precision",
                                 "description"))

    t <- tidyr::spread(fields_df , .data$X2, .data$X1)
    t <- dplyr::mutate(t, object = sf_object_name)
    t <- dplyr::select(t,-.data$X)
    return(t)
  }

}
