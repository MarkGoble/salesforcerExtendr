#' Returns details of objects / fields in an org
#'
#' This function will return a list of two data frames - one with fields from objects
#' which can be seen under the users permissions (populated objects) and the other
#' for objects which don't have any visible fields (unpopulated objects)
#'
#' If use connection file is true then uses the secure_connection method otherwise
#' will open salesforce in a browser to authenticate.
#'
#'@param use_connection_file should a connection file be used? (set to false if a session has already been established)
#'@param connection_file only used if use-connection_file is TRUE. Passed to Secure_connection
#'@param directory only used if use_connection_file is TRUE. Passed to Secure_connection
#'
#' @return A \code{list} of two data frames (see intoduction)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sf_get_object_details()
#' }

sf_get_object_details <- function(use_connection_file = FALSE, connection_file = NULL, directory = NULL){

  if(use_connection_file == TRUE){
    secure_connection(connection_file = connection_file, directory = directory) }

  message("getting object details from salesforce")
  # get the list of all Sobjects
  objects_data_frame <- sf_objects_info()
  message("object info retrieved, getting field info for each object- this can take a little while")
  # we then iterate through this list to get a list containing the details of all
  # the objects

  objects_fields_list <- sf_objects_iteration(objects_data_frame)
  message("object fields retrieved")
  # create a dataframe of populated objects (i.e. ones where we can see fields)

  populated_objects <- invisible(sf_populated_objects(objects_fields_list, objects_data_frame))

  # create a dataframe of objects where we can't see the fields...

  unpopulated_objects <- invisible(sf_unpopulated_objects(objects_fields_list, objects_data_frame))

  return(list("populated_objects" = populated_objects,"unpopulated_objects" = unpopulated_objects))

}
