#' securely connect to a data source using a connection file
#'
#' Source the connection details from a connection file. Updated to allow passing
#' in of the connection file details including the directory.
#' Note this version uses the rstudioapi package so intended to be used within
#' rstudio
#'
#' Source the connection details from a connection file
#'
#' @param connection_file an r script file containing the connection
#' @param directory the directory containing the connection file
#'
#' @importFrom rstudioapi selectFile
#' @export
#'
#' @examples
#' \dontrun{
#' Insert example here
#' }

secure_connection <- function(connection_file = NULL, directory = NULL){

  connection_file_path <- as.character()

  current_working_directory <- getwd()

  # if we haven't set the directory set to the current working directory
  if(is.null(directory)){
    directory <- getwd()
  }

  # check that the directory path exists - if not exit.
  if(!checkmate::check_directory_exists(directory)){
    setwd(current_working_directory)
    stop("file directory doesn't exist")
  }

  #if a connection_file has been specified then create the full file path
  if(!is.null(connection_file)){
    connection_file_path <- paste0(directory,"/",connection_file )
  } else {
      # if we didn't specify a connection file then choose it and set the
      # full file path. Using rstudioapiselectFile as allows setting of
      # directory without having to use setwd which gives inconsistent
      # results.
      connection_file_path <- rstudioapi::selectFile(path = directory,
                                                     filter = "R Files (*.R)")

      #if we cancel in the chooser dialog then the selectFile returns null
      # so check for this and stop...
      if(is.null(connection_file_path)){
        stop("no file chosen")
      }
  }

  # debug print statement
  #print(connection_file_path)

  # check the full file path is valid and if not we stop.
  checkmate::assert_file_exists(connection_file_path)

  #finally run the script in the local environment
  local(source(connection_file_path))

}

