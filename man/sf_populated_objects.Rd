% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_populated_objects.R
\name{sf_populated_objects}
\alias{sf_populated_objects}
\title{creates a data frame of populated salesforce objects and their fields}
\usage{
sf_populated_objects(objects_fields_list, objects_data_frame)
}
\arguments{
\item{objects_fields_list}{a \code{list} of the objects fields}

\item{objects_data_frame}{a \code{data frame} of object information}
}
\value{
a \code{data frame} of all the populated objects and their fields
}
\description{
This function will reformat the data from an object list into a data frame
}
\details{
Notes and Limitations:

Takes the output and filter to just get those list items which contain object details
}
\examples{
\dontrun{
to add
}
}
