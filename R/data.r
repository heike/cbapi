#' Map of congressional districts based on the 113th congress
#' 
#' @name cdmap
#' @title congressional districts
#' @description Map of the Congressional districts as determined by the 113th congress, downloaded from http://www.census.gov/cgi-bin/geo/shapefilesrd13/layers.cgi and modeified for use as map object in ggplot2.
#' \code{cdmap} is a data frame consisting of the variables
#' \itemize{
#' \item \emph{ID:} integer ID for each polygon
#' \item \emph{Long:} geographic longitude
#' \item \emph{Lat:} geographic latitude
#' \item \emph{subregion:} integer id
#' \item \emph{order:} order in which to plot points in polygon
#' \item \emph{group:} group within polygon
#' \item \emph{GEOID:} equivalent to group but first two digits are state fips code.
#' }
#' @docType data
#' @usage data(cdmap)
NULL

#' Census Bureau Datafiles
#' 
#' @name censusData
#' @title Census Bureau Database information
#' @description Information on all (as of Aug 2013) databases available for access through the Census Bureau's API at http://www.census.gov/developers/data/
#' \code{censusData} is a data frame consisting of the variables
#' \itemize{
#' \item \emph{ topic:} short name for the data base, generally one of `American Community Survey', `Census Summary File 1', or `Census Summary File 3'
#' \item \emph{ url:} url for access to the data
#' \item \emph{year:} four digit year involved in data description
#' \item \emph{dbname:} Census Bureau's identifier for the data set
#' \item \emph{xml:} url to access meta-information, usually XML descriptives of all variable 
#' \item \emph{info:} short text with description of the data base.
#' }
#' @docType data
#' @usage data(censusData)
NULL

#' key
#' 
#' @name key
#' @title key for access to the Census Bureau's API
#' @description key for access to the Census Bureau's API, available from http://www.census.gov/developers/tos/key_request.html
#' @docType data
#' @usage data(key)
NULL
