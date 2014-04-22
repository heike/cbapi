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
#' @examples
#' \dontrun{
#' ## read from ACS 2012 profile:
#' read.census(sprintf("http://api.census.gov/data/2012/acs1/profile?get=DP02_0001PE&for=state:*&key=%s", getkey()))
#' }
#' @docType data
#' @usage data(cdmap)
NULL

#' Census Bureau Datafiles
#' 
#' @name censusData
#' @title Census Bureau Database information
#' @description Information on all (as of Apr 2014) databases available for access through the Census Bureau's API at http://www.census.gov/developers/data/
#' In order to update the available information, run the function \code{updateCensusData}.
#' \code{censusData} is a data frame consisting (among others) of the variables
#' \itemize{
#' \item \emph{title:} short name for the data base, generally one of `American Community Survey', `Census Summary File 1', or `Census Summary File 3'
#' \item \emph{c_dataset:} Census Bureau's identifier for the data set
#' \item \emph{webService:} url for access to the data
#' \item \emph{c_vintage:} four digit year involved in data description
#' \item \emph{c_variablesLink:} url to access meta-information, usually XML descriptives of all variable 
#' \item \emph{c_geographyLink:} url to access meta-information, usually XML descriptives of all variable 
#' \item \emph{description:} short text with description of the data base.
#' \item \emph{c_examplesLink:} examples of how to access the dataset.
#' \item \emph{...} several other variables that might be changing with any update of the API
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

