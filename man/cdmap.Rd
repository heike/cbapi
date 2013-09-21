\docType{data}
\name{cdmap}
\alias{cdmap}
\title{congressional districts}
\description{
  Map of the Congressional districts as determined by the
  113th congress, downloaded from
  http://www.census.gov/cgi-bin/geo/shapefilesrd13/layers.cgi
  and modeified for use as map object in ggplot2.
  \code{cdmap} is a data frame consisting of the variables
  \itemize{ \item \emph{ID:} integer ID for each polygon
  \item \emph{Long:} geographic longitude \item \emph{Lat:}
  geographic latitude \item \emph{subregion:} integer id
  \item \emph{order:} order in which to plot points in
  polygon \item \emph{group:} group within polygon \item
  \emph{GEOID:} equivalent to group but first two digits
  are state fips code. }
}
\details{
  Map of congressional districts based on the 113th
  congress
}
\examples{
\dontrun{
## read from ACS 2012 profile:
read.census(sprintf("http://api.census.gov/data/2012/acs1/profile?get=DP02_0001PE&for=state:*&key=\%s", getkey()))
}
}
\keyword{datasets}

