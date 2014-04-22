\docType{data}
\name{censusData}
\alias{censusData}
\title{Census Bureau Database information}
\description{
  Information on all (as of Apr 2014) databases available
  for access through the Census Bureau's API at
  http://www.census.gov/developers/data/ In order to update
  the available information, run the function
  \code{updateCensusData}. \code{censusData} is a data
  frame consisting (among others) of the variables
  \itemize{ \item \emph{title:} short name for the data
  base, generally one of `American Community Survey',
  `Census Summary File 1', or `Census Summary File 3' \item
  \emph{c_dataset:} Census Bureau's identifier for the data
  set \item \emph{webService:} url for access to the data
  \item \emph{c_vintage:} four digit year involved in data
  description \item \emph{c_variablesLink:} url to access
  meta-information, usually XML descriptives of all
  variable \item \emph{c_geographyLink:} url to access
  meta-information, usually XML descriptives of all
  variable \item \emph{description:} short text with
  description of the data base. \item
  \emph{c_examplesLink:} examples of how to access the
  dataset. \item \emph{...} several other variables that
  might be changing with any update of the API }
}
\details{
  Census Bureau Datafiles
}
\keyword{datasets}

