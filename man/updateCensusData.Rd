\name{updateCensusData}
\alias{updateCensusData}
\title{Update the list of datasets available through the Census Bureau's API}
\usage{
  updateCensusData()
}
\value{
  updated version of the censusData
}
\description{
  Updates the version of the censusData data set from the
  information available at
  \url{http://api.census.gov/data/}. This might get the
  help file for censusData out of sync. It might also break
  access to the API. Do at your own risk, and be prepared
  to re-install the cbapi package.
}

