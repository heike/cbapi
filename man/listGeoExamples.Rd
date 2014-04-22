\name{listGeoExamples}
\alias{listGeoExamples}
\title{List examples of available geographic units for a database}
\usage{
  listGeoExamples(year, dbname)
}
\arguments{
  \item{year}{four digit year}

  \item{dbname}{see censusData$dbname for available
  databases}
}
\value{
  data frame with examples for the available geographic
  units in the database
}
\description{
  List examples of available geographic units for a
  database
}
\examples{
listGeoExamples(2010, "sf1")[,-3]
iowa <- read.census(gsub("XXX", getkey(), "http://api.census.gov/data/2010/sf1?key=XXX&get=P0010001,NAME&for=zip+code+tabulation+area:*&in=state:19"))
}

