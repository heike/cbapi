\name{listGeo}
\alias{listGeo}
\title{List of available geographic units for a database}
\usage{
  listGeo(year, dbname)
}
\arguments{
  \item{year}{four digit year}

  \item{dbname}{see censusData$dbname for available
  databases}
}
\value{
  data frame with available geographic units in the
  database and their requirements
}
\description{
  List of available geographic units for a database
}
\examples{
listGeo(2010, "sf1")
listGeo(2010, "acs")
iowa <- read.census(gsub("XXX", getkey(), "http://api.census.gov/data/2010/sf1?key=XXX&get=P0010001,NAME&for=zip+code+tabulation+area:*&in=state:19"))
}

