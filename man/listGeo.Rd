\name{listGeo}
\alias{listGeo}
\title{List available geographic units for a database}
\usage{
  listGeo(year, dbname)
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
  List available geographic units for a database
}
\examples{
listGeo(2010, "sf1")[,-3]
}

