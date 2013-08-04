\name{read.census}
\alias{read.census}
\title{Read raw data from the Census Bureau using the API}
\usage{
  read.census(url)
}
\description{
  Get raw data from the Census Bureau, JSON format is
  converted to R data frame
}
\examples{
#  get data on total population for all congressional districts of iowa
u1 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP02_0086E&for=congressional+district:*&in=state:19"
df2 <- read.census(u1)
}

