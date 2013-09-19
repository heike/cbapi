\name{read.census}
\alias{read.census}
\title{Read raw data from the Census Bureau using the API}
\usage{
  read.census(url)
}
\arguments{
  \item{url}{character vector of length one containing the
  get query.}
}
\value{
  data frame
}
\description{
  Get raw data from the Census Bureau, JSON format is
  converted to R data frame
}
\examples{
# State populations based on the 2010 Decennial Census
popstate <- read.census(sprintf("http://api.census.gov/data/2010/sf1?key=\%s&get=P0010001,NAME&for=state:*", getkey()))

#  get data on total population for all congressional districts of iowa
u1 <- sprintf("http://api.census.gov/data/2011/acs1_cd113?key=\%s&get=DP02_0086E&for=congressional+district:*&in=state:19", getkey())
df2 <- read.census(u1)
}

