\name{getDBInfo}
\alias{getDBInfo}
\title{Get variable names and descriptions for data sets available through Census Bureau API}
\usage{
  getDBInfo(.dbname, .year, vars = NULL, search = TRUE)
}
\arguments{
  \item{.dbname}{name of the database, one of "sf1",
  "acs5", "acs1_cd113", "sf1", "sf3", "sf1", "sf3". see
  dataset censusData for a list}

  \item{.year}{year of interest - cf censusData}

  \item{vars}{list of variables, if NULL, all are returned
  in the result}

  \item{search}{should the list of variables be interpreted
  as search terms?}
}
\value{
  data frame with following fields: \itemize{ \item ID:
  variable ID \item Name: name of the variable \item Node:
  full path to the variable \item depth: positive integer
  number of the path length \item ... an arbitrary number
  of variables, one for each level of the path }
}
\description{
  Get variable names and descriptions for data sets
  available through Census Bureau API
}
\examples{
\dontrun{
## need internet connection for the following code

## get meta information for the full summary file 1 file for Census 2010
census2010 <- getDBInfo("sf1", 2010)

## all available information on 'total population' from the 2011 American Community Survey
acs <- getDBInfo("acs1_cd113", 2011, "total population")
table(acs$V1.1)
head(subset(acs, depth == 2))

## get complete meta information for the 2011 American Community Survey
acs_all <- getDBInfo("acs1_cd113", 2011)
table(acs_all$depth)
table(acs_all$V1.1)  # concept variables
subset(acs_all, V1.1 == "LANGUAGE SPOKEN AT HOME")
gp <- subset(acs_all, V1.1 == "GRANDPARENTS")
table(subset(gp, depth>2)$V2)
}
}

