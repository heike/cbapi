\name{getData}
\alias{getData}
\title{Get data from the Census Bureau}
\usage{
  getData(.dbname, .year, vars,
    .for = "congressional+district", .in = "state")
}
\arguments{
  \item{.dbname}{character string of the data, defaults to
  data for the American Community Survey "acs", and summary
  files "sf1", "sf3", for the decennial census of 1990,
  2000, and 2010 are available}

  \item{.year}{year of survey}

  \item{vars}{vector of character strings with identifiers
  of the variables to download}

  \item{.for}{geographical unit, default is congressional
  district}

  \item{.in}{geographical unit, default is state}
}
\value{
  data frame
}
\description{
  Get data from the Census Bureau
}
\examples{
\dontrun{
getDBInfo("acs1_cd113", 2011, "sex")[3:6,]
getDBInfo("acs1_cd113", 2011, "hispanic")[3:6,]
hispanics <- getData("acs1_cd113", 2011, c("DP02_0086E", "DP05_0066E","DP05_0071E"))
require(ggplot2)
hispanics$`Hispanic or Latino (of any race)` <- as.numeric(as.character(hispanics$`Hispanic or Latino (of any race)`))
hispanics$`Not Hispanic or Latino` <- as.numeric(as.character(hispanics$`Not Hispanic or Latino`))
hispanics$`Total population` <- as.numeric(as.character(hispanics$`Total population`))
qplot(`Hispanic or Latino (of any race)`, `Not Hispanic or Latino`, facets=~state, data = hispanics)
data(cdmap)
hispanics$GEOID <- with(hispanics, paste(state,`congressional district`, sep=""))
cdmap.data <- merge(cdmap, hispanics, by="GEOID")
qplot(Long, Lat, fill=`Hispanic or Latino (of any race)`/`Total population`, data=cdmap.data, geom="polygon", group=group, order=order)
zips <- getData("sf1", 2010, "P0010001", .for="zip+code+tabulation+area", .in="state:19")
counties <- getData("sf1", 2010, "P0010001", .for = "county", .in="")
counties[,1] <- as.numeric(counties[,1])
places <- getData("sf1", 2010, "P0010001", .for = "place", .in="")

info <- getDBInfo("acs1_cd113", 2011, "ancestry")
# get rid of all the margins of error:
info <- info[-2*(1:27),]
ancestry <- getData("acs1_cd113", 2011, c("DP02_0086E", as.character(info$ID)))
data(cdmap)
ancestry$GEOID <- with(ancestry, paste(state,`congressional district`, sep=""))
countries <- names(ancestry)[2:28]
res <- unlist(llply(1:nrow(ancestry), function(i) which.max(ancestry[i,2:28])))
ancestry$top <- names(res)

cdmap.data <- merge(cdmap, ancestry, by="GEOID")
write.csv(cdmap.data, file="cd.csv", row.names=FALSE)
cdmap.data <- read.csv("cd.csv")

library(ggplot2)
qplot(Long, Lat, fill=top,
data=cdmap.data, geom="polygon", group=group, order=order) +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_fill_brewer("Most common ancestry", palette="Set3", guide = guide_legend(nrow=3))
}
}

