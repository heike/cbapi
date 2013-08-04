#' Get variable names and descriptions for data sets available through Census Bureau API
#' 
#' @param .dbname name of the database, one of "sf1", "acs5", "acs1_cd113", "sf1", "sf3", "sf1", "sf3". see dataset censusData for a list
#' @param .year year of interest - cf censusData
#' @param vars list of variables, if NULL, all are returned in the result
#' @param search should the list of variables be interpreted as search terms?
#' @export
#' @return data frame with following fields:
#' \begin{itemize}
#' \item ID: variable ID 
#' \item Name: name of the variable
#' \item Node: full path to the variable
#' \item depth: positive integer number of the path length
#' \item ... an arbitrary number of variables, one for each level of the path
#' \end{itemize}
#' @examples
#' \dontrun{
#' census2010 <- getDBInfo("sf1", 2010)
#' acs <- getDBInfo("acs1_cd113", 2011, "total population")
#' table(acs$V1.1)
#' head(subset(acs, depth == 2))
#' acs_all <- getDBInfo("acs1_cd113", 2011)
#' table(acs_all$depth)
#' table(acs_all$V1.1)  # concept variables
#' subset(acs_all, V1.1 == "LANGUAGE SPOKEN AT HOME")
#' gp <- subset(acs_all, V1.1 == "GRANDPARENTS")
#' table(subset(gp, depth>2)$V2)
#' }
getDBInfo <- function(.dbname, .year, vars=NULL, search=TRUE) {
  db <- subset(censusData, (year ==.year) & (dbname == .dbname))
  require(XML)
  doc <- htmlParse(db$xml[1], useInternalNodes = TRUE)
  varlist <- getNodeSet(doc, "//variable")
  dframe.api <- ldply(varlist, function(x) { 
    data.frame(xmlAttrs(x)["name"], xmlValue(x))})
  hierarchy <- strsplit(as.character(dframe.api[,2]), " *!! *")
  ks <- laply(hierarchy,length)
  kmax <- max(ks)
  dframe.api2 <- data.frame(dframe.api, Name=ldply(hierarchy, function(x) x[length(x)]),
                            depth=ks,
                            ldply(hierarchy, function(x) {
                              res <- rep(NA, kmax)
                              res[1:length(x)] <- gsub("\"","", x)
                              res
                            }))
  
  names(dframe.api2)[1:3] <- c("ID", "Name", "Node")
  if (is.null(vars)) {
    return(dframe.api2)  
  }
  names <- dframe.api2$Name
  if (search) {
    idx <- sort(unique(unlist(sapply(vars, function(x) grep(tolower(x), tolower(names))))))
    if (length(idx) == 0) return(dframe.api2) 
    return(dframe.api2[idx,])
  } else {
    return(subset(dframe.api2, ID %in% vars)) 
  } 
}

#' Get the developer's key to access the API
#' 
#' In order to access the Census Bureau's data, you have to get a key and agree to the terms of service. You can sign up for a key at http://www.census.gov/data/key_signup.html
#' @export
getkey <- function() {
  if (is.null(key)) cat("You need to sign up for a key for using the Census Bureau's API at http://www.census.gov/data/key_signup.html")
  else key
}


#' Read raw data from the Census Bureau using the API
#' 
#' Get raw data from the Census Bureau, JSON format is converted to R data frame
#' @examples
#' #  get data on total population for all congressional districts of iowa
#' u1 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP02_0086E&for=congressional+district:*&in=state:19"
#' df2 <- read.census(u1)
read.census <- function(url) {
  require(rjson)
  suppressWarnings(llist <- fromJSON(file=url))
  require(plyr)
  dframe <- ldply(llist[-1])
  names(dframe) <- llist[[1]]
  dframe
}


#' Get data from the Census Bureau
#' 
#' @param .dbname character string of the data, defaults to data for the American Community Survey "acs", and summary files "sf1", "sf3", for the decennial census of 1990, 2000, and 2010 are available
#' @param .year year of survey 
#' @param vars vector of character strings with identifiers of the variables to download
#' @param .for geographical unit, default is congressional district
#' @param .in geographical unit, default is state
#' @export
#' @return data frame
#' @examples
#' \dontrun{
#' getDBInfo("acs1_cd113", 2011, "sex")[3:6,]
#' getDBInfo("acs1_cd113", 2011, "hispanic")[3:6,]
#' hispanics <- getData("acs1_cd113", 2011, c("DP02_0086E", "DP05_0066E","DP05_0071E"))
#' require(ggplot2)
#' hispanics$`Hispanic or Latino (of any race)` <- as.numeric(as.character(hispanics$`Hispanic or Latino (of any race)`))
#' hispanics$`Not Hispanic or Latino` <- as.numeric(as.character(hispanics$`Not Hispanic or Latino`))
#' hispanics$`Total population` <- as.numeric(as.character(hispanics$`Total population`))
#' qplot(`Hispanic or Latino (of any race)`, `Not Hispanic or Latino`, facets=~state, data = hispanics)
#' data(cdmap)
#' hispanics$GEOID <- with(hispanics, paste(state,`congressional district`, sep=""))
#' cdmap.data <- merge(cdmap, hispanics, by="GEOID")
#' qplot(Long, Lat, fill=`Hispanic or Latino (of any race)`/`Total population`, data=cdmap.data, geom="polygon", group=group, order=order)
#' zips <- getData("sf1", 2010, "P0010001", .for="zip+code+tabulation+area", .in="state:19")
#' counties <- getData("sf1", 2010, "P0010001", .for = "county", .in="")
#' counties[,1] <- as.numeric(counties[,1])
#' places <- getData("sf1", 2010, "P0010001", .for = "place", .in="")
#' }
getData <- function(.dbname, .year, vars, .for="congressional+district", .in="state") {
  db <- subset(censusData, (year ==.year) & (dbname == .dbname))
  varlist <- paste(as.character(vars), collapse=",")
  if (length(grep(":", .in)) == 0) .in <- sprintf("%s:*", .in)
  if (length(grep(":", .for)) == 0) .for <- sprintf("%s:*", .for)
  url <- sprintf("%s/%d/%s?key=%s&get=%s,NAME&for=%s&in=%s", db$url[1], .year, .dbname, getkey(), varlist, .for, .in)
  cat(gsub("key=[^&]*", "", sprintf("Getting data from: %s", url)))
  dframe <- read.census(url)
  
  k <- length(vars)
  names(dframe)[1:k] <- getDBInfo(.dbname, .year, vars, search=FALSE)$Node
  
  dframe
}

#' List available geographic units for a database
#' 
#' @param year
#' @param database see censusData$dbname for available databases
#' @export
#' @return data frame with examples for the available geographic units in the database
#' @examples
#' listGeo(2010, "sf1")[,-3]
listGeo <- function(year, dbname) {
  url <- sprintf("http://api.census.gov/data/%s/%s/geo.html", year, dbname)
  require(XML)
  readHTMLTable(url)[[1]]
}