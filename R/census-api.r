#' Get variable names and descriptions for data sets available through Census Bureau API
#' 
#' @param .dbname name of the database, one of "sf1", "acs5", "acs1_cd113", "sf1", "sf3", "sf1", "sf3". see dataset censusData$c_dataset for a list
#' @param .year year of interest - cf censusData$c_vintage
#' @param vars list of variables, if NULL, all are returned in the result
#' @param search should the list of variables be interpreted as search terms?
#' @export
#' @return data frame with following fields:
#' \itemize{
#' \item ID: variable ID 
#' \item Name: name of the variable
#' \item Node: full path to the variable
#' \item depth: positive integer number of the path length
#' \item ... an arbitrary number of variables, one for each level of the path
#' }
#' @examples
#' \dontrun{
#' ## need internet connection for the following code
#' 
#' ## get meta information for the full summary file 1 file for Census 2010
#' census2010 <- getDBInfo("sf1", 2010)
#' 
#' ## all available information on 'total population' from the 2011 American Community Survey 
#' acs <- getDBInfo("acs5", 2011, "total population")
#' table(acs$V1)
#' head(subset(acs, depth == 2))
#' 
#' ## get complete meta information for the 2011 American Community Survey
#' acs_all <- getDBInfo("acs5", 2011)
#' table(acs_all$depth)
#' sort(table(acs_all$V1))
#' acs_all[grep("LANGUAGE SPOKEN AT HOME", acs_all$concept),]
#' gp <- acs_all[grep("GrndPrnts", acs_all$concept),]
#' table(subset(gp, depth>2)$V2)
#' }
getDBInfo <- function(.dbname, .year, vars=NULL, search=TRUE) {
  data(censusData, envir = environment())
  # make bindings visible
  year <- NULL
  dbname <- NULL
  ID <- NULL
  db <- subset(censusData, (c_vintage ==.year) & (c_dataset == .dbname))
  if (nrow(db) == 0)  stop("no dataset found with this specification. do you need to update censusData?")
  
  require(rjson)
  suppressWarnings(varlist <- fromJSON(file=as.character(db$c_variablesLink)))
  varframe <- ldply(varlist$variables, as.data.frame)
  varframe$.id <- as.character(varframe$.id)
  
  
  hierarchy <- strsplit(as.character(varframe$label), " *!! *")
  ks <- laply(hierarchy,length)
  kmax <- max(ks)
  varframe.api <- data.frame(varframe, 
                            depth=ks,
                            ldply(hierarchy, function(x) {
                              res <- rep(NA, kmax)
                              res[1:length(x)] <- gsub("\"","", x)
                              res <- gsub(" *:$","", res)
                              res
                            }), stringsAsFactors = FALSE)
  
#  names(varframe.api)[1:3] <- c("ID", "Label", "Concept")
  if (is.null(vars)) {
    return(varframe.api)  
  }
  
  if (search) {
    names <- as.character(varframe.api$concept)
    if (length(names) == 0) {
      warning("No variable matched the search criteria.")
      return(varframe.api) 
    }
    idx <- sort(unique(unlist(sapply(vars, function(x) grep(tolower(x), tolower(names))))))
    if (length(idx) == 0) {
      warning("No variable matched the search criteria.")
      return(varframe.api) 
    } 
    return(varframe.api[idx,])
  } else {
    return(subset(varframe.api, .id %in% vars)) 
  } 
}

#' Get the developer's key to access the API
#' 
#' In order to access the Census Bureau's data, you have to get a key and agree to the terms of service. You can sign up for a key at http://www.census.gov/data/key_signup.html
#' @export
getkey <- function() {
#  try(data(key, envir = environment()))
  dir <- system.file(package = "cbapi")
  suppressWarnings(try(load(file=sprintf("%s/data/key.RData", dir)), silent = TRUE))
  if (is.null(key)) {
    cat("You need to sign up for a key for using the Census Bureau's API at http://api.census.gov/data/key_signup.html\nThen see ?setkey")
    browseURL("http://api.census.gov/data/key_signup.html")
  }  
  else key
}

#' Set the developer's key to access the API
#' 
#' Save the developer's key to a file for enabling use of the Census Bureau's API. You can request a key from http://api.census.gov/data/key_signup.html.
#' You only have to do this once. 
#' @param key character string of the key you got from the Census Bureau.
#' @export
setkey <- function(key) {
  dir <- system.file(package = "cbapi")
  key <- key
  save(key, file=sprintf("%s/data/key.RData", dir))
  cat("key is saved, now you will be able to access data through the API. ")
#  try(data(key, envir = environment()))
#  require(devtools)
#  install(dir)
}


#' Read raw data from the Census Bureau using the API
#' 
#' Get raw data from the Census Bureau, JSON format is converted to R data frame
#' @param url character vector of length one containing the get query. 
#' @return data frame
#' @export
#' @examples
#' # State populations based on the 2010 Decennial Census
#' popstate <- read.census(sprintf("http://api.census.gov/data/2010/sf1?key=%s&get=P0010001,NAME&for=state:*", getkey()))
#' 
#' #  get data on total population for all congressional districts of iowa
#' u1 <- sprintf("http://api.census.gov/data/2011/acs1_cd113?key=%s&get=DP02_0086E,DP02_0086M&for=congressional+district:*&in=state:19", getkey())
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
#' getDBInfo("sf1", 2010, "sex")[3:6,]
#' getDBInfo("acs5", 2011, "hispanic")[3:6,]
#' 
#' hispanics <- getData("acs1/cd113", 2011, c("DP02_0086E", "DP05_0066E","DP05_0071E"))
#' require(ggplot2)
#' hispanics$`Hispanic or Latino (of any race)` <- as.numeric(as.character(hispanics$`Hispanic or Latino (of any race)`))
#' hispanics$`Not Hispanic or Latino` <- as.numeric(as.character(hispanics$`Not Hispanic or Latino`))
#' hispanics$`Total population` <- as.numeric(as.character(hispanics$`Total population`))
#' qplot(`Hispanic or Latino (of any race)`, `Not Hispanic or Latino`, facets=~state, data = hispanics)
#' data(cdmap)
#' hispanics$GEOID <- with(hispanics, paste(state,`congressional district`, sep=""))
#' cdmap.data <- merge(cdmap, hispanics, by="GEOID")
#' qplot(Long, Lat, fill=`Hispanic or Latino (of any race)`/`Total population`, data=cdmap.data, geom="polygon", group=group, order=order)
#' 
#' zips <- getData("sf1", 2010, "P0010001", .for="zip+code+tabulation+area", .in="state:19")
#' 
#' counties <- getData("sf1", 2010, "P0010001", .for = "county", .in="")
#' counties[,1] <- as.numeric(counties[,1])
#' places <- getData("sf1", 2010, "P0010001", .for = "place", .in="")
#' 
#' info <- getDBInfo("acs1_cd113", 2011, "ancestry")
#' # get rid of all the margins of error:
#' info <- info[-2*(1:27),]
#' ancestry <- getData("acs1/cd113", 2011, c("DP02_0086E", as.character(info$ID)))
#' data(cdmap)
#' ancestry$GEOID <- with(ancestry, paste(state,`congressional district`, sep=""))
#' countries <- names(ancestry)[2:28]
#' res <- unlist(llply(1:nrow(ancestry), function(i) which.max(ancestry[i,2:28])))
#' ancestry$top <- names(res)
#' 
#' cdmap.data <- merge(cdmap, ancestry, by="GEOID")
#' write.csv(cdmap.data, file="cd.csv", row.names=FALSE)
#' cdmap.data <- read.csv("cd.csv")
#' 
#' library(ggplot2)
#' qplot(Long, Lat, fill=top, 
#' data=cdmap.data, geom="polygon", group=group, order=order) + 
#'   theme_bw() +
#'   theme(legend.position="bottom") + 
#'   scale_fill_brewer("Most common ancestry", palette="Set3", guide = guide_legend(nrow=3))
#' }
getData <- function(.dbname, .year, vars, .for="congressional+district", .in="state") {
  data(censusData, envir = environment())
  year <- NULL
  dbname <- NULL
  
  db <- subset(censusData, (c_vintage ==.year) & (c_dataset == .dbname))
  varlist <- paste(as.character(vars), collapse=",")
  if (length(grep(":", .in)) == 0) .in <- sprintf("%s:*", .in)
  if (length(grep(":", .for)) == 0) .for <- sprintf("%s:*", .for)
  url <- sprintf("%s?key=%s&get=%s,NAME&for=%s&in=%s", db$webService, getkey(), varlist, .for, .in)
  cat(gsub("key=[^&]*", "", sprintf("Getting data from: %s", url)))
  dframe <- read.census(url)
  
  k <- length(vars)
  names(dframe)[1:k] <- as.character(getDBInfo(.dbname, .year, vars, search=FALSE)$label)
  
  dframe
}

#' List examples of available geographic units for a database
#' 
#' 
#' @param year four digit year
#' @param dbname see censusData$dbname for available databases
#' @export
#' @return data frame with examples for the available geographic units in the database
#' @examples
#' listGeoExamples(2010, "sf1")[,-3]
#' iowa <- read.census(gsub("XXX", getkey(), "http://api.census.gov/data/2010/sf1?key=XXX&get=P0010001,NAME&for=zip+code+tabulation+area:*&in=state:19"))
listGeoExamples <- function(year, dbname) {
  url <- sprintf("http://api.census.gov/data/%s/%s/geo.html", year, dbname)
  require(XML)
  readHTMLTable(url)[[1]]
}

#' List of available geographic units for a database
#' 
#' @param year four digit year
#' @param dbname see censusData$dbname for available databases
#' @return data frame with available geographic units in the database and their requirements 
#' @examples
#' listGeo(2010, "sf1")
#' listGeo(2011, "acs1")
#' listGeo(2012, "acs1")
#' iowa <- read.census(gsub("XXX", getkey(), "http://api.census.gov/data/2010/sf1?key=XXX&get=P0010001,NAME&for=zip+code+tabulation+area:*&in=state:19"))
#' @export
listGeo <- function(year, dbname) {
  require(rjson)
  url <- subset(censusData, c_vintage == year & c_dataset == dbname)$c_geographyLink
  if (length(url) == 0) error("there is no data set available with this combination of data set identifier and year. is censusData up to date?")
  if (length(url) > 1) warning("there are multiple choices available for this combination of data and year. using the first one.")
  url <- url[1]
  suppressWarnings(geolist <- fromJSON(file=as.character(url)))
  geoframe <- ldply(geolist$fips, function(x) {
    x$requires <- paste(x$requires, collapse=", ")
    as.data.frame(x)
  })
  geoframe
}

#' Update the list of datasets available through the Census Bureau's API
#' 
#' Updates the version of the censusData data set from the information available at \url{http://api.census.gov/data/}. This might get the help file for censusData out of sync. It might also break access to the API. Do at your own risk, and be prepared to re-install the cbapi package. 
#' @return updated version of the censusData 
#' @export
updateCensusData <- function() {
  url <- "http://api.census.gov/data/"
  require(rjson)
  suppressWarnings(datalist <- fromJSON(file=url))
  require(plyr)
  censusData <- ldply(datalist, function(x) {
    x$c_dataset <- paste(x$c_dataset, collapse="/")
    as.data.frame(x)
  })
  dir <- system.file(package = "cbapi")
  save(censusData, file=sprintf("%s/data/censusData.RData", dir))
  cat("censusData set is updated now.")
}
