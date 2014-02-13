censusData <- data.frame(topic=c("2010 Census Summary File 1", "American Community Survey 5 Year Data", "American Community Survey 1 Year Data for 113th Congressional Districts", "2000 Census Summary File 1", "2000 Census Summary File 3", "1990 Census Summary File 1 (SF1)", "1990 Census Summary File 3 (SF3)"), 
                         url=c("http://api.census.gov/data", "http://api.census.gov/data", "http://api.census.gov/data/", "http://www.census.gov/data/", "http://api.census.gov/data", "http://api.census.gov/data", "http://api.census.gov/data"),
                         year=c(2010, 2011, 2011, 2000, 2000, 1990, 1990),
                         dbname=c("sf1", "acs5", "acs1_cd113", "sf1", "sf3", "sf1", "sf3"),
                         xml=c("http://www.census.gov/developers/data/sf1.xml", 
                               "http://www.census.gov/developers/data/acs_5yr_2010_var.xml",
                               "http://www.census.gov/developers/data/acs_1yr_2011_cd113_var.xml",
                               "http://www.census.gov/developers/data/2000_sf1.xml", 
                               "http://www.census.gov/developers/data/2000_sf3.xml", 
                               "http://www.census.gov/developers/data/1990_sf1.xml", 
                               "http://www.census.gov/developers/data/1990_sf3.xml"),
                         #                         geo=c("http://www.census.gov/data/2010/sf1/geo.html", "http://www.census.gov/data/2000/sf1/geo.html"),
                         info=c("Summary File 1 (SF 1) contains the data compiled from the questions asked of all people and about every housing unit. Population items include sex, age, race, Hispanic or Latino origin, household relationship, household type, household size, family type, family size, and group quarters. Housing items include occupancy status, vacancy status, and tenure (whether a housing unit is owner-occupied or renter-occupied).

SF 1 includes population and housing characteristics for the total population, population totals for an extensive list of race (American Indian and Alaska Native tribes, Asian, and Native Hawaiian and Other Pacific Islander) and Hispanic or Latino groups, and population and housing characteristics for a limited list of race and Hispanic or Latino groups. Population and housing items may be cross-tabulated. Selected aggregates and medians also are provided.

The API provides access to a majority of SF1 tables, but not all.

SF1 includes population tables (identified with a ''P'') and housing tables (identified with an ''H'') shown down to various levels of geography.",
                                "The American Community Survey (ACS) is an ongoing survey that provides data every year -- giving communities the current information they need to plan investments and services. The ACS covers a broad range of topics about social, economic, demographic, and housing characteristics of the U.S. population. Much of the ACS data provided on the Census Bureau's Web site are available separately by age group, race, 
                              Hispanic origin, and sex. The 5-year estimates from the ACS are ''period'' estimates that represent data collected over a period of time. The primary advantage of using multiyear estimates is the increased statistical reliability of the data for less populated areas and small population subgroups.", "", "Summary File 3 consists of 813 detailed tables of Census 2000 social, economic and housing characteristics compiled from a sample of approximately 19 million housing units (about 1 in 6 households) that received the Census 2000 long-form questionnaire.", 
                                "Summary File 3 consists of 813 detailed tables of Census 2000 social, economic and housing characteristics compiled from a sample of approximately 19 million housing units (about 1 in 6 households) that received the Census 2000 long-form questionnaire.", "", ""))

save(censusData, file="data/censusData.RData")

key <- "7f784587c3918611ad6ca67188d9b269b3558dd4"
save(key, file="data/key.RData")

