




ancestry <- getData("acs1_cd113", 2011, info$ID)




# total population, all congressional districts of iowa
u1 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP02_0086E&for=congressional+district:*&in=state:19"
head(getDBInfo("acs1_cd113", 2011, "Total population"))
pops <- getData("acs1_cd113", 2011, c("DP02_0086E", "DP02_0086M"))


head(getDBInfo("acs1_cd113", 2011, "Total population"))


# total female, male population, all congressional districts of iowa
u2 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP05_0003E,DP05_0002E&for=congressional+district:*&in=state:19"
getDBInfo("acs1_cd113", 2011, "sex")[3:6,]
gender <- getData("acs1_cd113", 2011, c("DP05_0002E", "DP05_0003E"))


# total population by age, all congressional districts of iowa
u3 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP05_0004E,DP05_0005E,DP05_0006E,DP05_0007E,DP05_0008E,DP05_0009E,DP05_0010E,DP05_0011E,DP05_0012E,DP05_0013E,DP05_0014E,DP05_0015E,DP05_0016E&for=congressional+district:*&in=state:*"

# race 
u4 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=DP05_0029E,DP05_0032E,DP05_0033E,DP05_0034E,DP05_0039E,DP05_0047E,DP05_0052E,DP05_0053E&for=congressional+district:*&in=state:*  "
getDBInfo("acs1_cd113", 2011, "race")

# hispanics
vars <- "DP05_0066E,DP05_0071E" # total pop, hispanic, non-hispanic
u5 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=__VARS__&for=congressional+district:*&in=state:* "
u5 <- gsub("__VARS__", vars, u5)


# income
vars <- "DP03_0062E"  # median income in 2011
u6 <- "http://api.census.gov/data/2011/acs1_cd113?key=7f784587c3918611ad6ca67188d9b269b3558dd4&get=__VARS__&for=congressional+district:*&in=state:* "
u6 <- gsub("__VARS__", vars, u6)

getDBInfo("acs1_cd113", 2011, "Median family income")
income <- getData("acs1_cd113", 2011, c("DP03_0086E", "DP03_0086M"))

library(plyr)



(df2 <- read.census(u1))
(df2 <- read.census(u2))
(df2 <- read.census(u3))
(df2 <- read.census(u4))
(df2 <- read.census(u5))
(df2 <- read.census(u6))

lookup <- ldply(names(df2), function(x) dframe.api2[grep(x, dframe.api2[,1]),c(1,3)])
lookup
names(df2)[1:(length(names(df2))-2)] <- as.character(lookup[,2])

library(ggplot2)
library(reshape)
dfm <- melt(df2, id.vars=length(names(df2))-0:1)
qplot(variable, weight=as.numeric(as.character(value)), data=dfm, geom="bar", fill=`congressional district`, position="dodge", facets=~state) + coord_flip() + scale_fill_brewer(palette="Paired", guide="none")

qplot(variable, weight=as.numeric(as.character(value)), 
      data=subset(dfm, `congressional district`=="01"), geom="bar", fill=`congressional district`, 
      facets=~state) + 
  geom_point(aes(x=variable, y=as.numeric(as.character(value))), data=dfm)
coord_flip() + 
  scale_fill_brewer(palette="Set2", guide="none")



library(cbapi)
zips <- getData("sf1", 2010, "P0010001", .for="zip+code+tabulation+area", .in="state:19")
zips[,1] <- as.numeric(zips[,1])
summary(zips)

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

qplot(Long, Lat, fill=French.Canadian/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("french-canadian.pdf", width=15, height=10)

qplot(Long, Lat, fill=German/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("german.pdf", width=15, height=10)

qplot(Long, Lat, fill=German/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("german.pdf", width=15, height=10)

qplot(Long, Lat, fill=Norwegian/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("norwegian.pdf", width=15, height=10)

qplot(Long, Lat, fill=Scottish/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("scottish.pdf", width=15, height=10)

qplot(Long, Lat, fill=Swedish/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("swedish.pdf", width=15, height=10)

qplot(Long, Lat, fill=American/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("american.pdf", width=15, height=10)

qplot(Long, Lat, fill=Subsaharan.African/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("subsaharan.pdf", width=15, height=10)

qplot(Long, Lat, fill=English/Total.population*100, 
      data=cdmap.data, geom="polygon", group=group, order=order) + theme(legend.position="bottom")
ggsave("english.pdf", width=15, height=10)