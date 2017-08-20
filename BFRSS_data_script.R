setwd('/Users/delfuego/localhost/r scripts/BFRSS/')

library(dplyr)

##################################
#####    READ DATA    ############
##################################

#####    DATA SOURCE    ############
# https://chronicdata.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu
# source: https://chronicdata.cdc.gov/health-area/behavioral-risk-factors

# brfss_rawData = read.csv('Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present-OpenedInExcelThenSavedAsCSV.csv', sep=",")
bfrss_rawData = read.csv('Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv', sep=",")
# Note: if data appears corrupted, and you're on a mac, you may need to download & install Quartz, and then restart your mac
# https://www.xquartz.org/

head(bfrss_rawData, 1)
glimpse(bfrss_rawData)
#View(brfss_rawData)
# Raw data has 920959 rows and 27 columns
nrow(bfrss_rawData)

# Creating a dataset: selected out Alaska rows
bfrss_rawData_AlaskaData <- filter(bfrss_rawData, bfrss_rawData$Locationabbr == "AK")
# Now there are 17356 records (1.88% of total)
nrow(bfrss_rawData_AlaskaData)
head(bfrss_rawData_AlaskaData, 1)

summary(bfrss_rawData_AlaskaData)

# What are the questions about, and how many are there?
levels(factor(bfrss_rawData_AlaskaData$QuestionID))


#### MAPPING #####


install.packages(c("ggplot2", "devtools", "dplyr", "stringr")) # data tools
install.packages(c("maps", "mapdata")) # maping packages
devtools::install_github("dkahle/ggmap") # most up to date version, ggmap package

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
head(usa)
ggplot() + geom_polygon(data=usa, aes(x=long, y=lat, group=group)) + coord_fixed(1.3)
  

  
  
########################################
########################################
##### TO INCLUDE IN NOTES:


### THINGS THAT DID NOT WORK OR THAT AREN'T PERTINENT BUT ARE RELATED/INTERESTING:


### Mapping ###
# source info: http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html
# --> Only has lower 48 states
install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")

require(maps)
require(ggmap)
library(maps)
library(ggmap)


par(mfrow = c(2, 1))
map("state", "ALASKA")
dev.off()
###########################################################################


# This could be interesting to mess with:
# http://www.computerworld.com/article/3175623/data-analytics/mapping-in-r-just-got-a-whole-lot-easier.html
install.packages("tmap")
install.packages("tmaptools")
library("tmap")
library("tmaptools")
install.packages("remotes")
remotes::install_github('walkerke/tigris')
library("tigris")

us_geo <- states(class="sf")
###########################################################################

###########################################################################

###########################################################################