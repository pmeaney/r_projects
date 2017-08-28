
####################
####################
## Install packages & load libraries:
# install.packages("ggplot2")
# install.packages("devtools")
# install.packages("data.table")
# install.packages("readr")
# install.packages("sp")
# install.packages("rgdal")
# install.packages("choroplethr")
# install.packages("choroplethrMaps")
# install.packages("dplyr")
# install.packages("formattable")
# install.packages("fmsb")
###########################################################################
##  Downloaded the install package manually from here                    ##
##  https://github.com/arilamstein/choroplethrZip/archive/v1.5.0.tar.gz  ##
##  Manually Installed Package since github was blocked by firewall      ##
###########################################################################

## This lets us check Variance Inflation Factors:


library("ggplot2")
library("devtools")
library("data.table")
library("readr")
library("sp")
library("rgdal")
library("choroplethr")
library("choroplethrMaps")
library("choroplethrZip")
library("dplyr")
library("formattable")
library("fmsb")

# Can be used to change what repositories to download from
# setRepositories()


## going to use the devtools package from CRAN to install choroplethrZip from github
## Might need to do this, not sure:
# detach("package:plyr", unload=TRUE)
####################
####################
# Outline:
#
# 1. Load economic data.                                      https://data.austintexas.gov/resource/hcnj-rei3.csv
#   a. download data automatically
#   b. load zip codes separately (because I was having difficulty accessing them from the dataset)
#   c. select out only the columns we need
#
# 2. Load crime data.
#   a. You'll need to *download this data manually*
#      (40k rows, 12.3 MB file)  *-->  https://data.austintexas.gov/dataset/Annual-Crime-2014/7g8v-xxja/about
#   b. select out only the columns we need
#   c. filter crime data by zipcodes available in the neighborhood economic dataset
#   d. simplify the names for the types of crimes
#   e. make a data table of frequencies for each crime type
#   f. shift data around to make a final table for crime: crime_list
#
# 3. Add in population data, and create a main dataset of crime & demographic data
#   a. use R's local data library to access 2012 population statistics: data(df_pop_zip)
#   b. select out the columns we need
#   c. create a main dataset of crime & demographic data
#
# 4. Add in geographical data
#   a. load the data automatically                         https://www.census.gov/geo/maps-data/data/gazetteer2015.html
#   b. select out the zipcodes we're focused on, along with their area in square-meters.
#
# 5.  Create our main dataset
#   Combine Economic, Population, Geographic, and Crime data.
#
# 6.  Create crime-per-capita dataset

################################################################################
##          ####################################################################
##    1.    Import and format 2014 neighborhood economic data
##          ####################################################################
################################################################################

#####  **  **  **  **  **  **  DOWNLOAD THE DATASET CSV FILE   **  **  **  **  **  **  ** 
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  
##  Automatically download and load from url via this link:
austin2014_data_raw <- read_csv('https://data.austintexas.gov/resource/hcnj-rei3.csv', na = '')
##  OR download the dataset by visitng the link above, and then loading it from the csv file in a directory:
#setwd()
# austin2014_data_raw <- read_csv('hcnj-rei3.csv', na = '')
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  

glimpse(austin2014_data_raw)
nrow(austin2014_data_raw)

austin2014_data_raw$`Zip Code` # Taking a look at the Zipcode column, we see one row has no zipcode (one of them is NA)
austin2014_data <- austin2014_data_raw[-1,] # Remove the first row, which has no zip code (this row is the average data for all of Austin, I emailed them for confirmation)
nrow(austin2014_data)
# View(austin2014_data)

## Select a few columns for our neighborhood economic data subset
columnSelection <- c("Zip Code", "Population below poverty level", "Median household income", "Unemployment", "Median rent")
austin2014_EconData_selection <- subset(austin2014_data, select=columnSelection)
names(austin2014_EconData_selection) <- c("zipcode", "Population below poverty level", "Median household income", "Unemployment", "Median rent")

## Reset row number index since we removed the first row (r kept the original index, now we're gonna reset it)
rownames(austin2014_EconData_selection) <- 1:nrow(austin2014_EconData_selection)
# View(austin2014_EconData_selection)

# Remove dollar signs
AustinData_noDollarSign <- austin2014_EconData_selection
AustinData_noDollarSign$`Median household income` = as.numeric(gsub("\\$", "", AustinData_noDollarSign$`Median household income`))
AustinData_noDollarSign$`Median rent` = as.numeric(gsub("\\$", "", AustinData_noDollarSign$`Median rent`))
# View(AustinData_noDollarSign)
austin2014_EconData_selection <- AustinData_noDollarSign

# Remove percentage signs
AustinData_noPercentSign <- austin2014_EconData_selection
AustinData_noPercentSign$`Population below poverty level` = as.numeric(gsub("\\%", "", AustinData_noPercentSign$`Population below poverty level`))
AustinData_noPercentSign$Unemployment = as.numeric(gsub("\\%", "", AustinData_noPercentSign$Unemployment))
# View(AustinData_noPercentSign)
austin2014_EconData_selection <- AustinData_noPercentSign

# Simplify names
names(austin2014_EconData_selection) <- c("zipcode", "PopulationBelowPovertyLevel", "MedianHouseholdIncome", "Unemployment", "MedianRent")
################################################################################
##          ####################################################################
##    2.    Import, count, and format 2014 crime data
##          ####################################################################
################################################################################

#####  **  **  **  **  **  **  DOWNLOAD THE DATASET CSV FILE   **  **  **  **  **  **  ** 
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  
## NOTE: You need to MANUALLY download this entire dataset (40.6k rows) by visiting this link:
## https://data.austintexas.gov/dataset/Annual-Crime-2014/7g8v-xxja/about
## (This command allows direct download, but is limited to 1000 rows, so we didn't use it: austinCrime2014_data_raw <- read_csv('https://data.austintexas.gov/resource/7g8v-xxja.csv', na = '') )

## This uses the csv file you downloaded from the above link, so make sure you have it in the right directory:
austinCrime2014_data_raw <- read.csv('Annual_Crime_2014.csv', na = '')
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  

glimpse(austinCrime2014_data_raw)
nrow(austinCrime2014_data_raw)

## How many unique zipcodes?
length(unique(austinCrime2014_data_raw$GO.Location.Zip))

## Select and rename required columns
columnSelection_Crime <- c("GO.Location.Zip", "GO.Highest.Offense.Desc", "Highest.NIBRS.UCR.Offense.Description")
austinCrime_dataset <- select(austinCrime2014_data_raw, one_of(columnSelection_Crime))
names(austinCrime_dataset) <- c("zipcode", "highestOffenseDesc", "NIBRS_OffenseDesc")
glimpse(austinCrime_dataset)
nrow(austinCrime_dataset)

## Filter crime data by zipcodes available in the neighborhood economic data subset
length(unique(austinCrime2014_data_raw$GO.Location.Zip)) # 49
length(unique(austin2014_EconData_selection$zipcode)) # 36

austinCrime2014_data_selected_zips <- filter(austinCrime_dataset, zipcode %in% austin2014_EconData_selection$zipcode)
glimpse(austinCrime2014_data_selected_zips)
length(unique(austinCrime2014_data_selected_zips$zipcode)) # 36
nrow(austinCrime2014_data_selected_zips)
typeof(austinCrime2014_data_selected_zips)

## Convert our crime data subset from string/char data into factorized data so we can see levels

## let's make the character data columns c("highestOffenseDesc", "NIBRS_OffenseDesc") into factors so we can check its levels
glimpse(austinCrime2014_data_selected_zips) # characters
cols <- c("highestOffenseDesc", "NIBRS_OffenseDesc") # these are columns with character datatype we will next convert to factor datatype
austinCrime2014_data_selected_zips[cols] <- lapply(austinCrime2014_data_selected_zips[cols], factor) # converting
glimpse(austinCrime2014_data_selected_zips) # now they're factors
# View(austinCrime2014_data_selected_zips)


levels(austinCrime2014_data_selected_zips$highestOffenseDesc) #--> looks good
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc) # output is weird:  "Burglary / \nBreaking & Entering" "Robbery"

# So, one crime description column has 43 levels, and the other has 14 levels. 
# We're going to go with the column NIBRS_OffenseDesc because it has the more
# simplified description of the crimes.  And we're going to simplify it even more.
# We'll use gsub() to reduce down the names of crimes to their most basic description:
# "Assault"  "Burglary" "Homicide" "Rape"     "Robbery"  "Theft"

# Here's an example of as.data.frame(table()):
# This shows that as.data.frame(table()) will give us a list of frequencies per crime in each zipcode:
someCheck <- austinCrime2014_data_selected_zips[-2] # removing the column "highestOffenseDesc" because were aren't using it
someCheck <- as.data.frame(table(someCheck))
# View(someCheck)

# The problem with the above: There are different types of theft, etc. 
# So, before we make the table, we're going to simplify the crime categories.
crimeData_duplicate <- austinCrime2014_data_selected_zips
# View(crimeData_duplicate)
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc)) # We have all these levels with "Theft: blah..."
crimeData_duplicate$NIBRS_OffenseDesc <- gsub("Theft.*", "Theft", crimeData_duplicate$NIBRS_OffenseDesc)
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc)) # Ok, now we just have "Auto Theft" to fix
crimeData_duplicate$NIBRS_OffenseDesc <- gsub("[A-Z][a-z]+.Theft", "Theft", crimeData_duplicate$NIBRS_OffenseDesc)
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc)) # Now, "Auto Theft" has been replaced with "Theft"

# Looks like the only complicated level now is  "Homicide: Murder & Nonnegligent Manslaughter" so lets shorten it
crimeData_duplicate$NIBRS_OffenseDesc <- gsub("Homicide.*", "Homicide", crimeData_duplicate$NIBRS_OffenseDesc)
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc))

# Now make "Aggravated Assault" just "Assault" and "Burglary / \nBreaking & Entering" just "Burglary
crimeData_duplicate$NIBRS_OffenseDesc <- gsub("[A-Z][a-z]+.Assault", "Assault", crimeData_duplicate$NIBRS_OffenseDesc)
crimeData_duplicate$NIBRS_OffenseDesc <- gsub("Burglary.*", "Burglary", crimeData_duplicate$NIBRS_OffenseDesc)
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc)) # now Assault and Burlgary are also simplified

# Now, remove the un-needed column, and give the final one a proper name:
crimeData_duplicate <- crimeData_duplicate[-2]
# View(crimeData_duplicate)
crimeDataTable <- as.data.frame(table(crimeData_duplicate))
colnames(crimeDataTable) <- c("zipcode", "crime", "freq")
# View(crimeDataTable)

# Now we'll make lists of frequencies per crime and zipcode, then slap it all together
crimeLevels <- levels(factor(crimeDataTable$crime))  # Creates the list of levels (crimeLevels) as: "Assault"  "Burglary" "Homicide" "Rape"     "Robbery"  "Theft"
assault_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[1],]  # crimeLevels[1] is "Assault", and so forth. 
burglary_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[2],]
homicide_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[3],]
rape_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[4],]
robbery_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[5],]
theft_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[6],]

crime_list <- data.frame("zipcode" = assault_perZip$zipcode,
                         "assault" = assault_perZip$freq,
                         "burglary" = burglary_perZip$freq,
                         "homicide" = homicide_perZip$freq,
                         "rape" = rape_perZip$freq,
                         "robbery" = robbery_perZip$freq,
                         "theft" = theft_perZip$freq
)

# Now we have our list of crimes per zipcode
# View(crime_list)

################################################################################
##          ####################################################################
##    3.    Add in population data
##          ####################################################################
################################################################################
### Extract population per zipcode
data(df_pop_zip) # 2012 census population data, check it out: ?df_pop_zip

## Select pop data for zipcodes where we have crime data
austinPopulationPerZip <- df_pop_zip[df_pop_zip$region %in% crime_list$zipcode, ]
# View(austinPopulationPerZip)
names(austinPopulationPerZip) <- c("zipcode", "population") #rename columns from [region and value] to [zipcode and population]
# View(austinPopulationPerZip)

################################################################################
##          ####################################################################
##    4.    Add in geographical data
##          ####################################################################
################################################################################

## Check the geographic density of crimes
## Geographic area per zipcode dataset downloaded here: https://www.census.gov/geo/maps-data/data/gazetteer2015.html
##  (Direct download link: http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip )

#####  **  **  **  **  **  **  DOWNLOAD THE DATASET CSV FILE   **  **  **  **  **  **  ** 
###       ***** UNCOMMENT THIS BLOCK TO AUTOMATICALLY DOWNLOAD the geographic data: *****
# Here we're going to download the zip file and the txt file within.
# temp <- tempfile()  # See Stack Overflow post: http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
# download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip",temp)
# zipGeoSizeTable <- read.table(unz(temp, "2015_Gaz_zcta_national.txt"), sep="\t", header=TRUE)
# unlink(temp)
zipGeoSizeTable <- read.table("2015_Gaz_zcta_national.txt", sep="\t", header=TRUE)
#####  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  

dim(zipGeoSizeTable)
tail(zipGeoSizeTable)
zipcodesGeoSizeFiltered <- filter(zipGeoSizeTable, GEOID %in% austin2014_EconData_selection$zipcode) ## select out zipcodes relevant to our project.
nrow(zipcodesGeoSizeFiltered)
## The only column that is fully populated is ALAND, so we'll go with that (area of land in square meters)
# View(zipcodesGeoSizeFiltered)
## Pretty sure it's already sorted by zipcode, but just to  be sure, let's sort it by zipcode
zipcodesGeoSizeFiltered <- zipcodesGeoSizeFiltered[order(as.numeric(as.character(zipcodesGeoSizeFiltered$GEOID))),] # order by GEOID
zipcodesGeoSizeFiltered <- zipcodesGeoSizeFiltered[-c(3:7)] # Drop the columns we won't use
# View(zipcodesGeoSizeFiltered)
names(zipcodesGeoSizeFiltered) <- c("zipcode", "SqMtrs")
zipcodesGeoSizeFiltered$SqKm <- zipcodesGeoSizeFiltered$SqMtrs/1000000
# View(zipcodesGeoSizeFiltered)

################################################################################
##          ####################################################################
##    5.    Create our main dataset
##          ####################################################################
################################################################################

# Add together Econ & Population datasets
AustinData <- cbind(austin2014_EconData_selection, austinPopulationPerZip)
AustinData <- AustinData[-6] # remove duplicate zipcode
# View(AustinData)

# Add on geographical data
AustinData <- cbind(AustinData, zipcodesGeoSizeFiltered)
AustinData <- AustinData[-7] # remove duplicate zipcode
# View(AustinData)

# Add on crime data
AustinData <- cbind(AustinData, crime_list)
AustinData <- AustinData[-9] # remove duplicate zipcode
# View(AustinData)

# Give the dataset the right row names
rownames(AustinData) <- 1:nrow(AustinData)
# View(AustinData)

# Re-order our dataset: Zipcode & area first.
head(AustinData)
AustinData <- AustinData[,c(1,7,8,2:6,9:14)]
head(AustinData)
# write.table(AustinData, "StatsProject_AustinData.csv", sep=",", row.names = FALSE)

################################################################################
##          ####################################################################
##    6.    Create crime-per-capita dataset
##          ####################################################################
################################################################################

# Let's check some crime per capita stats


assault_perCapita <- AustinData$assault / AustinData$population  # crimeLevels[1] is "Assault", and so forth. 
burglary_perCapita <- AustinData$burglary / AustinData$population
homicide_perCapita <- AustinData$homicide / AustinData$population
rape_perCapita <- AustinData$rape / AustinData$population
robbery_perCapita <- AustinData$robbery / AustinData$population
theft_perCapita <- AustinData$theft / AustinData$population

totalCrimes <- AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft
totalCrimesPerCapita <- assault_perCapita + burglary_perCapita + homicide_perCapita + rape_perCapita + robbery_perCapita + theft_perCapita

crimeList_perCapita <- data.frame("zipcode" = AustinData$zipcode,
                                  "assault_perCapita" = assault_perCapita,
                                  "burglary_perCapita" = burglary_perCapita,
                                  "homicide_perCapita" = homicide_perCapita,
                                  "rape_perCapita" = rape_perCapita,
                                  "robbery_perCapita" = robbery_perCapita,
                                  "theft_perCapita" = theft_perCapita,
                                  "totalCrimes" = totalCrimes,
                                  "totalCrimesPerCapita" = totalCrimesPerCapita
)


# Let's use a for loop to apply the function "specify_decimal" to reduce the demical places shown to just 6.
#specify_decimal <- function(x, k) format(round(x, k)) # x = the column to reduce decimals on, k = number of decimal places

#for (i in 2:length(crimeList_perCapita)){
#  crimeList_perCapita[i] <- specify_decimal(crimeList_perCapita[i], 6)
#}

View(crimeList_perCapita)

# Combine CrimeList_PerCapita With Austin Data
AustinData <- cbind(AustinData, crimeList_perCapita)
AustinData <- AustinData[-15] # remove duplicate zipcode
# View(AustinData)
dim(AustinData)



################################################################################
##          ####################################################################
##    X.    Initial Analysis
##          ####################################################################
################################################################################

head(AustinData)

reg.lm <- lm(AustinData$MedianHouseholdIncome ~ ., data=AustinData)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)

VIF(reg.lm)

summary(lm(AustinData$MedianHouseholdIncome ~ AustinData$PopulationBelowPovertyLevel + AustinData$Unemployment + AustinData$MedianRent + AustinData$population + AustinData$theft + AustinData$theft_perCapita))

reg.lm <- lm(AustinData$totalCrimes ~ ., data=AustinData)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)

summary(lm(AustinData$totalCrimes ~ AustinData$PopulationBelowPovertyLevel + AustinData$MedianHouseholdIncome + AustinData$MedianRent))
summary(lm(AustinData$totalCrimes ~ AustinData$PopulationBelowPovertyLevel * AustinData$MedianHouseholdIncome * AustinData$MedianRent))

####
####  These show significant effects of crime on economic indicators
####
# Let's take a sample and use it to make a model
austinSample <- sample_n(AustinData, 30)
View(austinSample)

summary(lm(austinSample$PopulationBelowPovertyLevel ~ austinSample$assault*austinSample$rape*austinSample$theft))
summary(lm(austinSample$MedianHouseholdIncome  ~ austinSample$assault*austinSample$rape*austinSample$theft ))
summary(lm(austinSample$MedianRent  ~ austinSample$assault*austinSample$rape*austinSample$theft ))



## Select rows where PopBelowPoverty > IQR3 (PopBelowPoverty)
## Select rows where PopBelowPoverty < IQR1 (PopBelowPoverty)

# Result: Economic indicators determine median household indicators.
# Well, since we're testing for crime, let's just use the crime list, and add on whatever output we're testing
head(crime_list)
checkMedianIncome <- cbind(crime_list, AustinData$MedianHouseholdIncome)
head(checkMedianIncome)
names(checkMedianIncome)[8] <- "MedianIncome" # change name of column 8

reg.lm <- lm(checkMedianIncome$MedianIncome ~ ., data=checkMedianIncome)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)
# Result is an error b/c we're including all factors -- and that includes MedianIncome itself.  So let's just use the others:

reg.lm <- lm(checkMedianIncome$MedianIncome ~ 
               checkMedianIncome$assault + 
               checkMedianIncome$burglary + 
               checkMedianIncome$homicide + 
               checkMedianIncome$rape + 
               checkMedianIncome$robbery +
               checkMedianIncome$theft, data=checkMedianIncome)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)
# Note: there is not much difference between AIC value including all factors shown above, versus other combinations
# Therefore, AIC isn't really telling us much. Can ignore it.

# let's try crime per capita
checkMedianIncome <- cbind(crimeList_perCapita, AustinData$MedianHouseholdIncome)
head(checkMedianIncome)
names(checkMedianIncome)[8] <- "MedianIncome" # change name of column 8


plot(AustinData$theft ~ AustinData$MedianHouseholdIncome)


### Great angle/correlation
plot(AustinData$PopulationBelowPovertyLevel~AustinData$assault_perCapita)
cor(AustinData$PopulationBelowPovertyLevel,AustinData$assault_perCapita)

plot(AustinData$PopulationBelowPovertyLevel~AustinData$rape_perCapita)
cor(AustinData$PopulationBelowPovertyLevel,AustinData$rape_perCapita)

plot(AustinData$PopulationBelowPovertyLevel~AustinData$homicide_perCapita) # Doesn't seem to fit well
cor(AustinData$PopulationBelowPovertyLevel,AustinData$homicide_perCapita)

plot(AustinData$PopulationBelowPovertyLevel~AustinData$theft_perCapita)
cor(AustinData$PopulationBelowPovertyLevel,AustinData$theft_perCapita)

plot(AustinData$PopulationBelowPovertyLevel~AustinData$burglary_perCapita)
cor(AustinData$PopulationBelowPovertyLevel,AustinData$burglary_perCapita)

summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault_perCapita))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$theft_perCapita))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$burglary_perCapita))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$rape_perCapita))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$homicide_perCapita))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$robbery_perCapita))


# Review Burglary
SLR_Burglary<-lm(AustinData$burglary_perCapita~
                   +AustinData$PopulationBelowPovertyLevel
                 +AustinData$MedianHouseholdIncome
                 +AustinData$Unemployment
                 +AustinData$MedianRent
                 +AustinData$population
)
SLR_Burglary
summary(SLR_Burglary)
anova(SLR_Burglary)

multipleregression_burglary<-lm(
  AustinData$burglary_perCapita~
    AustinData$PopulationBelowPovertyLevel
  *AustinData$MedianHouseholdIncome
  *AustinData$MedianRent
  #    *AustinData$Unemployment
  #    *AustinData$population
)
summary(multipleregression_burglary)
# This models shows promise in showing the correlation between poverty and burglary



# Review Robbery
SLR_Robbery<-lm(AustinData$robbery_perCapita~
                  AustinData$PopulationBelowPovertyLevel+
                  AustinData$MedianHouseholdIncome+
                  AustinData$Unemployment+
                  AustinData$MedianRent+
                  AustinData$population)
SLR_Robbery
summary(SLR_Robbery)
anova(SLR_Robbery)

multipleregression_robbery<-lm(
  AustinData$robbery_perCapita~
    AustinData$PopulationBelowPovertyLevel
  *AustinData$MedianHouseholdIncome
  *AustinData$MedianRent
  #    *AustinData$Unemployment
  #    *AustinData$population
)
summary(multipleregression_robbery)
qqnorm(multipleregression_robbery)


# Review Assault
SLR_Assault<-lm(AustinData$assault_perCapita~
                  AustinData$PopulationBelowPovertyLevel+
                  AustinData$MedianHouseholdIncome+
                  AustinData$Unemployment+
                  AustinData$MedianRent+
                  AustinData$population)
SLR_Assault
summary(SLR_Assault)
anova(SLR_Assault)

multipleregression_assault<-lm(
  AustinData$assault_perCapita~
    AustinData$PopulationBelowPovertyLevel
  *AustinData$MedianHouseholdIncome
  *AustinData$MedianRent
  #    *AustinData$Unemployment
  #    *AustinData$population
)
summary(multipleregression_assault)
plot(multipleregression_assault)


# Review Rape
SLR_Rape<-lm(AustinData$rape_perCapita~
               AustinData$PopulationBelowPovertyLevel+
               AustinData$MedianHouseholdIncome+
               AustinData$Unemployment+
               AustinData$MedianRent+
               AustinData$population)
SLR_Rape
summary(SLR_Rape)
anova(SLR_Rape)

# Review Theft
SLR_Theft<-lm(AustinData$theft_perCapita~
                AustinData$PopulationBelowPovertyLevel+
                AustinData$MedianHouseholdIncome+
                AustinData$Unemployment+
                AustinData$MedianRent+
                AustinData$population)
SLR_Theft
summary(SLR_Theft)
anova(SLR_Theft)


# Review Homicide
SLR_Homicide<-lm(AustinData$homicide_perCapita~
                   AustinData$PopulationBelowPovertyLevel+
                   AustinData$MedianHouseholdIncome+
                   AustinData$Unemployment+
                   AustinData$MedianRent+
                   AustinData$population)
SLR_Homicide
summary(SLR_Homicide)
anova(SLR_Homicide)


AustinData2 <- AustinData

summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault/AustinData$population))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$rape))
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault_perCapita + AustinData$rape_perCapita + AustinData$burglary_perCapita + AustinData$homicide_perCapita + AustinData$robbery_perCapita + AustinData$theft_perCapita))
summary(aov(AustinData$MedianHouseholdIncome ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))
summary(aov(AustinData$MedianRent ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))

plot(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault)
plot(AustinData$assault ~ AustinData$pop)





################################################################################
##          ####################################################################
##    Z.    Specific analyses: Exploring which regression models work best for our dataset
##          ####################################################################
################################################################################

# 1. Used all rows to figure out which are the best models
# 2. Use the best models to create training model.  austinSample <- sample_n(AustinData, 30)
# a. pick models
# b. make sample
# 3. Run model on training data

################################################################################
#### Y: CrimePerCapita, X: MedianHouseholdIncome

# Starting place: Total crimes per capita vs income
multipleregression_1<-lm(
  AustinData$totalCrimesPerCapita ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

# Income vs assault
multipleregression_1<-lm(
  AustinData$assault_perCapita ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)


# Income vs burglary
multipleregression_1<-lm(
  AustinData$burglary_perCapita ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

# Income vs robbery
multipleregression_1<-lm(
  AustinData$robbery_perCapita ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

# Income vs homicide
multipleregression_1<-lm(
  AustinData$homicide ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

# Income vs rape
multipleregression_1<-lm(
  AustinData$rape ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

# Income vs theft
multipleregression_1<-lm(
  AustinData$theft ~ AustinData$MedianHouseholdIncome
)
summary(multipleregression_1)

################################################################################
#### Y: CrimePerCapita, X: MedianHouseholdIncome, MedianRent, PopBelowPoverty

# Starting place: Total crimes per capita vs income
multipleregression_1<-lm(
  AustinData$totalCrimesPerCapita ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)

## Assumption 2 Constant Variance of Residuals
results <- residuals(multipleregression_1)
plot(results)

## Assumption 3 Constant Variance of Residuals 
qqnorm(results)

## Assumption 4 Independence of residuals
acf(results)

# Income vs assault
multipleregression_1<-lm(
  AustinData$assault_perCapita ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)


# Income vs burglary
multipleregression_1<-lm(
  AustinData$burglary_perCapita ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)

# Income vs robbery
multipleregression_1<-lm(
  AustinData$robbery_perCapita ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)

# Income vs homicide
multipleregression_1<-lm(
  AustinData$homicide ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)

# Income vs rape
multipleregression_1<-lm(
  AustinData$rape ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)

# Income vs theft
multipleregression_1<-lm(
  AustinData$theft ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
summary(multipleregression_1)


################################################################################
### Note: we can say we sacrificed VIF for R-squared.  
# We're accepting some colinearity (we're using 3 economic predictors) because they allow us to have a stronger R-Squared.

##### So, we selected Assault (82%), Robbery (80%), and Burglary (70%) as our models (since they have the highest R squared)
## First, we make the Training & Test data set, at 80% to 20% ratio

AustinTrainingDataset <- sample_n(AustinData, 30, replace = FALSE)
glimpse(AustinTrainingDataset)
AustinTestDataset <- subset(AustinData, !(zipcode %in% AustinTrainingDataset$zipcode))
glimpse(AustinTestDataset)

AustinTestDataset <- data.frame(MedianHouseholdIncome = AustinTestDataset$MedianHouseholdIncome,
                                MedianRent = AustinTestDataset$MedianRent,
                                PopulationBelowPovertyLevel = AustinTestDataset$PopulationBelowPovertyLevel,
                                assaultPerCapita = AustinTestDataset$assault_perCapita,
                                robberyPerCapita = AustinTestDataset$robbery_perCapita,
                                burglaryPerCapita = AustinTestDataset$burglary_perCapita,
                                theftPerCapita = AustinTestDataset$theft_perCapita
)
glimpse(AustinTestDataset)



########################################
########################################  Interval: Confidence
########################################
## Testing to find average confidence interval of the test data, base on the model

####  Difference between Confidence Interval and Prediction Interval: ########################################################################################################################
# Reference: https://stats.stackexchange.com/questions/16493/difference-between-confidence-intervals-and-prediction-intervals
########################################################################################################################
graphics.off()
par(mfrow=c(2,4))

##################### Income vs assault 
## Training model
multipleregression_train_assault<-lm(
  assault_perCapita ~ 
    MedianHouseholdIncome * MedianRent * PopulationBelowPovertyLevel, 
  data=AustinTrainingDataset )
summary(multipleregression_train_assault)
VIF(multipleregression_train_assault)

avgCI_assault <- predict.lm(multipleregression_train_assault, newdata=AustinTestDataset, interval="confidence")
avgCI_assault


# Plotting our fit, 95% confidence interval, and actual
plot(avgCI_assault[,1],type="l",
     ylim=c(0,max(avgCI_assault[,3])+max(avgCI_assault[,3])*.1),
     main="Confidence interval vs actual: Assault per capita")
lines(avgCI_assault[,1],col="red")
lines(avgCI_assault[,2],col="green")
lines(avgCI_assault[,3],col="green")
lines(AustinTestDataset$assaultPerCapita,col="black")

##################### Income vs Robbery
## Training model
multipleregression_train_robbery<-lm(
  robbery_perCapita ~ 
    MedianHouseholdIncome * MedianRent * PopulationBelowPovertyLevel, 
  data=AustinTrainingDataset )
summary(multipleregression_train_robbery)
VIF(multipleregression_train_robbery)

avgCI_robbery <- predict.lm(multipleregression_train_robbery, newdata=AustinTestDataset, interval="confidence")
avgCI_robbery

plot(avgCI_robbery[,1],type="l",
     ylim=c(0,max(avgCI_robbery[,3]+max(avgCI_robbery[,3])*.1)),
     main="Confidence interval vs actual: Robbery per capita")
lines(avgCI_robbery[,1],col="red")
lines(avgCI_robbery[,2],col="green")
lines(avgCI_robbery[,3],col="green")
lines(AustinTestDataset$robberyPerCapita,col="black")


##################### Income vs Burglary
## Training model
multipleregression_train_burglary<-lm(
  burglary_perCapita ~ 
    MedianHouseholdIncome * MedianRent * PopulationBelowPovertyLevel, 
  data=AustinTrainingDataset )
summary(multipleregression_train_burglary)
VIF(multipleregression_train_burglary)

avgCI_burglary <- predict.lm(multipleregression_train_burglary, newdata=AustinTestDataset, interval="confidence")
avgCI_burglary

plot(avgCI_burglary[,1],type="l",
     ylim=c(0,max(avgCI_burglary[,3]+max(avgCI_burglary[,3])*.1)),
     main="Confidence interval vs actual: Burglary per capita"
)
lines(avgCI_burglary[,1],col="red")
lines(avgCI_burglary[,2],col="green")
lines(avgCI_burglary[,3],col="green")
lines(AustinTestDataset$burglaryPerCapita,col="black")

##################### Income Vs. Theft
multipleregression_train_theft<-lm(
  theft_perCapita ~ 
    MedianHouseholdIncome * MedianRent * PopulationBelowPovertyLevel, 
  data=AustinTrainingDataset )
summary(multipleregression_train_theft)
VIF(multipleregression_train_theft)

avgCI_theft <- predict.lm(multipleregression_train_theft, newdata=AustinTestDataset, interval="confidence")
avgCI_theft

plot(avgCI_theft[,1],type="l",
     ylim=c(0,max(avgCI_theft[,3]+max(avgCI_theft[,3])*.1)),
     main="Confidence interval vs actual: theft per capita"
)
lines(avgCI_theft[,1],col="red")
lines(avgCI_theft[,2],col="green")
lines(avgCI_theft[,3],col="green")
lines(AustinTestDataset$theftPerCapita,col="black")


########################################
########################################  Interval: Prediction
########################################

##################### Income vs assault 
avgPrediction_assault <- predict.lm(multipleregression_train_assault, newdata=AustinTestDataset, interval="prediction")

# Plotting our fit, prediction interval, and actual
plot(avgPrediction_assault[,1],type="l",
     ylim=c(0,max(avgPrediction_assault[,3])+max(avgPrediction_assault[,3])*.1),
     main="Prediction interval vs actual: Assault per capita")
lines(avgPrediction_assault[,1],col="red")
lines(avgPrediction_assault[,2],col="green")
lines(avgPrediction_assault[,3],col="green")
lines(AustinTestDataset$assaultPerCapita,col="black")

##################### Income vs Robbery
avgPrediction_robbery <- predict.lm(multipleregression_train_robbery, newdata=AustinTestDataset, interval="prediction")

plot(avgPrediction_robbery[,1],type="l",
     ylim=c(0,max(avgPrediction_robbery[,3]+max(avgPrediction_robbery[,3])*.1)),
     main="Prediction interval vs actual: Robbery per capita")
lines(avgPrediction_robbery[,1],col="red")
lines(avgPrediction_robbery[,2],col="green")
lines(avgPrediction_robbery[,3],col="green")
lines(AustinTestDataset$robberyPerCapita,col="black")

##################### Income vs Burglary
avgPrediction_burglary <- predict.lm(multipleregression_train_burglary, newdata=AustinTestDataset, interval="prediction")

plot(avgPrediction_burglary[,1],type="l",
     ylim=c(0,max(avgPrediction_burglary[,3]+max(avgPrediction_burglary[,3])*.1)),
     main="Prediction interval vs actual: Burglary per capita"
)
lines(avgPrediction_burglary[,1],col="red")
lines(avgPrediction_burglary[,2],col="green")
lines(avgPrediction_burglary[,3],col="green")
lines(AustinTestDataset$burglaryPerCapita,col="black")

##################### Income vs Theft
avgPrediction_theft <- predict.lm(multipleregression_train_theft, newdata=AustinTestDataset, interval="prediction")

plot(avgPrediction_theft[,1],type="l",
     ylim=c(0,max(avgPrediction_theft[,3]+max(avgPrediction_theft[,3])*.1)),
     main="Prediction interval vs actual: Theft per capita"
)
lines(avgPrediction_theft[,1],col="red")
lines(avgPrediction_theft[,2],col="green")
lines(avgPrediction_theft[,3],col="green")
lines(AustinTestDataset$theftPerCapita,col="black")


########################################
########################################
#### Testing "VIF"####
check_VIF <- lm(AustinData$MedianHouseholdIncome ~ AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
VIF(check_VIF)

check_VIF <- lm(AustinData$totalCrimes ~ AustinData$MedianHouseholdIncome * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
VIF(check_VIF)

check_VIF <- lm(AustinData$totalCrimesPerCapita ~ AustinData$assault_perCapita * AustinData$MedianRent * AustinData$PopulationBelowPovertyLevel )
VIF(check_VIF)

## Confidence vs. Predict?  I s
avgCI_assault <- predict.lm(multipleregression_train_assault, newdata=AustinTestDataset, interval="confidence")
avgCI_assault

avgCI_assault <- predict.lm(multipleregression_train_assault, newdata=AustinTestDataset, interval="prediction")
avgCI_assault 




##################################################################
######################################## PLOTS
########################################

# Income vs total crime -- Pattern, -.43 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$totalCrimes, main="Total Crime vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$totalCrimes)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$totalCrimes))

#### Unscaled:
graphics.off()
par(mfrow=c(2,3))
# Income vs homicide -- No pattern
plot(AustinData$MedianHouseholdIncome~AustinData$homicide, main="Homicide vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$homicide)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$homicide))

# Income vs assault -- Pattern, -.50 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$assault, main="Assault vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$assault)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$assault))

# Income vs burglary -- Pattern, -.50 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$burglary, main="Burglary vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$burglary)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$burglary))

# Income vs robbery -- Pattern, -.46 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$robbery, main="Robbery vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$robbery)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$robbery))

# Income vs robbery -- theft, -.46 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$theft, main="Theft vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$theft)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$theft))

# Income vs robbery -- rape, -.39 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$rape, main="Rape vs Median Household Income")
cor(AustinData$MedianHouseholdIncome,AustinData$rape)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$rape))

########################################
######################################## Scaled Plots
########################################

graphics.off()
par(mfrow=c(2,3))
# Income vs homicide -- No pattern
plot(AustinData$MedianHouseholdIncome~AustinData$homicide, main="Homicide vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$homicide)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$homicide))

# Income vs assault -.50 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$assault, main="Assault vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$assault)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$assault))

# Income vs burglary -.50 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$burglary, main="Burglary vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$burglary)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$burglary))

# Income vs robbery -.46 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$robbery, main="Robbery vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$robbery)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$robbery))

# Income vs theft -.46 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$theft, main="Theft vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$theft)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$theft))

# Income vs rape -.39 correlation coefficient
plot(AustinData$MedianHouseholdIncome~AustinData$rape, main="Rape vs Median Household Income", xlim=c(0,3150))
cor(AustinData$MedianHouseholdIncome,AustinData$rape)
abline(lm(AustinData$MedianHouseholdIncome~AustinData$rape))

