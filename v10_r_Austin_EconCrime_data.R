####################
####################
## Install packages & load libraries:
# install.packages(c("choroplethr", "choroplethrMaps", "ggplot2", "devtools", "data.table", "readr", "formattable"))
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
## going to use the devtools package from CRAN to install choroplethrZip from github
library(devtools)
# install_github('arilamstein/choroplethrZip')
library(choroplethrZip)
library(data.table)
library(readr)
## Might need to do this, not sure:
# detach("package:plyr", unload=TRUE)
library(dplyr)
library(formattable)
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

austin2014_data_raw <- read_csv('https://data.austintexas.gov/resource/hcnj-rei3.csv', na = '')
glimpse(austin2014_data_raw)
nrow(austin2014_data_raw)

austin2014_data_raw$`Zip Code` # Taking a look at the Zipcode column, we see one row has no zipcode (one of them is NA)
austin2014_data <- austin2014_data_raw[-1,] # Remove the first row, which has no zip code (this row is the average data for all of Austin, I emailed them for confirmation)
nrow(austin2014_data)
View(austin2014_data)

## Select a few columns for our neighborhood economic data subset
columnSelection <- c("Zip Code", "Population below poverty level", "Median household income", "Unemployment", "Median rent")
austin2014_EconData_selection <- subset(austin2014_data, select=columnSelection)
names(austin2014_EconData_selection) <- c("zipcode", "Population below poverty level", "Median household income", "Unemployment", "Median rent")

## Reset row number index since we removed the first row (r kept the original index, now we're gonna reset it)
rownames(austin2014_EconData_selection) <- 1:nrow(austin2014_EconData_selection)
View(austin2014_EconData_selection)

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

## NOTE: You need to download this entire dataset (40.6k rows):
## https://data.austintexas.gov/dataset/Annual-Crime-2014/7g8v-xxja/about
## This command allows direct download, but limited to 1000 rows, so we didn't use it: austinCrime2014_data_raw <- read_csv('https://data.austintexas.gov/resource/7g8v-xxja.csv', na = '')

## This uses the csv file you downloaded from the above link, so make sure you have it in the right directory:
austinCrime2014_data_raw <- read.csv('Annual_Crime_2014.csv', na = '')
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
View(austinCrime_dataset)
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
View(someCheck)

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
View(crimeDataTable)

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
View(crime_list)

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

## Check square miles of each zipcode for crime density mapping
## Geographic area per zipcode dataset downloaded here: https://www.census.gov/geo/maps-data/data/gazetteer2015.html
##  (Direct download link: http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip )

## NOTE: If you need to download the table manually: unpack it, put it in the right directory and read it with this:
# zipGeoTable <- read.table("2015_Gaz_zcta_national.txt", sep="\t", header=TRUE)


###       ***** You'll need to uncomment this block to download the geographic data: *****        #####
# Here we're going to download the zip file and the txt file within.
temp <- tempfile()  # See Stack Overflow post: http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip",temp)
zipGeoSizeTable <- read.table(unz(temp, "2015_Gaz_zcta_national.txt"), sep="\t", header=TRUE)
unlink(temp)

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
View(zipcodesGeoSizeFiltered) 

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

# Give the dataset the right row names
rownames(AustinData) <- 1:nrow(AustinData)
View(AustinData)

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

crimeList_perCapita <- data.frame("zipcode" = AustinData$zipcode,
                         "assault_perCapita" = assault_perCapita,
                         "burglary_perCapita" = burglary_perCapita,
                         "homicide_perCapita" = homicide_perCapita,
                         "rape_perCapita" = rape_perCapita,
                         "robbery_perCapita" = robbery_perCapita,
                         "theft_perCapita" = theft_perCapita
)

# Let's use a for loop to apply the function "specify_decimal" to reduce the demical places shown to just 6.
specify_decimal <- function(x, k) format(round(x, k)) # x = the column to reduce decimals on, k = number of decimal places

for (i in 2:length(crimeList_perCapita)){
  crimeList_perCapita[i] <- specify_decimal(crimeList_perCapita[i], 6)
}

View(crimeList_perCapita)



################################################################################
##          ####################################################################
##    X.    Analysis
##          ####################################################################
################################################################################

head(AustinData)

reg.lm <- lm(AustinData$MedianHouseholdIncome ~ ., data=AustinData)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)
# Result: Economic indicators determine median household indicators.
# Well, since we're testing for crime, let's just use the crime list, and add on whatever output we're testing
head(crime_list)
checkMedianIncome <- cbind(crime_list, AustinData$MedianHouseholdIncome)
head(checkMedianIncome)
names(checkMedianIncome)[8] <- "MedianIncome" # change name of column 8

reg.lm <- lm(checkMedianIncome$MedianIncome ~ ., data=checkMedianIncome)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)
# Result is an error

# let's try crime per capita
checkMedianIncome <- cbind(crimeList_perCapita, AustinData$MedianHouseholdIncome)
head(checkMedianIncome)
names(checkMedianIncome)[8] <- "MedianIncome" # change name of column 8
reg.lm <- lm(checkMedianIncome$MedianIncome ~ ., data=checkMedianIncome)
sum.reg.lm <- summary(reg.lm)
step.1<-step(reg.lm,direction="backward",trace=T)

plot(AustinData$theft ~ AustinData$MedianHouseholdIncome)

plot(AustinData$theft ~ AustinData$MedianRent)
plot(AustinData$population ~ crimeList_perCapita$homicide_perCapita)

# All variables in AustinData vs pop below poverty
summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))
model_A <- summary(lm(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))
step(lm(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft), direction="forward")

model_A <- lm(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft)
coefficients(model_A) # model coefficients
confint(model_A, level=0.95) # CIs for model parameters 
fitted(model_A) # predicted values
residuals(model_A) # residuals
anova(model_A) # anova table 
vcov(model_A) # covariance matrix for model parameters 
influence(model_A) 

kappa(model_A)

summary(aov(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$rape + AustinData$theft))
our_lm <- lm(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault + AustinData$rape + AustinData$theft)
summary(our_lm)

kappa(our_lm)

summary(aov(AustinData$MedianHouseholdIncome ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))
summary(lm(AustinData$MedianHouseholdIncome ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))
summary(aov(AustinData$MedianRent ~ AustinData$assault + AustinData$burglary + AustinData$homicide + AustinData$rape + AustinData$robbery + AustinData$theft))


plot(AustinData$PopulationBelowPovertyLevel ~ AustinData$assault)
plot(AustinData$assault ~ AustinData$pop)
# Outlier removal
checkData <- AustinData
head(checkData)


# Let's take a sample and use it to make a model
austinSample <- sample_n(AustinData, 30)
# View(austinSample)


summary(lm(austinSample$PopulationBelowPovertyLevel ~ austinSample$assault*austinSample$rape*austinSample$theft))
summary(lm(austinSample$MedianHouseholdIncome  ~ austinSample$assault*austinSample$rape*austinSample$theft ))
summary(lm(austinSample$MedianRent  ~ austinSample$assault*austinSample$rape*austinSample$theft ))


################################################################################
##          ####################################################################
##    N.    Notes
##          ####################################################################
################################################################################


# 
# # Check for:
# # collinearity
# # 12.4 - Detecting Multicollinearity Using Variance Inflation Factors
# # https://onlinecourses.science.psu.edu/stat501/node/347
# 
# plot(crimeList_perCapita$assault_perCapita, crimeList_perCapita$burglary_perCapita)
# aov(crimeList_perCapita, crimeList_perCapita$assault_perCapita~crimeList_perCapita$burglary_perCapita)
# plot(crimeList_perCapita)
# 
# plot(AustinData$MedianHouseholdIncome~AustinData$theft)
# boxplot(AustinData$MedianHouseholdIncome~ AustinData$Unemployment)
# 
# 
# head(AustinData)

blah <- levels(austinCrime2014_data_selected_zips$highestOffenseDesc)
blah2 <- levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc)
blah3 <- levels(factor(crimeData_duplicate$NIBRS_OffenseDesc))
write.table(blah, "levels.csv", sep=",", row.names = FALSE)
write.table(blah2, "levels2.csv", sep=",", row.names = FALSE)
write.table(blah2, "levels3.csv", sep=",", row.names = FALSE)
