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
#   d. convert to factor data to view levels we're working with
#
# 3. Process the crime data: count up crimes per zipcode
#   a. count up the crimes we're focused on, per zipcode (Robberies [Rob], Burglaries [Burg])
#   b. create list of zipcodes where Rob or Burg have happened [MainList].  Then, create a list for each separately.
#      Compare Rob and Burg list to MainList and check to make sure all zipcodes in MainList are in Rob and Burg (using "mysetdiff()" ).
#      Then add in the missing zipcodes to Rob and Burg, with a 0, so we know neither happened in these zipcodes.
#
# 4. Map the crime data -- This is mostly just so we're ready to map once we decide on our final data to map.
#   a. convert headers from zipcode and crime count to "region" and "value", which is what zip_choropleth maps: a value to a zipcode area.
#   b. map Robberies per zipcode
#   c. map Burglaries per zipcode
#
# 5. Add in population data, and create a main dataset of crime & demographic data
#   a. use R's local data library to access 2012 population statistics: data(df_pop_zip)
#   b. select out the columns we need
#   c. create a main dataset of crime & demographic data
#
# 6. Add in geographic area data -- square meters per zipcode
#   a. download data automatically
#   b. select SqMtrs.  Convert it to SqKm.  Divide Rob and Burg by SqKm.
#   c. Output the main dataset of Economic-demographic, Crime, Population and Geographical information

################################################################################
##          ####################################################################
##    1.    Import and format 2014 neighborhood economic data
##          ####################################################################
################################################################################

austin2014_data_raw <- read_csv('https://data.austintexas.gov/resource/hcnj-rei3.csv', na = '')
glimpse(austin2014_data_raw)
nrow(austin2014_data_raw)

austin2014_data_raw$`Zip Code` # All we need is Zipcode.  Taking a look we see one row has no zipcode (one of them is NA)
austin2014_data <- austin2014_data_raw[-1,] # Remove the first row, which has no zip code (this row is the average data for all of Austin, I emailed them for confirmation)
nrow(austin2014_data)

## Grab the zip code data separately
## (Previously I had received weird output when using this command: austin2014_data$`Zip Code` )
zipCodesOfData <- fread('https://data.austintexas.gov/resource/hcnj-rei3.csv') %>%
  mutate(`Zip Code` = ifelse(`Zip Code` == "", NA, `Zip Code`)) %>%
  na.omit() %>% 
  select(`Zip Code`)
nrow(zipCodesOfData)
names(zipCodesOfData) <- "ZipCode" # Rename from "Zip Code" to "ZipCode". Recombine it with the dataset. 
austin2014_data <- cbind(austin2014_data, zipCodesOfData) # Add it on to the econ dataset

## Select a few columns for our neighborhood economic data subset
columnSelection <- c("ZipCode", "Population below poverty level", "Median household income", "Unemployment", "Median rent")
austin2014_EconData_selection <- subset(austin2014_data, select=columnSelection)
names(austin2014_EconData_selection)

## Reset row number index since we removed the first row (r kept the original index, now we're gonna reset it)
rownames(austin2014_EconData_selection) <- 1:nrow(austin2014_EconData_selection)
View(austin2014_EconData_selection)

################################################################################
##          ####################################################################
##    2.    Import and format 2014 crime data
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

## Filter crime data by zipcodes available in the neighborhood economic data subset
length(unique(austinCrime2014_data_raw$GO.Location.Zip)) # 49
length(unique(austin2014_EconData_selection$ZipCode)) # 36

austinCrime2014_data_selected_zips <- filter(austinCrime_dataset, zipcode %in% austin2014_EconData_selection$ZipCode)
glimpse(austinCrime2014_data_selected_zips)
length(unique(austinCrime2014_data_selected_zips$zipcode)) # 36
nrow(austinCrime2014_data_selected_zips)
typeof(austinCrime2014_data_selected_zips)

## Convert our crime data subset from string/char data into factorized data so we can see levels

## let's make the character data columns c("highestOffenseDesc", "NIBRS_OffenseDesc") into factors so we can check its levels
glimpse(austinCrime2014_data_selected_zips) # characters
cols <- c("highestOffenseDesc", "NIBRS_OffenseDesc") # columns with character datatype to convert to factor datatype
austinCrime2014_data_selected_zips[cols] <- lapply(austinCrime2014_data_selected_zips[cols], factor)
glimpse(austinCrime2014_data_selected_zips) # factors
# View(austinCrime2014_data_selected_zips)
levels(austinCrime2014_data_selected_zips$highestOffenseDesc) #--> looks good
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc) # output is weird:  "Burglary / \nBreaking & Entering" "Robbery"

typeof(austinCrime2014_data_selected_zips)

## Shortening the entry "Burglary / Breaking & Entering" to just "Burglary"
columnWithStringReplaced<- gsub("Burglary / \nBreaking & Entering", "Burglary", austinCrime2014_data_selected_zips$NIBRS_OffenseDesc)
austinCrime2014_data_selected_zips <- cbind(austinCrime2014_data_selected_zips, columnWithStringReplaced)
typeof(austinCrime2014_data_selected_zips)
glimpse(austinCrime2014_data_selected_zips)

austinCrime2014_data_selected_zips <- austinCrime2014_data_selected_zips[,-3] #remove old NIBRS column
glimpse(austinCrime2014_data_selected_zips)
# View(austinCrime2014_data_selected_zips)
names(austinCrime2014_data_selected_zips) <- c("zipcode", "highestOffenseDesc", "NIBRS_OffenseDesc") # make all columns have the right same
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc)

## View our data
# View(austin2014_EconData_selection)
# View(austinCrime2014_data_selected_zips)
levels(austinCrime2014_data_selected_zips$highestOffenseDesc) #--> looks good
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc) #--> looks good
glimpse(austinCrime2014_data_selected_zips)


# 
# check1 <- as.data.frame(table(austinCrime2014_data_selected_zips))
# View(check1)
# check2 <- filter(check1, check1$Freq > 0) 
# View(check2)
# 
# check2$new <- gsub("Theft.*", "Theft", check2$NIBRS_OffenseDesc)
# check2$new2 <- gsub("[A-Z][a-z]+.Theft", "Theft", check2$new)
# 
# check2$newx <-check2$new2 <- gsub("[A-Z][a-z]+.Theft.+[A-Z][a-z]", "Theft", check2$NIBRS_OffenseDesc)
# levels(factor(check2$new))
# levels(factor(check2$new2))
# 
# 
# 
# xyz <- check2
# xyz <- xyz[-c(2:4)]
# # View(xyz)
# xyz <- xyz[-4]
# # View(xyz)
# 
# 
# xyz_count_AutoTheftExcluded <- xyz[-3]
# # View(xyz_count_AutoTheftExcluded)
# xyz_count_AutoTheftExcluded = xyz_count_AutoTheftExcluded %>% 
#   group_by(zipcode, new) %>% 
#   mutate(occ = n())
# 
# xyz_count_AutoTheftIncludedInTheft <- xyz[-2]
# # View(xyz_count_AutoTheftIncludedInTheft)
# xyz_count_AutoTheftIncludedInTheft = xyz_count_AutoTheftIncludedInTheft %>% 
#   group_by(zipcode, new2) %>% 
#   mutate(occ = n())
# 
# View(xyz_count_AutoTheftExcluded)
# View(xyz_count_AutoTheftIncludedInTheft)




################################################################################
##          ####################################################################
##    3.    Process the crime data: count up crimes per zipcode
##          ####################################################################
################################################################################

## Next step: counting "NIBRS crimes" per zipcode ("NIBRS crimes" is the name of the column with shorter, less complex descriptions of crime, so we went with it)
zipCrimeCountNIBRS <- austinCrime2014_data_selected_zips[,-2] # First, we need the right columns
glimpse(zipCrimeCountNIBRS)

# This gives us count of NIBRS_OffenseDesc per Zipcode
zipCrimeCountNIBRS = zipCrimeCountNIBRS %>% 
  group_by(zipcode, NIBRS_OffenseDesc) %>% 
  mutate(occ = n())

# View(zipCrimeCountNIBRS)

## filter and group the crimes (burglary, robbery) by the zipcodes present
## this is using dplyr
rob_and_burg_perZip <- zipCrimeCountNIBRS %>%
  group_by(zipcode, occ, NIBRS_OffenseDesc) %>%
  summarise() %>%
  select(zipcode=zipcode, occ, NIBRS_OffenseDesc)

# View(rob_and_burg_perZip)

## let's check unique zipcodes
length(unique(rob_and_burg_perZip$zipcode)) # 36 zipcodes present

# select out all zipcodes with robbery -- something like this:
# select zipcode, NIBRS_OffenseDesc
# where NIBRS_OffenseDesc == robbery

robberyPerZip <- filter(rob_and_burg_perZip, NIBRS_OffenseDesc == "Robbery")
# View(robberyPerZip)

####
# The next step shows:
# Using 'mysetdiff' to check the difference between set of zipcodes between:
# the main set (all zipcodes for which there have been burglaries or robberies)
# and the subset (comparing it to just Burglaries, or to just Robberies)
# Then, we'll add on the missing zipcodes to Burglaries as a 0, and to robberies as a 0,
# this way we'll have Robbery and Burglary values for all zipcodes present in the main set.

# I found this function here: http://stackoverflow.com/questions/21574214/finding-elements-that-do-not-overlap-between-two-vectors
mysetdiff<-function (x, y, multiple=FALSE) 
{
  x <- as.vector(x)
  y <- as.vector(y)
  if (length(x) || length(y)) {
    if (!multiple) {
      unique( x[match(x, y, 0L) == 0L])  
    }else  x[match(x, y, 0L) == 0L] 
  } else x
}

# This shows the one zipcode missing in robberyPerZip$zipcode
diff_zip_Robbery <- mysetdiff(rob_and_burg_perZip$zipcode, robberyPerZip$zipcode)
print(diff_zip_Robbery)   # This shows 6 zipcodes don't have robberies

# need to create list: 1 row, 3 columns, then rbind it
typeof(robberyPerZip)
# View(robberyPerZip)

# This creates a list: the zipcodes in zipdiff, each with a 0 for occurances of robbery, along with the "robbery" label,
# in this form, for 6 rows (i.e. length(diff_zip_Robbery)):  <zipcode>, 0, "Robbery" 
list_to_append <- list(diff_zip_Robbery, c(rep.int(0, length(diff_zip_Robbery))), c(rep("Robbery", length(diff_zip_Robbery))))  # this is a list of zipcodes without robberies: 6 zipcodes, 6 zeros, 6 row with "robbery"
names(list_to_append) <- c("zipcode", "occ", "NIBRS_OffenseDesc")
robberyPerZip_done <- rbind(robberyPerZip, list_to_append)
View(robberyPerZip_done)

# Same thing as last step, but with burglary instead of robbery:
# select out all zipcodes with burglary
burglaryPerZip <- filter(rob_and_burg_perZip, NIBRS_OffenseDesc == "Burglary")
View(burglaryPerZip) 

# This shows the zipcodes missing in burglaryPerZip$zipcode
diff_zip_Burglary <- mysetdiff(rob_and_burg_perZip$zipcode, burglaryPerZip$zipcode)
print(diff_zip_Burglary)

newMatrix <- cbind(diff_zip_Burglary,  c(rep.int(0, length(diff_zip_Burglary))), c(rep("Burglary", length(diff_zip_Burglary))))
colnames(newMatrix) <- c("zipcode", "occ", "NIBRS_OffenseDesc")
# View(newMatrix)

## make both (a matrix, newMatrix) and (a list, burglaryPerZip) into data frames in order to combine them
mat_df <- data.frame(newMatrix)
burg_df <- data.frame(burglaryPerZip)

# doesn't work b/c they're different object types:
# burglaryPerZip_done <- rbind(burglaryPerZip, newMatrix)

# but since we converted into dataframe, this works:
burglaryPerZip_done <- rbind(burg_df, mat_df)
class(burglaryPerZip_done)
nrow(burglaryPerZip_done)

View(burglaryPerZip_done)
burglaryPerZip_done <- as.list(burglaryPerZip_done) # --> convert back into a list

## Now we have a list of robberies and burglaries per zipcode, including those with zero per zipcode:
View(burglaryPerZip_done)
View(robberyPerZip_done)

# Need to provide list of 'region' (i.e. zipcode) and 'value' (i.e. numeric value to map)
# b/c this is the format requires by choroplethr's zip_choropleth function (check out ?zip_choropleth)
## Burglary:
Burglary_occuranceByZip <- burglaryPerZip_done[-3] #take off the crime description column
names(Burglary_occuranceByZip) <- c("region", "value") # rename to region, value
Burglary_occuranceByZip <- data.frame(Burglary_occuranceByZip) # make it a dataframe
sortedData_BurglaryPerZip <- Burglary_occuranceByZip[order(as.numeric(as.character(Burglary_occuranceByZip$value))),] # order by value 

class(sortedData_BurglaryPerZip)
glimpse(sortedData_BurglaryPerZip)
sortedData_BurglaryPerZip

## Robbery:
Robbery_occuranceByZip <- robberyPerZip_done[-3] #take off the crime description column
names(Robbery_occuranceByZip) <- c("region", "value") # rename to region, value
Robbery_occuranceByZip <- data.frame(Robbery_occuranceByZip) # make it a dataframe
sortedData_RobberyPerZip <- Robbery_occuranceByZip[order(as.numeric(as.character(Robbery_occuranceByZip$value))),] # order by value 

class(sortedData_RobberyPerZip)
glimpse(sortedData_RobberyPerZip)
sortedData_RobberyPerZip

################################################################################
##          ####################################################################
##    4.    Map the crime data
##          ####################################################################
################################################################################
## Now we're ready to map.  More info on R's choroplethr package from its creator, Ari Lamstein: http://www.arilamstein.com/open-source/

## Robberies map:
sortedData_RobberyPerZip$region <- as.character(sortedData_RobberyPerZip$region)
class(sortedData_RobberyPerZip$value)
zip_choropleth(sortedData_RobberyPerZip, 
               zip_zoom = sortedData_RobberyPerZip$region, 
               title      = "Robbery occurances by zipcode",
               legend     = "Robberies",
               num_colors   = 1
) + coord_map()

## Burglaries map:
sortedData_BurglaryPerZip
sortedData_BurglaryPerZip_toCorrect <- sortedData_BurglaryPerZip
str(sortedData_BurglaryPerZip_toCorrect)
sortedData_BurglaryPerZip_toCorrect$value <- as.numeric(as.character(sortedData_BurglaryPerZip_toCorrect$value))
# View(sortedData_BurglaryPerZip_toCorrect)

zip_choropleth(sortedData_BurglaryPerZip_toCorrect, 
               zip_zoom = sortedData_BurglaryPerZip_toCorrect$region, 
               title      = "Burglary occurances by zipcode",
               legend     = "Burglaries",
               num_colors   = 1
) + coord_map()


################################################################################
##          ####################################################################
##    5.    Add in population data, and create a main dataset of crime & demographic data
##          ####################################################################
################################################################################
### Extract population per zipcode
data(df_pop_zip) # 2012 census population data, check it out: ?df_pop_zip

## Select pop data for zipcodes where we have crime data
austinPopulation2012_perCrimeZip <- df_pop_zip[df_pop_zip$region %in% rob_and_burg_perZip$zipcode, ]
View(austinPopulation2012_perCrimeZip)

## Next, we're going to make one big dataframe with our selected variables
## might need dplyr: https://www.rdocumentation.org/packages/dplyr/versions/0.5.0
## First though, let's gonna check out the data and do some sorting.

austinPopulation2012_perCrimeZip # selected zipcodes
nrow(sortedData_RobberyPerZip)
nrow(austinPopulation2012_perCrimeZip)

# Sort each one by zipcode, then select out values we want
austinPopulation2012_perCrimeZip_byZip <- austinPopulation2012_perCrimeZip[order(as.numeric(as.character(austinPopulation2012_perCrimeZip$region))),] # Creating main CrimePerZip dataset, ordered by zipcode
sortedData_RobberyPerZip_ByZip <- Robbery_occuranceByZip[order(as.numeric(as.character(Robbery_occuranceByZip$region))),] # Creating sorted RobData, ordered by zipcode
sortedData_BurglaryPerZip_ByZip <- Burglary_occuranceByZip[order(as.numeric(as.character(Burglary_occuranceByZip$region))),] # Creating sorted BurgData, ordered by zipcode

# View(austinPopulation2012_perCrimeZip_byZip)
# View(sortedData_RobberyPerZip_ByZip)
# View(sortedData_BurglaryPerZip_ByZip)

##  Here's where we are selecting specific variables
austin_PopCrime_perZip <- data.frame("zipcode" = austinPopulation2012_perCrimeZip_byZip$region,
                                     "population" = austinPopulation2012_perCrimeZip_byZip$value,
                                     "robberies" = sortedData_RobberyPerZip_ByZip$value, 
                                     "burglaries" = sortedData_BurglaryPerZip_ByZip$value) 
austin_PopCrime_perZip$burglaries <- as.numeric(as.character(austin_PopCrime_perZip$burglaries)) # as.numeric(as.character()) info: http://stackoverflow.com/questions/22790529/the-as-numeric-function-changes-the-values-in-my-dataframe
austin_PopCrime_perZip$rob_by_pop <- austin_PopCrime_perZip$robberies/austin_PopCrime_perZip$population
austin_PopCrime_perZip$burg_by_pop <- austin_PopCrime_perZip$burglaries/austin_PopCrime_perZip$population

## Next, let's remove some of those decimal places.  
specify_decimal <- function(x, k) format(round(x, k), nsmall=k) # Found this function here on Stack Overflow http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
austin_PopCrime_perZip$rob_by_pop <- specify_decimal(austin_PopCrime_perZip$rob_by_pop, 6)
austin_PopCrime_perZip$burg_by_pop <- specify_decimal(austin_PopCrime_perZip$burg_by_pop, 6)
View(austin_PopCrime_perZip)

## and view Crime per Population (Rob/Burg Per Capita) as percentage
austin_PopCrime_perZip$rob_by_pop_pct <- percent(austin_PopCrime_perZip$rob_by_pop)
austin_PopCrime_perZip$burg_by_pop_pct <- percent(austin_PopCrime_perZip$burg_by_pop)
View(austin_PopCrime_perZip)

## Let's plot by percentage of population

# Take the main set we just made, select out zipcode & value to map
rob_as_pct_pop_toMap$region <- austin_PopCrime_perZip$zipcode
rob_as_pct_pop_toMap$value <- austin_PopCrime_perZip$rob_by_pop
rob_as_pct_pop_toMap <- data.frame(rob_as_pct_pop_toMap)
# View(rob_as_pct_pop_toMap)

rob_as_pct_pop_toMap$value <- as.numeric(as.character(rob_as_pct_pop_toMap$value))
glimpse(rob_as_pct_pop_toMap)
zip_choropleth(rob_as_pct_pop_toMap, 
               zip_zoom = rob_as_pct_pop_toMap$region, 
               title      = "Burglary by zipcode as % of population",
               legend     = "Burglaries as % of population",
               # reference_map = T,
               num_colors = 1
) + coord_map()

##
econData <- austin2014_EconData_selection # grabbing econ data
econData <- econData[order(as.numeric(as.character(econData$ZipCode))),] # sorted econ data
ZipDiff <- mysetdiff(econData$ZipCode, austin_PopCrime_perZip$zipcode) # find the difference between EconData & CrimeData
econData <- subset(econData,  ! ZipCode %in% ZipDiff ) # now there's 36 rows
nrow(econData)

rownames(econData) <- 1:nrow(econData) # rename the rows so we don't skip any indexes (since some were subtracted out)
View(econData)


## Now, combining EconData with CrimeData
austinData <- cbind(econData, austin_PopCrime_perZip)
View(austinData)
# remove the duplicate zipcode column
austinData <- austinData[-6]
# View(austinData)



################################################################################
##          ####################################################################
##    6.    Add in geographic area data -- square meters per zipcode
##          ####################################################################
################################################################################

## Check square miles of each zipcode for crime density mapping
## Geographic area per zipcode dataset downloaded here: https://www.census.gov/geo/maps-data/data/gazetteer2015.html
##  (Direct download link: http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip )

## NOTE: If you need to download the table manually: unpack it, put it in the right directory and read it with this:
# zipGeoTable <- read.table("2015_Gaz_zcta_national.txt", sep="\t", header=TRUE)

## Here we're going to download the zip file and the txt file within.
temp <- tempfile()  # See Stack Overflow post: http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_zcta_national.zip",temp)
zipGeoSizeTable <- read.table(unz(temp, "2015_Gaz_zcta_national.txt"), sep="\t", header=TRUE)
unlink(temp)

dim(zipGeoSizeTable)
tail(zipGeoSizeTable)
zipcodesGeoSizeFiltered <- filter(zipGeoTable, GEOID %in% austin_PopCrime_perZip$zipcode) ## select out zipcodes relevant to our project.
nrow(zipcodesGeoSizeFiltered)
## The only column that is fully populated is ALAND, so we'll go with that (area of land in square meters)
View(zipcodesGeoSizeFiltered)
## Pretty sure it's already sorted by zipcode, but just to  be sure, let's sort it by zipcode
zipcodesGeoSizeFiltered <- zipcodesGeoSizeFiltered[order(as.numeric(as.character(zipcodesGeoSizeFiltered$GEOID))),] # order by GEOID
zipcodesGeoSizeFiltered <- zipcodesGeoSizeFiltered[-c(3:7)] # Drop the columns we won't use
head(zipcodesGeoSizeFiltered,3)
names(zipcodesGeoSizeFiltered) <- c("zipcode", "SqMtrs") # give more readable names
head(zipcodesGeoSizeFiltered,3)

## add SqMtrs onto the main dataset
Austin_Econ_Crime_Pop_Geo <- cbind(austinData, zipcodesGeoSizeFiltered$SqMtrs)
View(Austin_Econ_Crime_Pop_Geo)
colnames(Austin_Econ_Crime_Pop_Geo)[13] <- "SqMtrs" # rename our new column to SqMtrs
Austin_Econ_Crime_Pop_Geo$SqKm <- Austin_Econ_Crime_Pop_Geo$SqMtrs/1000000 # create SqKm
Austin_Econ_Crime_Pop_Geo$RobPerSqKm <- Austin_Econ_Crime_Pop_Geo$robberies/Austin_Econ_Crime_Pop_Geo$SqKm
Austin_Econ_Crime_Pop_Geo$BurgPerSqKm <- Austin_Econ_Crime_Pop_Geo$burglaries/Austin_Econ_Crime_Pop_Geo$SqKm
View(Austin_Econ_Crime_Pop_Geo)
length(Austin_Econ_Crime_Pop_Geo)

## Output our table
write.table(Austin_Econ_Crime_Pop_Geo, "Austin_Econ_Crime_Pop_Geo.csv", sep=",", row.names = FALSE)


## Map it:  Example map.
# make data frame
mapThis <- data.frame(region=Austin_Econ_Crime_Pop_Geo$ZipCode, value=Austin_Econ_Crime_Pop_Geo$RobPerSqKm)
View(mapThis)
glimpse(mapThis)
mapThis$region <- factor(mapThis$region)
mapThis$value <- as.numeric(mapThis$value)
glimpse(mapThis)

zip_choropleth(mapThis, 
               zip_zoom = mapThis$region,
               title      = "Robberies per Sq Km",
               legend     = "Robberies",
               # reference_map = T,
               num_colors = 1
) + coord_map()

mapThis <- data.frame(region=Austin_Econ_Crime_Pop_Geo$ZipCode, value=Austin_Econ_Crime_Pop_Geo$BurgPerSqKm)
View(mapThis)
glimpse(mapThis)
mapThis$region <- factor(mapThis$region)
mapThis$value <- as.numeric(mapThis$value)
glimpse(mapThis)

zip_choropleth(mapThis, 
               zip_zoom = mapThis$region,
               title      = "Burglaries per Sq Km",
               legend     = "Burglaries",
               # reference_map = T,
               num_colors = 1
) + coord_map()
?zip_choropleth


################################################################################
##          ####################################################################
##    X.          EXTRA STUFF
##          ####################################################################
################################################################################

# I decided to go ahead and add up all the crimes per zipcode

# This shows that as.data.frame(()) will give us a list of frequencies per crime in each zipcode:
someCheck <- austinCrime2014_data_selected_zips[-2]
someCheck <- as.data.frame(table(someCheck))
View(someCheck)

# The problem: There are different types of theft, etc.  So, before we make the table, 
# let's simplify the crime categories.

crimeData_duplicate <- austinCrime2014_data_selected_zips
levels(factor(crimeData_duplicate$NIBRS_OffenseDesc)) # We have all these levels with "Theft: blah..."
crimeData_duplicate$correctTheft <- gsub("Theft.*", "Theft", crimeData_duplicate$NIBRS_OffenseDesc)
levels(factor(crimeData_duplicate$correctTheft)) # Ok, now we just have "Auto Theft" to fix
crimeData_duplicate$correctTheft2 <- gsub("[A-Z][a-z]+.Theft", "Theft", crimeData_duplicate$correctTheft)
levels(factor(crimeData_duplicate$correctTheft2)) # Now, "Auto Theft" has been replaced with "Theft"
# Looks like the only complicated level now is  "Homicide: Murder & Nonnegligent Manslaughter" so lets shorten it
crimeData_duplicate$correctHomicide <- gsub("Homicide.*", "Homicide", crimeData_duplicate$correctTheft2)
levels(factor(crimeData_duplicate$correctHomicide))
# Now make "Aggravated Assault" just "Assault"
crimeData_duplicate$correctHomicide <- gsub("[A-Z][a-z]+.Assault", "Assault", crimeData_duplicate$correctHomicide)
levels(factor(crimeData_duplicate$correctHomicide)) # Now it's just Assault
# Now, remove the un-needed columns, and give the final one a proper name:
View(crimeData_duplicate)
crimeData_duplicate <- crimeData_duplicate[-c(2:5)]
View(crimeData_duplicate)


crimeDataTable <- as.data.frame(table(crimeData_duplicate))
colnames(crimeDataTable) <- c("zipcode", "crime", "freq")
View(crimeDataTable)

# Now we'll make lists of frequencies per crime and zipcode, then slap it all together
crimeLevels <- levels(factor(crimeDataTable$crime))
assault_perZip <- crimeDataTable[crimeDataTable[, "crime"] == crimeLevels[1],]
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

View(crime_list)
View(austinData)

newAustinDataset <- cbind(austinData, crime_list)
View(newAustinDataset)

# Add on these variables if possible:
# concealed carry per zipcode
# number of cops per 1000 people


# Hypothesis testing is initial testing, ultimate goal is MLR with the significant variables
# Significance testing is a type of hypothesis testing
# Show we've done some descriptive & some hypothesis testing before we do MLR


# Validation (training & test data)
# Make sure we Check for assumptions
# Cross validation
# Pick 30 as training, and 2 as test
# Try it 10 or 100 times

# Tukey HSD---
# Anova & TukeyHSD
# anovaWait=aov(WaitTime~factor(Day),data=pronto)
# summary(anovaWait)
# TukeyHSD(anovaWait)

# What do we want to know?

# Pick 2 or 3 hypothesis
# "Does income have anyhting to do with robberies/burglaries?"
# "Does population size have anything to do with crime in general?"
# Pick something with a correlation.
# Then look at neighborhoods-- where is there a correlation where is there not a correlation

# Outline of our project paper:###################
# max of 15 pages

# Introduction
# Hypothesis
# Assumptions
# "Validation Analysis"  --> there is a script showing this (i think)
# Should we discuss other studies on crime/economy?

# Discuss our training data
# Discuss the model we create based on the training model
# Test the data

# Show Choropleth Maps of data: 
# 1. actual map of data, 
# 2. a map of data based on our model & inserting hypothetical values as parameters for the prediction

