## Note: the first set of libraries won't be needed yet;
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
# # use the devtools package from CRAN to install choroplethrZip from github
# # install.packages("devtools")
library(devtools)
# install_github('arilamstein/choroplethrZip')
library(choroplethrZip)
library(data.table)

# These are needed:
library(readr)
detach("package:plyr", unload=TRUE) # <- might not be necessary. 
library(dplyr)
#
# Import & inspect 2014 neighborhood economic data
#
austin2014_data_raw <- read_csv('https://data.austintexas.gov/resource/hcnj-rei3.csv', na = '')
glimpse(austin2014_data_raw)
nrow(austin2014_data_raw)
# # View(austin2014_data_raw)

# Clean it: Remove the first row, which has no zip code (this row is the average data for all of Austin)
austin2014_data <- austin2014_data_raw[-1,]
# # View(austin2014_data)
nrow(austin2014_data) # now there's two less rows. <-- Solution: on line above, check only by zipcode: na.omit(data_raw$Zipcode)
              # or, get the alternative from StackOverflow

#
# Grab the zip code data separately
# (when I tried to grab it just by doing: austin2014_data$`Zip Code` the outputted column looks weird)

zipCodesOfData <- fread('https://data.austintexas.gov/resource/hcnj-rei3.csv') %>%
  mutate(`Zip Code` = ifelse(`Zip Code` == "", NA, `Zip Code`)) %>%
  na.omit() %>% 
  select(`Zip Code`)
nrow(zipCodesOfData)
# View(zipCodesOfData)

# Rename it from "Zip Code" to "ZipCode". Recombine it with the dataset. 
names(zipCodesOfData) <- "ZipCode"
austin2014_data <- cbind(austin2014_data, zipCodesOfData)
# View(austin2014_data)

# Select a few columns for our neighborhood economic data subset
columnSelection <- c("ZipCode", "Population below poverty level", "Median household income", "Unemployment", "Median rent")
austin2014_EconData_selection <- subset(austin2014_data, select=columnSelection)
names(austin2014_EconData_selection)
# View(austin2014_EconData_selection)

# Reset row number index since we removed the first row (r kept the original index, now we're gonna reset it)
rownames(austin2014_EconData_selection) <- 1:nrow(austin2014_EconData_selection)
View(austin2014_EconData_selection)


#
# Import crime data
#

# Import data
austinCrime2014_data_raw <- read_csv('https://data.austintexas.gov/resource/7g8v-xxja.csv', na = '')
glimpse(austinCrime2014_data_raw)
nrow(austinCrime2014_data_raw)

# How many unique zipcodes?
length(unique(austinCrime2014_data_raw$`GO Location Zip`))
# # View(austinCrime2014_data_raw)

# Select and rename required columns
columnSelection_Crime <- c("GO Location Zip", "GO Highest Offense Desc", "Highest NIBRS/UCR Offense Description")
austinCrime_dataset <- select(austinCrime2014_data_raw, one_of(columnSelection_Crime))
names(austinCrime_dataset) <- c("zipcode", "highestOffenseDesc", "NIBRS_OffenseDesc")
glimpse(austinCrime_dataset)
nrow(austinCrime_dataset)

# Filter crime data by zipcodes available in the neighborhood economic data subset
length(unique(austinCrime2014_data_raw$`GO Location Zip`)) # 36
length(unique(austin2014_EconData_selection$ZipCode))

austinCrime2014_data_selected_zips <- filter(austinCrime_dataset, zipcode %in% austin2014_EconData_selection$ZipCode)
glimpse(austinCrime2014_data_selected_zips)
length(unique(austinCrime2014_data_selected_zips$zipcode)) # 31
nrow(austinCrime2014_data_selected_zips)
typeof(austinCrime2014_data_selected_zips)



#
# Convert our crime data subset from string/char data into factorized data so we can see levels
#

# let's make the character data columns c("highestOffenseDesc", "NIBRS_OffenseDesc") into factors so we can check its levels
glimpse(austinCrime2014_data_selected_zips) # characters
cols <- c("highestOffenseDesc", "NIBRS_OffenseDesc") # columns with character datatype to convert to factor datatype
austinCrime2014_data_selected_zips[cols] <- lapply(austinCrime2014_data_selected_zips[cols], factor)
glimpse(austinCrime2014_data_selected_zips) # factors

# View(austinCrime2014_data_selected_zips)

levels(austinCrime2014_data_selected_zips$highestOffenseDesc) #--> looks good
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc) # output is weird:  "Burglary / \nBreaking & Entering" "Robbery"

typeof(austinCrime2014_data_selected_zips)

## Shortening the entry "Burglary / Breaking & Entering" to just "Burglary
columnWithStringReplaced<- gsub("Burglary / \nBreaking & Entering", "Burglary", austinCrime2014_data_selected_zips$NIBRS_OffenseDesc)
austinCrime2014_data_selected_zips <- cbind(austinCrime2014_data_selected_zips, columnWithStringReplaced)
typeof(austinCrime2014_data_selected_zips)
austinCrime2014_data_selected_zips <- austinCrime2014_data_selected_zips[,-3] #remove old NIBRS column
glimpse(austinCrime2014_data_selected_zips)
# View(austinCrime2014_data_selected_zips)
names(austinCrime2014_data_selected_zips) <- c("zipcode", "highestOffenseDesc)", "NIBRS_OffenseDesc") # make all columns have the right same
glimpse(austinCrime2014_data_selected_zips)

# # View our data
# View(austin2014_EconData_selection)
# View(austinCrime2014_data_selected_zips)
levels(austinCrime2014_data_selected_zips$highestOffenseDesc) #--> looks good
levels(austinCrime2014_data_selected_zips$NIBRS_OffenseDesc) #--> looks good
glimpse(austinCrime2014_data_selected_zips)

#
# Next step: counting NIBRS crimes per zipcode
#
zipCrimeCountNIBRS <- austinCrime2014_data_selected_zips[,-2]
glimpse(zipCrimeCountNIBRS)


# This gives us count of NIBRS_OffenseDesc per Zipcode
zipCrimeCountNIBRS = zipCrimeCountNIBRS %>% 
  group_by(zipcode, NIBRS_OffenseDesc) %>% 
  mutate(occ = n())

# View(zipCrimeCountNIBRS)

# filter and group the crimes (burglary, robbery) by the zipcodes present
# this is using dplyr
rob_and_burg_perZip <- zipCrimeCountNIBRS %>%
  group_by(zipcode, occ, NIBRS_OffenseDesc) %>%
  summarise() %>%
  select(zipcode=zipcode, occ, NIBRS_OffenseDesc)

# View(rob_and_burg_perZip)

# lets check unique zipcodes
length(unique(rob_and_burg_perZip$zipcode)) # 31 zipcodes present
print(nrow(rob_and_burg_perZip)/2) # but some don't have both crimes accounted for

# select out all zipcodes with robbery -- something like this:
# select zipcode, NIBRS_OffenseDesc
# where NIBRS_OffenseDesc = robbery

robberyPerZip <- filter(rob_and_burg_perZip, NIBRS_OffenseDesc == "Robbery")
# View(robberyPerZip) # has 30 rows

# need to diff the zips and tack on ones not shown

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
print(diff_zip_Robbery)

# need to create list: 1 row, 3 columns, then rbind it
typeof(robberyPerZip)
# View(robberyPerZip)
list_to_append <- list(diff_zip_Robbery, 0, "Robbery")
names(list_to_append) <- c("zipcode", "occ", "NIBRS_OffenseDesc")
robberyPerZip_done <- rbind(robberyPerZip, list_to_append)
# View(robberyPerZip_done)

##########
#### Same thing as last step, but with burglary instead of robbery:
# select out all zipcodes with burglary
burglaryPerZip <- filter(rob_and_burg_perZip, NIBRS_OffenseDesc == "Burglary")
# View(burglaryPerZip) # has 28 rows

# This shows the three zipcodes missing in burglaryPerZip$zipcode
diff_zip_Burglary <- mysetdiff(rob_and_burg_perZip$zipcode, burglaryPerZip$zipcode)
print(diff_zip_Burglary)

newMatrix <- cbind(diff_zip_Burglary, c(0,0,0), c("Burglary", "Burglary", "Burglary"))
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

burglaryPerZip_done <- as.list(burglaryPerZip_done) # --> convert back into a list

## Now we have a list of robberies and burglaries per zipcode, including those with zero per zipcode:
# View(burglaryPerZip_done)
# View(robberyPerZip_done)

# Need to provide list of 'region' (i.e. zipcode) and 'value' (i.e. numeric value to map)
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

# Now we're ready to map.

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
View(sortedData_BurglaryPerZip_toCorrect)

zip_choropleth(sortedData_BurglaryPerZip_toCorrect, 
               zip_zoom = sortedData_BurglaryPerZip_toCorrect$region, 
               title      = "Burglary occurances by zipcode",
               legend     = "Burglaries",
               num_colors   = 1
) + coord_map()
    



###
# Compare with population

### Extract population per zipcode

## econ data frame
data(df_pop_zip) # 2012 census population data

# Select pop data for where we have crime data (31 zips)  -- not Econ data (b/c there we have 36 zips)
austinPopulation2012_perCrimeZip <- df_pop_zip[df_pop_zip$region %in% rob_and_burg_perZip$zipcode, ]
View(austinPopulation2012_perCrimeZip)

## Next, let's just make one big dataframe with:   
# Zipcode, robberies, burglaries, population
# might need dplyr: https://www.rdocumentation.org/packages/dplyr/versions/0.5.0

austinPopulation2012_perCrimeZip # selected zipcodes

# doesnt work: x <- semi_join(by=austinPopulation2012_data_selected_zips_Burglary$region, austinCrime2014_data_selected_zips, austinPopulation2012_data_selected_zips_Burglary)

nrow(sortedData_RobberyPerZip)
nrow(austinPopulation2012_perCrimeZip)

# Sort each one by zipcode, then select out values we want

austinPopulation2012_perCrimeZip_byZip <- austinPopulation2012_perCrimeZip[order(as.numeric(as.character(austinPopulation2012_perCrimeZip$region))),]
sortedData_RobberyPerZip_ByZip <- Robbery_occuranceByZip[order(as.numeric(as.character(Robbery_occuranceByZip$region))),] # order by value
sortedData_BurglaryPerZip_ByZip <- Burglary_occuranceByZip[order(as.numeric(as.character(Burglary_occuranceByZip$region))),] # order by value

View(austinPopulation2012_perCrimeZip_byZip)
View(sortedData_RobberyPerZip_ByZip)
View(sortedData_BurglaryPerZip_ByZip)

# add in crime per population

# now making the selection
austin_PopCrime_perZip <- data.frame("zipcode" = austinPopulation2012_perCrimeZip_byZip$region,
                  "population" = austinPopulation2012_perCrimeZip_byZip$value,
                  "robberies" = sortedData_RobberyPerZip_ByZip$value, 
                  "burglaries" = sortedData_BurglaryPerZip_ByZip$value) 
austin_PopCrime_perZip$burglaries <- as.numeric(as.character(austin_PopCrime_perZip$burglaries)) # as.numeric(as.character()) info: http://stackoverflow.com/questions/22790529/the-as-numeric-function-changes-the-values-in-my-dataframe
austin_PopCrime_perZip$rob_by_pop <- austin_PopCrime_perZip$robberies/austin_PopCrime_perZip$population
austin_PopCrime_perZip$burg_by_pop <- austin_PopCrime_perZip$burglaries/austin_PopCrime_perZip$population

View(austin_PopCrime_perZip)
# install.packages('formattable')
library(formattable)
austin_PopCrime_perZip$rob_by_pop_pct <- percent(austin_PopCrime_perZip$rob_by_pop)
austin_PopCrime_perZip$burg_by_pop_pct <- percent(austin_PopCrime_perZip$burg_by_pop)
View(austin_PopCrime_perZip)

###################################
## Regarding the table of data, the next step is to combine this table with table of economic indexes
###################################

# Let's plot by percentage of population

# Take the main set we just made, select out zipcode & value to map
rob_as_pct_pop <- austin_PopCrime_perZip[-c(2:4)] # remove rows 2-6
rob_as_pct_pop_toMap <- rob_as_pct_pop[-c(3:5)] # remove row 3
View(rob_as_pct_pop_toMap)
names(rob_as_pct_pop_toMap) <- c("region", "value")

zip_choropleth(rob_as_pct_pop_toMap, 
               zip_zoom = rob_as_pct_pop_toMap$region, 
               title      = "Burglary by zipcode as % of population",
               legend     = "Burglaries as % of population",
               # reference_map = T,
               num_colors = 1
) + coord_map()

#####
econData <- austin2014_EconData_selection # grabbing econ data
econData <- econData[order(as.numeric(as.character(econData$ZipCode))),] # sorted econ data
ZipDiff <- mysetdiff(econData$ZipCode, austin_PopCrime_perZip$zipcode) # find the difference between EconData & CrimeData
econData <- subset(econData,  ! ZipCode %in% ZipDiff ) # now there's 31 rows

rownames(econData) <- 1:nrow(econData) # rename the rows so we don't skip any indexes (since some were subtracted out)
View(econData)


## Now, combining EconData with CrimeData
austinData <- cbind(econData, austin_PopCrime_perZip)
View(austinData)
# remove the duplicate zipcode column
austinData <- austinData[-6]
View(austinData)


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


#######
####
######################################################
 
# Neutral color - average incidence
# Extreme color on spectrum -- 

# some info on using ggplot2 for choropleth maps:
# http://prabhasp.com/wp/how-to-make-choropleths-in-r/
# 

# here is some dplyr code for making selections-- id like to learn more about dplyr:
# new <- zipCrimeCountNIBRS %>%
#   group_by(zipcode) %>%
#   mutate(unique_types = n_distinct(NIBRS_OffenseDesc))


# zipCrimeCount %>% 
#   group_by(zipcode) %>%
#   summarise(no_rows = length(zipcode))
