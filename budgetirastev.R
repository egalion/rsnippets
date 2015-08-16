rm(list=ls())
ls()
setwd("/home/alex29/Плот/mydata/")
library(tidyr)

# check.names prevents R from adding X in front of
# numerical values in front of column names.
# In principle it should be avoided, but we will convert
# data to long format, so these will become values of
# the year variable.

deficit_wide <- read.table("tec00127.tsv", header = TRUE, check.names = FALSE)
head(deficit_wide)
colnames(deficit_wide)[1] <- "country"
str(deficit_wide)

# Now we have to convert the country code EL to GR for Greece.
# As has been subsequently established, this is needed
# in order to merge the plots with gdp growth:

deficit_wide$country <- gsub("EL", "GR", deficit_wide$country)
deficit_wide[grepl("GR", deficit_wide$country), ]



# Convert wide to long format with the gather command from tidyr.
# The first argument is the name of the data object.
# Specify the name of the new variables - "year" and "budget condition"
# Specify the columns which will form the new values - from 2 to 13.

deficit_long <- gather(deficit_wide, year, budget_condition, 2:13)
head(deficit_long)
tail(deficit_long)
str(deficit_long)

head(deficit_long[which(grepl("GR", deficit_long$country)), ])

library(sqldf)
deficitpercent <- sqldf("select * from deficit_long where country LIKE '%PC_GDP%'")
head(deficitpercent)
tail(deficitpercent)
str(deficitpercent)

# alternatively we could use base R with the grepl function.
#

deficit_p <- deficit_long[grepl("PC_GDP", deficit_long$country), ]
str(deficit_p)
head(deficit_p)
tail(deficit_p)

#
# we will not use it further in the script
# even though it works
# If we want to select everything that doesn't contain the string
# We will use ! in front of grepl:
# deficit_long[!grepl("PC_GDP", deficit_long$country), ]

deficitpercent$budget_condition <- as.numeric(deficitpercent$budget_condition)
deficitpercent$year <- as.numeric(as.character(deficitpercent$year))

# conversion to date (not needed in this case)
# library(zoo)
# deficitpercent$year <- strptime(deficitpercent$year, format = "%Y")
# deficitpercent$year <-as.POSIXct(deficitpercent$year, format = "%Y")

deficitaftercrisis <- subset(deficitpercent, deficitpercent$year > 2007)
deficitaftercrisis_complete <- deficitaftercrisis[complete.cases(deficitaftercrisis$budget_condition), ]  
str(deficitaftercrisis_complete)

deficitaftercrisis_complete$country <- as.factor(as.character(deficitaftercrisis_complete$country))

head(deficitaftercrisis_complete[which(grepl("GR", deficitaftercrisis_complete$country)), ])


# lapply(split(deficitaftercrisis_complete$budget_condition, deficitaftercrisis_complete$country), mean)
avg_deficits <- sapply(split(deficitaftercrisis_complete$budget_condition, deficitaftercrisis_complete$country), mean)
head(avg_deficits)
str(avg_deficits)

# convert the output of sapply to data frame
# we can't do it directly
# so we get the names from the object and a vector with the values
# and combine them into a data frame

names(avg_deficits)
as.vector(avg_deficits)
avg_def <- data.frame(names(avg_deficits), as.vector(avg_deficits))

# We add names to the columns

names(avg_def) <- c("country", "average_deficit")

# And now it works

str(avg_def)
head(avg_def)

# Remove Norway and aggregate data EU27, EA19, etc., except EU28.

avg_def_countries <- sqldf("select * from avg_def where country NOT LIKE '%EA%' AND country NOT LIKE '%EU27' AND country NOT LIKE '%NO%'")
avg_def_countries

avg_def_ordered <- avg_def_countries[order(avg_def_countries$average_deficit), ]
head(avg_def_ordered)

# Now remove the "PC_GDP," string
avg_def_ordered$country <- gsub("PC_GDP,", "", avg_def_ordered$country)

library(ggplot2)
ggplot(avg_def_ordered, aes(reorder(country, average_deficit), average_deficit)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ylab("Дефицит/излишък в % от БВП") + xlab("") +
  ggtitle("Средни стойности на бюджета за 2008 - 2014 г.")

# Alternatively, one could reorder the factor levels.
# In this case we will reorder countries by their budget deficits.

avg_def_ordered$country <- factor(avg_def_ordered$country, levels = avg_def_ordered$country[order(avg_def_ordered$average_deficit)])
levels(avg_def_ordered$country)

# Now we can plot again with ggplot
# but with less typing:

ggplot(avg_def_ordered, aes(x = country, y = average_deficit)) + 
  geom_bar(stat = "identity", colour = "white", fill = "#56B4E9") + coord_flip() + 
  ylab("Дефицит/излишък в % от БВП") + xlab("") +
  ggtitle("Средни годишни стойности на \n бюджета за 2008 - 2014 г.")

# Read GDP growth per capita data and overlay it to the current graph

gdpgrowth <- read.csv("ny.gdp.pcap.kd.zg_Indicator_en_csv_v2.csv", 
                      check.names = FALSE, header = TRUE, skip = 4)
head(gdpgrowth)
str(gdpgrowth)

# remove unnecessary columns

for (i in c(60, 5, 4, 3)) {gdpgrowth[i] <- NULL}

# Change names of the first two columns

colnames(gdpgrowth)[1:2] <- c("countryname", "countrycode")

# Make wide data long.

gdpgrowth_long <- gather(gdpgrowth, year, gdpgrowth, 3:56)
str(gdpgrowth_long)

# Read GDP metadata because it contains geographical
# and income classification. Read only the first 4 columns.
# We check how it looks like with the command

preview_gdpmeta <- readLines("Metadata_Country_ny.gdp.pcap.kd.zg_Indicator_en_csv_v2.csv")
head(preview_gdpmeta)

# Then we see that column 5 is not needed and even might
# be problematic to load, because it contains commas.

# colClasses allows us to skip unneeded columns. NULL means
# to remove them. NA means to use the default approach to try
# and figure out what the column is automatically. Note that
# NA is not surrounded by quotation marks, while "NULL" is.

gdpmeta <- read.csv("Metadata_Country_ny.gdp.pcap.kd.zg_Indicator_en_csv_v2.csv",
                    colClasses = c(NA, NA, NA, NA, "NULL"))

str(gdpmeta)

# Change the names of the columns to match those of the
# gdp growth data. We want to merge the data objects by country.

colnames(gdpmeta)[1:2] <- c("countryname", "countrycode")

# Now merge. As the first two columns have the same names
# we can do just this: by = "countryname". But we want to
# merge by two variables, so we use:

growth_merged <- merge(gdpgrowth_long, gdpmeta, by = c("countryname", "countrycode"))

# If column names of the merged columns were different in the 
# two datasets, we could still merge (by one column) in the
# example with:
# merge(gdpgrowth_long, gdpmeta, by.x = "country", by.y = "state")
# assuming that the column name is "country" in "gdpgrowth_long"
# and "state" in "gdpmeta",

str(growth_merged)
growth_merged[7] <- NULL # There is an unnecessary column after merge.

# Subset the data to 2008 onwards.
# But first we need to convert the year from factor to numeric.
# If we do it directly, it will convert to the number of the
# factor. That is why we first convert to character. 

growth_merged$year <- as.numeric(as.character(growth_merged$year))
growth <- subset(growth_merged, year >= 2008)
str(growth)

# Remove unnecessary factor levels from subsetted data.

growth$countryname <- droplevels(growth$countryname)

# if there are more columns do:

for (i in (1:ncol(growth))) {
  growth[i] <- droplevels(growth[i])
}

head(growth)

# convert 3 letter country codes to 2 letter
# install.packages("countrycode")
library(countrycode)
?countrycode
# vector of values to be converted
codes.of.origin <- countrycode::countrycode_data$wb
# apply to the dataset by converting wb codes to 2 letter ISO
codes.of.destination <- countrycode(codes.of.origin, "wb", "iso2c")
# make a dataframe of codes to be merged to main data
df.codes <- data.frame(codes.of.origin, codes.of.destination)
names(df.codes) <- c("countrycode", "code2")
str(df.codes)
head(df.codes)
tail(df.codes)

# As was subsequently established in the analysis, some
# additional adjustment need to be made at this point.
# In the codes data frame GBR is GB 
# (should be UK to match the budget data set).
# ROM is RO in the code dataset, but in the WB dataset
# there is no ROM - it's ROU.  We found that out by:
# growth[which(grepl ("Romania", growth$countryname)), ]
# In the code dataset GRC is GR, # however it is EL in 
# the budget dataset. We fixed that above by changing EL 
# in the budget dataset to GR.

# Make changes to df.codes to match WB dataset:

df.codes$countrycode <- gsub("ROM", "ROU", df.codes$countrycode)

# Change df.codes to match EU budget dataset:

df.codes$code2 <- gsub("GB", "UK", df.codes$code2)

# Then merge 

df.growth <- merge(growth, df.codes, by = "countrycode")
str(df.growth)

df.growth$code2 <- as.factor(df.growth$code2)

# Subset by EU countries, but first remove EU28
EUcountries <- avg_def_ordered$country[!grepl("EU28", avg_def_ordered$country)]
EUcountries <- droplevels(EUcountries)
str(EUcountries)
df.growthEU <- df.growth[(df.growth$code2 %in% EUcountries), ]

# Drop unused levels
for (i in (1:ncol(df.growthEU))) {
  df.growthEU[i] <- droplevels(df.growthEU[i])
}

str(df.growthEU)
summary(df.growthEU$gdpgrowth)

# We can see that there is NA data
# To find out where we use:

df.growthEU$countryname[is.na(df.growthEU$gdpgrowth)]

df.growthEU[which(df.growthEU$code2 == "LU"), ]
df.growthEU[which(df.growthEU$code2 == "MT"), ]

# Luxembourg and Malta have no data for 2014

# Calculate mean growth for each country
# We will also have to remove missing data:

country_average_growth <- sapply(split(df.growthEU$gdpgrowth, df.growthEU$code2), mean, na.rm = TRUE)
country_average_growth

# Create a data frame from the output of sapply
df.avrgrowth <- data.frame(names(country_average_growth), country_average_growth)
colnames(df.avrgrowth)[1:2] <- c("country", "average_growth")
str(df.avrgrowth)
df.avrgrowth

levels(df.avrgrowth$country)

# We want to order the factors as they are in the 
# ordered budget database. But first we need to add
# EU average - EU28. We'll have to convert to 
# character first.

# One way to do is this:
# df.avrgrowth[nrow(df.avrgrowth)+1, ] <- c("EU28", mean(df.avrgrowth$average_growth))
# But it also has problems with the factor levels.

df.avrgrowth$country <- as.character(df.avrgrowth$country)
df.avrgrowth <- rbind(df.avrgrowth, c("EU28", mean(df.avrgrowth$average_growth)))
df.avrgrowth$country <- as.factor(df.avrgrowth$country)
df.avrgrowth$average_growth <- as.numeric(df.avrgrowth$average_growth)

str(df.avrgrowth)
df.avrgrowth

# Reorder the dataframe by country, using the order of the 
# ordered budget dataframe.

# First save the order to a vector

budgetorder <- avg_def_ordered$country[order(avg_def_ordered$average_deficit)]
budgetorder

# Then use the match function to reorder one vector to another.
# Note that for this to work, lengths and values of the two
# vectors must be the same. Only the order could be different.

df.avrgrowth.reordered <- df.avrgrowth[match(budgetorder, df.avrgrowth$country), ]
df.avrgrowth.reordered
levels(df.avrgrowth.reordered$country)

# Alternatively use
# sort(budgetorder)[df.avrgrowth$country]
# But for some reason it misplaces the order of "LT"

# And now reorder the factor levels

df.avrgrowth.reordered$country <- factor(df.avrgrowth.reordered$country, levels = budgetorder)
levels(df.avrgrowth.reordered$country)
df.avrgrowth.reordered

# However we don't need to reorder - neither the data, nor the
# factor levels for the second dataset (in this case the growth
# dataset). ggplot uses the order from the first dataset plotted.
# In this case it is the budget dataset.

# Now plot, using both EU ordered budget dataset
# and the reordered (by budget) growth dataset.

ggplot() + 
  geom_bar(data = avg_def_ordered, aes(x = country, y = average_deficit), 
           stat = "identity", colour = "white", fill = "#56B4E9") +
  geom_bar(data = df.avrgrowth.reordered, aes(x = country, y = average_growth), 
             stat = "identity", position = "identity", colour = "white", fill = "maroon", width = .2) +
  coord_flip() +
  ggtitle("Бюджет и растеж - средногодишни данни \n за 2008 - 2014 г.")

# Alternatively we can use points for economic growth:
ggplot() + 
  geom_bar(data = avg_def_ordered, 
           aes(x = country, y = average_deficit, colour = "Budget"), 
           stat = "identity", colour = "white", fill = "#56B4E9") +
  geom_point(data = df.avrgrowth.reordered, 
             aes(x = country, y = average_growth, colour = "Growth"), 
           stat = "identity", colour = "maroon", size = 3) +
  coord_flip() +
  ggtitle("Бюджет и растеж - средногодишни данни \n за 2008 - 2014 г.") +
  xlab("") + ylab("")

# Can't get legend to work in this way. Merge the two datasets.

df.budget.growth <- merge(avg_def_ordered, df.avrgrowth.reordered, by = "country")
str(df.budget.growth)
levels(df.budget.growth$country)

ggplot(df.budget.growth) +
  geom_bar(aes(x = country, y = average_deficit, colour = "Бюджет, % от БВП"), 
           stat = "identity", fill = "#56B4E9", width = .7) +
  geom_point(aes(x = country, y = average_growth, shape = "Растеж, БВП на човек, %"), 
             stat = "identity", colour = "darkblue", size = 3) +
  coord_flip() + xlab("") + ylab("") +
  ggtitle("Държавен бюджет и икономически растеж, \n средногодишни данни за 2008 - 2014 г.") +
  scale_color_manual("", values = c("Бюджет, % от БВП" = "#56B4E9")) +
  scale_shape_manual("", values = c("Растеж, БВП на човек, %" = 19)) +
  theme(legend.position = "right") +
  scale_y_continuous(breaks = round(seq(-12, 5, by = 2)), limits = c(-12.5, 4.5))
 