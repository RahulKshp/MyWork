# NOTE: The script enlists a sequence of commands where a sample dataset is picked, explored and cleansed.

# Please understand that this exercise of Data Preparation is just an illustration. 
# The strategy and steps for preparation may vary based on the data and the datascientist's goals.


#--------------------------#BASIC DATA EXPLORATION---------------------------

# Read the sample csv dataset into a dataframe

#setwd() is used to set current directory to pick the file of interest 
# If this command is skipped read.csv() should be given full filepath
setwd("D:/MyStuff/KnowledgeStuff/DSPT/DSC-407/")

# For read.csv(), always pass 'sep' param - it is possible that the given csv file is split by anything
# other than comma.
bank_data <- read.csv(file="Bank500.csv", sep=",", header = TRUE)

# View is a nice feature to see data in tabular in RStudio
View(bank_data)

#structure of the data
str(bank_data)

#check the number of rows and columns in the dataset
dim(bank_data)

#summary to check if the fields are read properly in R
summary(bank_data)

#First eyeball the first 10 rows of the data set
head(bank_data,10)


#Eyeball the last 10 rows of the data set 
tail(bank_data,10)


#-------------------------#DATA CLEANSING------------------------------

#remove blank values from the dataset
bank_data=subset(bank_data,(bank_data$y)!="")

# now the subset contains values excluding the blanks
summary(bank_data)

# Check for NA values in the  dataset
v=is.na(bank_data) #not required
### [Hari] is.na() creates a matrix of T & F values after applying the condition on each element of bank_data
dim(v) # Examine what you get
class(v) # [Hari]Can be verified by these commans

# To check how many rows have missing data
# First sum row-wise using rowSums() - Note that rowSums() can take only numeric R object
rowsum_v = rowSums(v)
rowsum_v
# Now if each rowsum_v value is >= 1, it means that the corresponding row has missing data in any of its columns
sum(ifelse((rowsum_v >= 1),1,0)) #Prints the no.of rows

# To check how many columns  have missing data
# sapply() or lapply() works on a list - so when dataframe object is passed, it works per column,
# as each column's internal representation is a list
colsum_nodata = sapply(bank_data, function(x) sum(is.na(x)))
colsum_nodata

#remove missing values(NA) from the dataset
bank_data =na.omit(bank_data) # [HARI] na.omit applies on rows

summary(bank_data) # check if NA values have been removed

# Check if the key column X.1 has unique values
length(bank_data$X.1) == length(unique(bank_data$X.1))

# Remove duplicate values in the dataset if there are any
bank_data = bank_data[!duplicated(bank_data$X.1),]
dim(bank_data)


#---------------------------#DATA IMPUTATION---------------------------

# Finding outliers in age data

# Why 'age' is picked to find outliers? - Based on initial observation of bank_data, 'age' column's summary
# showed a very unrealistically high max value

stats_age <- boxplot.stats(bank_data$age)
outliers <- stats_age$out # we have found outlier values with this boxplot stats
outliers

# Replacing outliers with appropriate quantile values

# For values that lie outside the 1.5*IQR limits, we could cap it by replacing those values with 
# 5% or 95% value based on whether the outlier is beyond the lower or last quartile respectively

# Function to replace outlier values
fun <- function(x){
  quantiles <- quantile(x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1] # [HARI] Implicit looping
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
# Add the transformed age values as a new column for further usage
bank_data$newage<-fun(bank_data$age )
View(bank_data)

# Compare old and new stats for age
stats_age
stats_newage <- boxplot.stats(bank_data$newage)
stats_newage

#Now, we need to impute missing values in 'education' field. 
#Since we do not know the level of education of these customers, let us impute the value 'Unknown' to all of them. 
# Write code to do the same.

# Since rows with NA were already removed, to test "imputation" we use another copy of bank_data
bank_data_copy <- read.csv(file="Bank500.csv",header = TRUE)

if("Hmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("Hmisc")}
library(Hmisc)
bank_data_copy$education # Observe 'education' column before imputation
bank_data_copy$education<-impute(bank_data_copy$education, "Unknown")
bank_data_copy$education # Observe after imputation
str(bank_data_copy) # Note the description of education column - It is added with 4th level

#-------------------------------#DATA TRANSFORMATION-------------------------

#	Age is a continuous variable. 
#It would be better to organize it in intervals.
#Introduce a new column 'age_interval' using cut_interval function from gg_plot2 package.  Use n = 6.

library("ggplot2")
age_intr=  cut_interval(bank_data$newage, n = 6)
bank_data["age_interval"] = age_intr
dim(bank_data)
names(bank_data)
View(bank_data)

#Using table function, 
#inspect the distribution of customers in different age_interval. 
#Generate a barplot also
mytable1=table(bank_data$age_interval,bank_data$job)
mytable1

# Putting 'xpd=T' makes clipping to the plot region - See par() for more details
par(xpd = T, mar = par()$mar + c(0,0,0,3),las=2)
barplot(mytable1,col=heat.colors(10),cex.names = 0.7)
# 'xpd=T' allows legend to be drawn separately
legend("topright", rownames(mytable1), fill= heat.colors(10),cex = 0.7)
par(mar=c(5, 4, 4, 2) + 0.1)