# f you want to set any value to a missing value
mydata <- read.csv("c:/mydata.csv", header=TRUE, na.strings=".")

# If you want to set multiple values to missing values

mydata <- read.csv("c:/mydata.csv", header=TRUE, na.strings=  c("A" , "B" ))

# Reading Excel File
install.packages("readxl")

library(readxl)
read_excel("my-old-spreadsheet.xls")
read_excel("my-new-spreadsheet.xlsx")
# Specify sheet with a number or name
read_excel("my-spreadsheet.xls", sheet = "data")
read_excel("my-spreadsheet.xls", sheet = 2)
# If NAs are represented by something other than blank cells,
# set the na argument
read_excel("my-spreadsheet.xls", na = "NA")

# Load Data from R
load("mydata.RData")

# Writing comma-delimited text file (CSV)
write.csv(mydata,"C:/Users/Deepanshu/Desktop/test.csv")


# COPY DATA FROM EXCEL TO R
data = read.table(text="
X Y Z
6 5 0
6 3 NA
6 1 5
8 5 3", header=TRUE)


# Suppose you want to save an individual object in R and read it later.Saving data file in R session

saveRDS(mydata, "logistic.rds")

mydata = readRDS("logistic.rds")

save (mydata,file="E:\\logistic.rdata")
load("E:\\logistic.rdata", ex <- new.env())
ls(ex)

load("E:\\logistic.rdata", ex <- new.env())
ls(ex)

# Saving everything in R session
save.image(file="1.RData")


summary(mydata)

summary( mydata$Q1)

names(mydata)
nrow(mydata) 
ncol(mydata) 

library(dplyr)
sample_n(mydata, 5)


sample_frac(mydata, 0.1)
summary( mydata[3])


# Number of missing values

# The function below returns number of missing values in each variable of a dataset. 
colSums(is.na(mydata))
sapply(mydata, function(y) sum(is.na(y)))

# Number of missing values in a single variable
sum(is.na(mydata$Q1))

mydata$Q1[mydata$Q1==1] <- 6

mydata[mydata == 2 | mydata == 3] <- NA

# How to use IF ELSE Statement

samples = data.frame(x =c(rep(1:10)), y=letters[1:10])
(samples$t1 = ifelse(samples$x>6,2,1))
samples


# Renaming variables
install.packages("dplyr")
# Load the plyr package
library(dplyr)

# Rename Q1 variable to var1
mydata <- rename(mydata, var1 = Q1)


# keep only first two variables .
mydata1 <- mydata[1:2]

# keep first and third through sixth variables .
mydata1 <- mydata[c(1,3:6)] 
newdata <- mydata[c("v1", "v2", "v3")]


# Deleting a particular column (Fifth column)
mydata [-5] 

# Dropping Q3 variable
mydata$Q3 <- NULL

# Dropping multiple variables by their names
df = subset(mydata, select = -c(x,z) )

# Subset data (Selecting Observations)
newdata <- mydata[1:10,]
mydata<-subset(mydata, age==3)

newdata<-subset(mydata, Name=="ABC" & age==3)

# Keeping only missing records
newdata<-subset(mydata, is.na(age))

# Keeping only non-missing records
newdata<-subset(mydata, !is.na(age))

# Sort gender variable in ascending order and then SAT in descending order
mydata.sorted1 <- mydata[order(mydata$Gender, -mydata$SAT),]

# Use factor() for nominal data
mydata$Gender <- factor(mydata$Gender, levels = c(1,2), labels = c("male", "female"))

# ordered() for ordinal data
mydata$var2 <- ordered(mydata$var2, levels = c(1,2,3,4), labels = c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"))


# Number of missing values in a variable
colSums(is.na(mydata))

# Number of missing values in a row
rowSums(is.na(mydata))


# List rows of data that have missing values

mydata[!complete.cases(mydata),]

# Creating a new dataset without missing data
mydata1 <- na.omit(mydata)

# Convert a value to missing
mydata[mydata$Q1==999,"Q1"] <- NA 

# 9. Aggregate by groups

# The following code calculates mean for variable "x" by grouped variable "y".
samples = data.frame(x =c(rep(1:10)), y=round((rnorm(10))))
mydata <- aggregate(x~y, samples, mean, na.rm = TRUE)

# merges only common cases to both datasets.
mydata <- merge(mydata1, mydata2, by=c("ID"))

# removing duplicates in a whole data set
mydata1 <- unique(data)

# removing duplicates by "Y" column
mydata2 <- subset(data, !duplicated(data[,"Y"]))

# select()	Selecting columns (variables)	SELECT
# filter()	Filter (subset) rows.	WHERE
# group_by()	Group the data	GROUP BY
# summarise()	Summarise (or aggregate) data	-
#   arrange()	Sort the data	ORDER BY
# join()	Joining data frames (tables)	JOIN
# mutate()	Creating New Variables	COLUMN ALIAS

# Remove Duplicate Rows based on all the variables (Complete Row)
x1 = distinct(mydata)
# 
# Remove Duplicate Rows based oCn a variable

# The .keep_all function is used to retain all other variables in the output data frame.

x2 = distinct(mydata, Index, .keep_all= TRUE)

mydata2 = select(mydata, Index, State:Y2008)

mydata = select(mydata, -Index, -State)

mydata3 = select(mydata, starts_with("Y"))

# Reorder Variables

mydata6 = rename(mydata, Index1=Index)

mydata7 = filter(mydata, Index == "A")
mydata7 = filter(mydata6, Index %in% c("A", "C"))

mydata10 = filter(mydata6, grepl("Ar", State))

summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))
ummarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))

summarise_at(mydata, vars(Y2011, Y2012),
             funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))

set.seed(222)
mydata <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata,vars(X1,X2), function(x) var(x - mean(x)))

# Summarize all Numeric Variables 
summarise_if(mydata, is.numeric, funs(n(),mean,median))

# Summarize Factor Variable 
summarise_all(mydata["Index"], funs(nlevels(.), nmiss=sum(is.na(.))))

arrange(mydata, desc(Index), Y2011)

# Filter Data within a Categorical Variable 
# pull top 2 rows from 'A', 'C' and 'I' categories of variable Index.  
mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%
  do(head( . , 2))

# Selecting 3rd Maximum Value by Categorical Variable

mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)

# We could use min_rank() function that calculates rank in the preceding example,
t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)

mydata5 = select(mydata, State, everything())
mydata1 = mutate(mydata, change=Y2015/Y2014)

mydata11 = mutate_all(mydata, funs("new" = .* 1000))

# Suppose you need to calculate rank for variables Y2008 to Y2010.
mydata12 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))
mydata13 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))

out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>%
  select(Index, State, Y2015)

out2 = mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>%
  select(Index, Y2015, Total)


inner_join(x, y, by = )
left_join(x, y, by = )
right_join(x, y, by = )
full_join(x, y, by = )
semi_join(x, y, by = )
anti_join(x, y, by = )

df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                 w = c('a', 'b', 'c', 'd', 'e'),
                 x = c(1, 1, 0, 0, 1),
                 y=rnorm(5),
                 z=letters[1:5])

df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5),
                 d =letters[2:6])

df3 = inner_join(df1, df2, by = "ID")

inner_join(df1, df2, by = c("ID"="ID1"))

Combine Data Vertically

intersect(x, y) Rows that appear in both x and y.

union(x, y)
Rows that appear in either or both x and y.

setdiff(x, y)
Rows that appear in x but not y.

