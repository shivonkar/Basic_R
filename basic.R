############### Load Library ###############################################
folder_current <- "D:/Trainings/R_Basic"
setwd(folder_current)

#source("CommonUtils.R")

# Constants
g_folderImage = "./Images/"
############## BASIC R ######################################

############## Vectors ######################################
# vector declaration
my_vector <- c('Apple','Orange','Banana')
print(my_vector)
            
# Get the class
print(class(my_vector)) # "character"

# Now with numeric data            
my_vector_int <- c(2,5,1)
print(my_vector_int)

# Get the class
print(class(my_vector_int)) # "numeric"

# CW: Do with float/double and see the class
# cw: 

############## List ######################################
# Create
my_list <- list(c(1,2,3),345,my_vector, my_vector_int)
print(my_list)

my_list <- list(list_int = c(1,2,3),some_num = 345, my_vec = my_vector, my_vec_int =my_vector_int)
print(my_list)
my_list["my_vec"]

# CW: Get other component by name, index (1, 2, 3, 4) and '$' too
############## Matrices ######################################
# Create
my_matrix <- matrix( c(1, 2, 3, 4, 5, 6))
my_matrix

my_matrix <- matrix( c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
my_matrix

my_matrix <- matrix( c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
my_matrix

my_matrix <- matrix( c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE)
my_matrix

# CW: Practice with char

############## DATA FRAME /######################################
train <- data.frame(ID=c(1,2,3), Name=c('A', 'B', 'C'), Salary=c(100, 200, 150))
train

# Colnames
names(train)
colnames(train)

#conerting colname to upper case
names(train)<- toupper(names(train))
head(train,2)

# First view
dim(train)
str(train)
head(train,2)
summary(train)

# CW: Click 'F1' on 'data.frame' and see the documentation

# Read data
train <- read.csv(file =  "./data/mpg.csv", header = T, sep = ",")
names(train)<- toupper(names(train))

# CW: Click 'F1' on 'read.csv' and see the documentation
# CW: Do all above things once more and see
# CW: Interpret "summary(train)" as per knowledge gained from Statistics class

# Data Extraction.
train[1,'MPG']
train[,'MPG']
train[['MPG']]

#Add columns in data table
train['MYCOLUMN'] <- 1
head(train,2)

#Update columns in data table
train['MYCOLUMN'] <- 2
head(train,2)

#Delete columns in data table
train['MYCOLUMN'] <- NULL
head(train,2)

#Search and replace using grep and gsub
train$ORIGIN_TEMP <- train$ORIGIN
grep('American', train$ORIGIN_TEMP)

train$ORIGIN_TEMP <- gsub('American', 'American_New', train$ORIGIN_TEMP)
train$ORIGIN_TEMP
train$ORIGIN_TEMP <- NULL

#Info: use stringi for detail string changes

# save
write.csv(x = train, file = "t.csv", row.names = F)

# Constants
strResponse = 'MPG'
# CW: Try to extract using above variable instead of hardcoding

############## Apply functions #####################
catColumns = 'ORIGIN';
list_numeric_cols <- setdiff(names(train),catColumns)

# List returns
train[,list_numeric_cols] <- lapply(train[,list_numeric_cols], function(x) as.numeric(x))

# Vector return
mean_all_col <- sapply(train[,list_numeric_cols], mean)

# For each row
sum_all_row <- apply(train[,list_numeric_cols], 1, sum)

# For each col
mean_all_col_2 <- apply(train[,list_numeric_cols], 2, mean)

# equality
all.equal(mean_all_col, mean_all_col_2)

# CW. 1. Convert CYLINDERS to factor. 2. Sum 'ENGINE DISP' and HORSEPOWER
 
############## Feature Transformation #####################################
head(train)
unique(train$YEAR)

# Create new feature with year with two groups - less than 75 and greater than 75
train['YEAR_GRP'] <-  ifelse(train$YEAR <= 75, "<=75", ">75")
train$YEAR_GRP_2 <- ifelse(train$YEAR <= 75, "<=75", ">75")
head(train, 5)

# Remove temp columns
train$YEAR_GRP <- NULL; train$YEAR_GRP_2 <- NULL;

############## Handle Missing Data #######################################
# Is there any missing data anywhere
anyNA(train)

# Impute some missing data for practice
df_with_missing_data <- train
df_with_missing_data['ACCELERATE'] <-  ifelse(df_with_missing_data$ACCELERATE >=18.0 & df_with_missing_data$ACCELERATE <= 20.0, NA, df_with_missing_data$ACCELERATE)
head(df_with_missing_data, 20) # see the NA
anyNA(df_with_missing_data)

# See the count rowwise
naRow <- sapply(c(1:nrow(df_with_missing_data)), function(x) (sum(is.na(df_with_missing_data[x,]) == T)))
naRow
sum(naRow)

# use of do.call
do.call(sum, list(sapply(c(1:nrow(df_with_missing_data)), function(x) (sum(is.na(df_with_missing_data[x,]) == T)))))

# CW: get missing count of each col and use do.call to get total sum

# CW: Read at home or at leasure
# Detail Analysis of Missing data
library(mice)

# Get pattern, if any
missing_pattern <- md.pattern(df_with_missing_data)
missing_pattern

# Get complete records only
dim(df_with_missing_data)
df_with_missing_data <- train[complete.cases(df_with_missing_data),]
dim(df_with_missing_data)

# CW: Amelia -> Look at leisure

rm(df_with_missing_data)

############## Encode Categorical features ###############################
unique(train[, catColumns])

# Create one hot coding
library(caret)
fit <- dummyVars(~ORIGIN,data=train)
dt <- as.data.frame(predict(fit,train))

# delete old category column
train['ORIGIN'] <- NULL

# Combine
train <- cbind(train, dt)
head(train)

rm(dt); detach(package:caret);

############## Various Table #####################################################
# Read data afresh
train <- read.csv("./data/mpg.csv", stringsAsFactors = T)
names(train) <- toupper(names(train))
str(train)

# Base functions
table(train$ORIGIN)

# Cross tabs
table(train$ORIGIN, train$CYLINDERS)

############## Basic plots ################################################
numericColumn <- 'ACCELERATE'

plot(train[[numericColumn]])
hist(train[[numericColumn]])

# CW: See the help of above two and explore the attributes

############## ggplot #####################################################
library(ggplot2); library(ggrepel)

# Temp feature for plots
numericColumn <- 'ACCELERATE'

# Box Plot
gg2  <- ggplot(data = train, aes(x = numericColumn, y = eval(as.name(numericColumn))))
gg2 <- gg2 + geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.shape = 1, varwidth = TRUE)
gg2 <- gg2 + labs(title=paste0("Spread of " , numericColumn), size = 10) + labs(x=numericColumn, y= paste0("Value of ", numericColumn), size = 10)
plot(gg2)

# histogram
gg0  <- ggplot(data = train, aes(x = eval(as.name(numericColumn))))
gg0 <- gg0 + geom_histogram(col="black", fill="lightblue", alpha = .2) # , stat="count" breaks=seq(mi, mx, by = step), binwidth  = bins,
gg0 <- gg0 + geom_vline(aes(xintercept=mean(eval(as.name(numericColumn)), na.rm=T)), color="green", linetype="dashed", size=2)
gg0 <- gg0 + labs(title= paste0("Distribution of " , numericColumn), size = 10) + labs(x=numericColumn, y="Count", size = 10)
plot(gg0)

#  Line Plot
gg2  <- ggplot(data = train, aes(x = seq(1,nrow(train)), y = eval(as.name(numericColumn)), col=ORIGIN))
gg2  <- gg2 + geom_line()
gg2 <- gg2 + labs(title=paste0("Line of " , numericColumn), size = 10) + labs(x='Seq', y= paste0("Value of ", numericColumn), size = 10)
plot(gg2)

#CW: Scatter plot: Let us see Mpg vs numericColumn

################## Save images ######################
png('abc.png')
hist(train[[numericColumn]])
dev.off()

pdf('abc.pdf')
hist(train[[numericColumn]])
dev.off()
# Note: Resize the file and see the redraw features

################## Using Cbind, Rbind ######################
train <- data.frame(ID=c(1,2,3), Name=c('A', 'B', 'C'), Salary=c(100, 200, 150))
train

df <- data.frame(Proj = c('R', 'Python', 'Tensorflow'))
df

# Use cbind
cbind(train, df)

df <- data.frame(ID=c(4), Name=c('D'), Salary=c(300))
df

# Use rbind
rbind(train, df)
##################### Data sorting ##########################################
train <- data.frame(ID=c(1,3,2), Name=c('A', 'B', 'C'), Salary=c(100, 200, 150))
train

#sort by ID (ascending)
train_sorted <- train[order(train$ID),] 
train_sorted

#sort by 'Salary' (descending)
train_sorted <- train[order(-train$Salary),] 
train_sorted

#sort by ID (ascending) and 'Salary' (descending)
train_sorted <- train[order(train$ID,-train$Salary),] 
train_sorted

############### Regular Expressions ###################################
# CW: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

############### Handling dates ########################################
# Date Values: It is the number of days since 1970-01-01, with negative values for earlier dates.

# Convert strings to dates. The default format is yyyy-mm-dd
myd <- as.Date(c("2018-01-31", "2017-01-31"))
myd

# number of days in between
days <- myd[1] - myd[2]
days

# today's date
today <- Sys.Date()
today

# current date and time.
now <- date() 
now

# Formatting

# Symbol	Meaning	Example
# %d	day as a number (0-31)
# %a  abbreviated weekday - Mon
# %A	unabbreviated weekday	- Monday
# %m	month (00-12)
# %b  abbreviated month - Jan
# %B	unabbreviated month	- January
# %y  2-digit year
# %Y	4-digit year	

# print today's date
today <- Sys.Date()
format(today, format="%B %d %Y")

# CW; Pratcie with all above symbols

#Date Conversion
# Assume format 'mm/dd/yyyy'. The default format is yyyy-mm-dd
strDates <- c("11/01/2018", "11/01/2017")
myd <- as.Date(strDates, "%m/%d/%Y")
myd


#Date to Character

# convert dates to character data
strDates <- as.character(myd)
strDates

library(lubridate)
# https://www.rstudio.com/resources/cheatsheets/#lubridate
library(lubridate)

# Make date object in multiple way
ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")

# With timezone
arrive <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
arrive
leave <- ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")
leave

# Extract
second(arrive)

# Update
second(arrive) <- 25
arrive

# Restore
second(arrive) <- 0

# Extract
wday(arrive)
wday(arrive, label = TRUE)

# Construct and update different timezone
meeting <- ymd_hms("2011-07-01 09:00:00", tz = "Pacific/Auckland")
with_tz(meeting, "America/Chicago")

mistake <- force_tz(meeting, "America/Chicago")
with_tz(mistake, "Pacific/Auckland")

#Few operations
auckland <- interval(arrive, leave) 
auckland

# Formating
auckland <- arrive %--% leave
auckland

# Make another object to see overlap and diff
jsm <- interval(ymd(20110720, tz = "Pacific/Auckland"), ymd(20110831, tz = "Pacific/Auckland"))
jsm

int_overlaps(jsm, auckland)
setdiff(auckland, jsm)

# Few different formats
minutes(2) ## period
dminutes(2) ## duration

leap_year(2011) ## regular year
ymd(20110101) + dyears(1)
ymd(20110101) + years(1)

leap_year(2012) ## leap year
ymd(20120101) + dyears(1)
ymd(20120101) + years(1)

meetings <- meeting + weeks(0:5)
meetings %within% jsm

# Few more math operations
auckland / ddays(1)
auckland / ddays(2)
auckland / dminutes(1)

auckland %/% months(1)
auckland %% months(1)

as.period(auckland %% months(1))
as.period(auckland)

jan31 <- ymd("2013-01-31")
jan31 + months(0:11)
floor_date(jan31, "month") + months(0:11) + days(31)
jan31 %m+% months(0:11)

##################### Reading/Saving data objects ##########################################
# Create any object
myd <- as.Date(c("2018-01-31", "2017-01-31"))
myd

# Save
saveRDS(myd, 'my_object.rds')

# Read for reuse
myd2 <- readRDS('my_object.rds')  
myd2

##################### Mathematical Functions ##########################################
# Note: These are few of commonly used functions

# Numeric Functions
abs(-10)	# absolute value
sqrt(10)	# square root
ceiling(1.23)	# 2
floor(1.23)	# 1
trunc(1.23) # remove decimals similar to making int
round(1.23, digits=1) #
signif(1.23, digits=2)	# alltogether 2 digits

# Trigonometric
#cos(x), sin(x), tan(x),	 acos(x), cosh(x), acosh(x)

# Log
log(20)	#natural logarithm
log10(20)	#common logarithm
exp(2)	# e^2

seq(from = 5, to = 20, by = 2)
rep(1:3, 2)

# String Functions
substr('Hi, how are you', start=5, stop=7)

# If fixed =FALSE then pattern is a regular expression. If fixed=TRUE then pattern is a text string. 
# Returns matching indices.
grep(pattern = 'ho', c('Hi, how are you', 'I am fine, how are you', 'great') , ignore.case=T, fixed=F)
grep(pattern = 'ho', c('Hi, how are you', 'I am fine, how are you', 'great') , ignore.case=T, fixed=T)	
grep(pattern = 'ho', c('Hi, how are you', 'I am fine, how are you', 'great') , ignore.case=T, fixed=F, value = T)	

#Find pattern in x and replace with replacement text. If fixed=FALSE then pattern is a regular expression.
# If fixed = T then pattern is a text string. 
gsub(pattern = 'ho', replacement = 'Ho', x = c('Hi, how are you', 'I am fine, how are you', 'great'),
    ignore.case =F, fixed=F)

gsub(pattern = '\\s', replacement = '_', x = c('Hi, how are you', 'I am fine, how are you', 'great'),
    ignore.case =F, fixed=F)

strsplit(x = c('Hi, how are you', 'I am fine, how are you', 'great'), split = " ") 

paste('Hi, how are you', 'I am fine, how are you', 'great', sep=" :: ")	
paste("H",1:3,sep="")
paste("H",1:3,sep="-")
paste("H",1:3,sep="-", collapse = ", ")

toupper(c('Hi, how are you', 'I am fine, how are you', 'great'))
tolower(c('Hi, how are you', 'I am fine, how are you', 'great'))

###################### Statistical & Probability Functions #########################
#plot standard normal curve
x <- pretty(c(-3,3), 30) # Pretty Breakpoints
y <- dnorm(x) # normal density function (by default m=0 sd=1)
plot(x, y, type='l', xlab="Sigma", ylab="Density", yaxs="i")

# cumulative normal probability for q (area under the normal curve to the left of q)
pnorm(1.96) # 0.975
qnorm(.975) # Normal quantile: value at the p percentile of normal distribution 

x <- rnorm(n = 100, m=50, sd=10) # n random normal deviates with mean m and standard deviation sd. 

# Similarly for Uniform and poisson distributions

##################### Measure of Shape ##########################################
library(ggplot2)

# Read data afresh
train <- read.csv("./data/mpg.csv", stringsAsFactors = T)
names(train) <- toupper(names(train))

# Temp feature for plots
numericColumn <- 'MPG'

skew <- round(e1071::skewness(train[[numericColumn]], na.rm = T), 2)
kurtosis <- round(e1071::kurtosis(train[[numericColumn]], na.rm = T), 2)


title <- paste0("Skew: " ,  skew,", Kurtosis: " , kurtosis)

gg  <- ggplot(data = train, aes(x = eval(as.name(numericColumn))))
gg <- gg + geom_histogram(aes(y =..density..),  col="black", fill="lightblue", alpha = .2) # , stat="count" breaks=seq(mi, mx, by = step), binwidth  = bins,
gg <- gg + geom_density(col="red") # , fill="lightblue", alpha = .2
gg <- gg + geom_vline(aes(xintercept=mean(eval(as.name(numericColumn)), na.rm=T)), color="green", linetype="dashed", size=1)
gg <- gg + labs(title= title, size = 10) + labs(x=numericColumn, y="Count/Density", size = 10)    
gg <- gg + theme(axis.text.y=element_blank()) # axis.title.x=element_blank(),,      axis.ticks.x=element_blank())
plot(gg)

##################### Summary Functions ##########################################
x <- c(1,2,NA,3)
# all in one
summary(x)

#CW: see summary of 'train'

##################### Measures of Central Tendency ##########################################
# One by one
x <- c(1,2,NA,3)
mean(x, na.rm=F)
mean(x, na.rm=T)

sd(x, na.rm = T)	# standard deviation 
median(x, na.rm = T)
quantile(x, c(0.3, 0.84), na.rm = T)
range(x, na.rm = T)
sum(x, na.rm = T)
diff(x, lag=1, na.rm = T)
min(x, na.rm = T)
max(x, na.rm = T)

##################### User defined functions ##########################################
get_mean <- function(x)
{
  mean <- mean(x, na.rm=T)
  
  return(mean)
}

x <- c(1,2,3)
get_mean(x)

##################### Sampling ##########################################

x <- seq(1, 100)
sample(x, size = 10, replace = F)
sample(x, size = 10, replace = T)

##################### Logging the various types of messages ##########################################
suppressPackageStartupMessages(library(logging))

g_folder_log <- "./log/"

#Adding logger
addHandler(handler = writeToFile, file = paste0(g_folder_log,"test_log_", format.Date(Sys.Date(), "%b_%d_%Y"), ".log"))
basicConfig(level = "INFO") #INFO

loginfo("It is information")
logwarn("It is Warning")
logerror("It is error")

##################### Exceptional Handling in details ##########################################
x <- c(1,2,3)
tryCatch(
  get_mean(x), # Written above
  error = function(cond){
    logerror(cond)
    quit(save = "no", status = 0, runLast = F)})

################ Miscellaneous ##########################################
nNumber <- 2
ifelse(nNumber < 1, 'Less than 1', 'Greater than 1')

# CW: Nest ifelse to print 'between 1 and 2' and 'Greater than 2'

##################### How to get going in open source ##########################################
