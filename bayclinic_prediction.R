#install.packages("tidyverse")
#install.packages("caret")
#install.packages("party")
#install.packages("mice")
#install.packages("corrplot")
#install.packages("e1071")
library("e1071")
library(tidyverse)
library(caret)
library(party)
library(mice) #for missing values
library(lubridate) #work with dates and times
library(corrplot)

#   Functions 
ConvertToFactor <- function(data) {
  for (i in names(data)) {
    # Check if the column is non-numeric
    if (!is.numeric(data[[i]])) {
      # Convert the column to factor
      data[[i]] <- as.factor(data[[i]])
    }
  }
  return(data)
}
#These functions plot outliers using box,bar plot. 
#They take all features except for ID'S (patient and Applointment)
OutliersWithBarPlot <- function(data){
  for(col in names(data)[-c(1, 2)]){
    barplot(table(data[[col]]),
            main =  paste(col, "Distribution", sep = " "),
            ylab = "Count"
    )
  }
}
OutliersPlot <- function(data){
  for(col in names(data)[-c(1, 2)]){
    
    if (is.numeric(data[[col]])) {
      boxplot(data[[col]], ylab = col)
    }
    else{
      barplot(table(data[[col]]),
              main =  paste(col, "Distribution", sep = " "),
              ylab = "Count"
      )
    }

  }
}
HistogramofData<-function(data){
  for(col in names(data[-c(1,2)])){
    
    if(is.numeric(data[[col]])){
      
      hist(data[[col]], main=paste("Histogram of", col, sep = " "),
           xlab="Value", 
           ylab="Frequency")
    }
    else{
      barplot(table(data[[col]]),main=paste("Histogram of", col, sep = " "),
              xlab="Value", 
              ylab="Frequency")
      
    }
  }}
#***  Feature engineering
EncodingFeatures <- function(data){
  for(col in names(data)){
    if(!is.numeric(data[[col]]))
      {
      data[[col]] <- as.integer(factor(data[[col]]))
    }
  }
  return(data)
}# not used
EncodingFeatures <- function(data,columns){
  for(col in columns){
    if(col  %in% names(data)){
      data[[col]] <- as.integer(factor(data[[col]]))
    }
    else{
      print("Invalid column name. . .")
    }
  }
  return(data)
}
MinMaxNormalization <- function(value){
  return((value - min(value))/(max(value)-min(value)))
}



# Load and Explore Data 
data <- MedicalCentre
names(data)
sapply(data,class)
head(data,n=2)
summary(data)
#Copy of Data
df1 <- data
head(df1,1)

# *** ***     Feature Engineering     *** ***

#   ******    1. Data Cleaning
colSums(is.na(df1)) #checking null values
par(las = 2) # Set axis label orientation to 70 degrees
md.pattern(df1)
age_mean <- mean(df1$Age, na.rm = TRUE) # Calculate the mean of the "Age" column
# Replace missing values in the "Age" column with the mean
df1$Age[is.na(df1$Age)] <- age_mean
colSums(is.na(df1))


#   ******    2. Outlier Plot
#ConvertToFactor(df1)
head(df1, n=1)
OutliersWithBarPlot(df1)
#Another Approach
OutliersPlot(df1)


#   ******    3. Handle Negative age Values
negativeAge <- sum(df1$Age<0)
negativeAge
df1 <- subset(df1, Age != -1)
sum(df1$Age<0)

# **  3.1 Additional Plot
HistogramofData(df1)


#   ******    4.  Awaiting Time

head(df1,1)
tail(df1,2)

# Calculate the "AwaitingTime" column
names(df1)[names(df1) == "AppointmentDay"] <- "AppointmentDate"
names(df1)[names(df1) == "ScheduledDay"] <- "ScheduledDate"


Appointment <- as.Date(df1$AppointmentDate)
Scheuled <- as.Date(df1$ScheduledDate)

df1$AwaitingTime <- as.numeric(Appointment - Scheuled)
sum(df1$AwaitingTime<0)
df1$AwaitingTime <- abs(df1$AwaitingTime)
sum(df1$AwaitingTime<0)


#   ******    5.  Encode Categorical Columns

columns <- c("Gender","Neighbourhood","No.show")
df1<-EncodingFeatures(df1,columns)
head(df1,2)

#df1$No.show <- dplyr::recode(df1$No.show, `No` = 0L, `Yes` = 1L)


#   ******    6.  Date Componenets

df1$ScheduledDate <- as.POSIXct(df1$ScheduledDate, format =  "%Y-%m-%dT%H:%M:%SZ")
df1$AppointmentDate <- as.POSIXct(df1$AppointmentDate, format =  "%Y-%m-%dT%H:%M:%SZ")

df1["AppointmentYear"] <- as.integer(format(df1$AppointmentDate, "%Y"))
df1["AppointmentMonth"] <- as.integer(format(df1$AppointmentDate,"%m"))
df1["AppointmentDay"] <- as.integer(format(df1$AppointmentDate,"%d"))


df1["ScheduledYear"] <- as.integer(format(df1$ScheduledDate, "%Y"))
df1["ScheduledMonth"] <- as.integer(format(df1$ScheduledDate,"%m"))
df1["ScheduledDay"] <- as.integer(format(df1$ScheduledDate,"%d"))
df1["ScheduledHour"] <- as.integer(format(df1$ScheduledDate,"%H"))
df1$ScheduledDate <- as.integer(df1$ScheduledDate)
df1$AppointmentDate<- as.integer(df1$AppointmentDate)
print(colSums(is.na(df1)))

head(df1,n=3)


#   ******    7. Encoding
df1$Age <- MinMaxNormalization(df1$Age)
  
tail(df1$Age,5)


#   ******    8. Variability 
# Calculate the correlation matrix
sapply(df1,class) #checking datatypes befor correlation 

zeroVarCols <- which(apply(df1, 2, function(x) var(x) == 0))# Find columns with zero variance
print(names(df1)[zeroVarCols])
df2 <- subset(df1, select = -c(AppointmentYear))
head(df2)

cor_matrix <- cor(df2)
corrplot(cor_matrix, method = "color", main="Correlation Matrix") #Plot Correlation Matrix
print(cor_matrix)


highlyCorrelated <- findCorrelation(cor_matrix, cutoff = 0.8, verbose = TRUE)# Find highly correlated features
highlyCorrelatedCols <- names(df2)[highlyCorrelated] # Column Name of theses features
df2 <- df2[, !names(df2)%in% highlyCorrelatedCols] # Drop the highly correlated columns 

names(df2)
cor_matrix2 <- cor(df2)
corrplot(cor_matrix2, method = "color", main="Correlation Matrix") #Plot Correlation Matrix
print(cor_matrix2)


# *** ***     Model Development I 
library(dplyr)
library(e1071)
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)

# Split the data into a training set (70%) and a test set (30%)
train <- df2 %>% sample_frac(0.7)
test <- df2 %>% anti_join(train, by = 'AppointmentID')
dim(test)
# Extract the predictor variables and the target variable from the training and test sets
X_train <- train %>% select(-PatientId, -AppointmentID, -AppointmentDate, -No.show)
y_train <- train$No.show
X_test <- test %>% select(-PatientId, -AppointmentID, -AppointmentDate, -No.show)
y_test <- test$No.show

# Fit an SVM model with a radial kernel to the training data
svmModel <- svm(x = X_train, y = y_train, type = 'C-classification', kernel = 'radial')

# Compute the predicted labels for the test data
y_pred <- predict(svmModel, X_test)

# Compute the confusion matrix
cm <- table(y_test, y_pred)

# Print the confusion matrix
print(cm)

# Plot the decision boundary
ggplot(data = train, aes(x = Age, y = AwaitingTime, color = No.show)) +
  geom_point() +
  scale_color_manual(values = c('#FF0000', '#0000FF')) +
  geom_smooth(method = 'svm', formula = y ~ x, data = train, aes(color = NULL), alpha = 0.2) +
  theme_bw()




















train <- df2 %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(df2, train, by = 'PatientId')
dim(train)
dim(test)
X_test<-subset(test,select=-No.show)
dim(X_test)
y_test<-test$No.show
dim(y_test)
class(y_train)
library(e1071)

dat = data.frame(X_train, y_train)


svmClassifire <- svm(formula = y_train ~ ., 
                     data = dat,
                     type = 'C-classification',
                     kernel = 'radial')

cm <- table(test_set$No.show, y_pred)





















# Classification template

# Classification template

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(42)
split = sample.split(df2$No.show, SplitRatio = 0.70)
training_set = subset(df2, split == TRUE)
test_set = subset(df2, split == FALSE)
dim(training_set)
dim(test_set)


# Fitting classifier to the Training set
# Create your classifier here

# Predicting the Test set results
y_pred = predict(svmClassifire, newdata = test_set)
#svm_pred <- predict(svm_model, testData)

# Making the Confusion Matrix
cm = table(test_set$No.show, y_pred)
table(y_pred, test_set$No.show)
cm$overall[1]
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Classifier (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))









































































*** ***
#make this example reproducible
set.seed(42)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df2), replace=TRUE, prob=c(0.7,0.3))
sample
train  <- df2[sample, -which(names(df2) == "no.show") ]
test   <- df2[!sample, ]
train
#####################################################
#use 70% of dataset as training set and 30% as test set 
train <- df2 %>% dplyr::sample_frac(0.20)
test  <- dplyr::anti_join(df2, train, by = 'PatientId')
dim(train)
dim(test)
X_train<-subset(train,select=-No.show)
dim(X_train)
y_train<-train$No.show
y_train

X_test<-subset(test,select=-No.show)
dim(X_test)
y_test<-test$No.show
dim(y_test)
class(y_train)
library(e1071)

dat = data.frame(X_train, y_train)
svmfit = svm(y_train ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
system.time(print(svmfit))
plot(svmfit, dat)
set.seed(10111)
x <- subset(df2,select=-No.show)
y <- df2[,"No.show"]
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
