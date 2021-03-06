## Setting working Directory
setwd("C:\\PERSONAL\\PGDDA\\CaseStudy\\HRAnalytics")

##============ install packages ===========
install.packages("ggplot2")
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
install.packages("ROCR")


library(ggplot2)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ROCR)

##============ import all the data to R ===========

employee <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
manager <-  read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
general <-  read.csv("general_data.csv",stringsAsFactors = FALSE)
Intime <- read.csv("in_time.csv",stringsAsFactors = FALSE)
Outtime <- read.csv("out_time.csv",stringsAsFactors = FALSE)

HR_Company <- merge(employee,manager,by = "EmployeeID")
HR_Company <- merge(HR_Company,general,by = "EmployeeID")


##============ Data understanding, preparation and EDA =======

## Convert ALL COlumns of the datframe Intime and Outtime to date and time frame 
## Can delete the Data part in each column

for(j in 3:ncol(Intime)){
  Intime[,j] <-  as.POSIXct(Intime[,j],format="%Y-%m-%d %H:%M:%S") 
  }
colnames(Intime)[1] <- "EmployeeID"


for(j in 3:ncol(Outtime)){
  Outtime[,j] <-  as.POSIXct(Outtime[,j],format="%Y-%m-%d %H:%M:%S") 
}
colnames(Outtime)[1] <- "EmployeeID"

## Find number of holidays in a year, if number of NAs for a specfic day is more than 4400 then it is a holiday
sum(is.na(Intime[,2]))

for(i in 1:ncol(Intime)){
  if(sum(is.na(Intime[,i])) >= 4400){
    print(colnames(Intime)[i])
  }else{
    next
  }
}

## so on the folowing days the company had holiday
## 2015.01.01, 2015.01.14, 2015.01.26, 2015.03.05, 2015.05.01, 2015.07.17, 2015.09.17,
## 2015.10.02, 2015.11.09, 2015.11.10, 2015.11.11, 2015.12.25

## Find average time spent by a employee each day in office outtime-intime

employee_timer <- data.frame(employee$EmployeeID)

colnames(employee_timer)[1] <- "EmployeeID"
timer1 <- c()
timer2 <- c()
ncol(Intime)
Intime1 <- data.frame()
Intime1 <- Intime[,1:2]

for(i in 3:ncol(Intime)){
   timer1 <- Intime[,i]
   timer2 <- Outtime[,i]
   
   Intime1[,i] <- difftime(timer2,timer1,units = "sec")
   timer1 <- c()
  timer2 <- c()
}

## In the table intimer1 convert NA to zero

Intime1[is.na(Intime1)] <- 0
Intime1 <- Intime1[,-c(1:2)]

## Adding a column in employee_timer that is average amount in Hours spent by employee over 261 

for(i in 1:nrow(Intime1)){
  employee_timer[i,2] <- round(mean(as.numeric(Intime1[i,]))/3600,digits = 2)
}
colnames(employee_timer)[2] <- "Average_Hours"

## No of leaves taken by each emloyee over 262 days

for(i in 1:nrow(Intime1)){
employee_timer[i,3] <- sum(Intime1[i,]==0)
}
colnames(employee_timer)[3] <- "no_of_leavestaken"


## Employee overstaying or not 

for(i in 1:nrow(employee_timer)){
  if(employee_timer[i,2] >= 8){
    employee_timer[i,4] <- "Y"
  }else{
    employee_timer[i,4] <- "N"
  }
}
colnames(employee_timer)[4] <- "emp_overstaying"

## Merge employee_timer with HR_Company

HR_Company <- merge(HR_Company,employee_timer,by = "EmployeeID")

View(HR_Company)

## Levels attribute of a variable
## EnvironmentSatisfaction

levels(HR_Company$EnvironmentSatisfaction)
levels(HR_Company$EnvironmentSatisfaction) <- c("Low","Medium","High","VeryHigh")

## JobSatisfaction

levels(HR_Company$JobSatisfaction)
levels(HR_Company$JobSatisfaction) <- c("Low","Medium","High","VeryHigh")

## WorkLifeBalance

levels(HR_Company$WorkLifeBalance)
levels(HR_Company$WorkLifeBalance) <- c("Bad","Good","Better","Best")

## JobInvolvement

levels(HR_Company$JobInvolvement)
levels(HR_Company$JobInvolvement) <- c("Low","Medium","High","VeryHigh")

## PerformanceRating

levels(HR_Company$PerformanceRating)
#contains only 3 and 4
levels(HR_Company$PerformanceRating) <- c("Excellent","Outstanding")

## Education

levels(HR_Company$Education)
levels(HR_Company$Education) <- c("Below College","College","Bachelor","Master","Doctor")


### Data preparation of HR_Analytics

## Check for duplicate data

sum(duplicated(HR_Company))

### no duplicate records found  

## No of NAs record

sum(is.na(HR_Company[]))

#111 records

for(i in 1:ncol(HR_Company)){
  if(sum(is.na(HR_Company[,i]) > 0)){
    print(i)
  }
}
colnames(HR_Company)

## So the above loop shows column "EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance" ,"NumCompaniesWorked" and "TotalWorkingYears" CONTAINS NA value
## Lets check which % of data is missing

sum(is.na(HR_Company$EnvironmentSatisfaction))/4410

sum(is.na(HR_Company$JobSatisfaction))/4410

sum(is.na(HR_Company$WorkLifeBalance))/4410

sum(is.na(HR_Company$NumCompaniesWorked))/4410

sum(is.na(HR_Company$TotalWorkingYears))/4410

## Number of NAs for the above 5 columns is very insignificant. hence i will delet these rows

HR_Company <- HR_Company[-(which(is.na(HR_Company$JobSatisfaction))),]
HR_Company <- HR_Company[-(which(is.na(HR_Company$NumCompaniesWorked))),]
HR_Company <- HR_Company[-(which(is.na(HR_Company$TotalWorkingYears))),]
HR_Company <- HR_Company[-(which(is.na(HR_Company$EnvironmentSatisfaction))),]
HR_Company <- HR_Company[-(which(is.na(HR_Company$WorkLifeBalance))),]

## Check for the outliers
str(HR_Company)

Data1 <- data.frame(sapply(HR_Company[,c(7,11,19,22,30)],function(x) quantile(x,seq(0,1,.01),na.rm = T)))
Data1
## no outliers present in the numeric data

## Plot histogram to check the outliers if any
hist(HR_Company$Age)
hist(HR_Company$DistanceFromHome)
hist(HR_Company$MonthlyIncome)
hist(HR_Company$PercentSalaryHike)
hist(HR_Company$Average_Hours)


## Creating new Dataset for Visualization
HR_vis<-HR_Company


## Convert categorical column to factors

HR_Company$EnvironmentSatisfaction  <- as.factor(HR_Company$EnvironmentSatisfaction)
HR_Company$JobSatisfaction          <- as.factor(HR_Company$JobSatisfaction)
HR_Company$WorkLifeBalance          <- as.factor(HR_Company$WorkLifeBalance)
HR_Company$JobInvolvement           <- as.factor(HR_Company$JobInvolvement)
HR_Company$PerformanceRating        <- as.factor(HR_Company$PerformanceRating)
HR_Company$Attrition                <-  as.factor(HR_Company$Attrition)
HR_Company$BusinessTravel           <- as.factor(HR_Company$BusinessTravel)
HR_Company$Department               <-  as.factor(HR_Company$Department)
HR_Company$Education                <- as.factor(HR_Company$Education)
HR_Company$EducationField           <-  as.factor(HR_Company$EducationField)
HR_Company$Gender                   <-  as.factor(HR_Company$Gender)
HR_Company$JobLevel                 <-  as.factor(HR_Company$JobLevel)
HR_Company$JobRole                  <- as.factor(HR_Company$JobRole)
HR_Company$MaritalStatus            <- as.factor(HR_Company$MaritalStatus)
HR_Company$Over18                   <- as.factor(HR_Company$Over18 )
HR_Company$StockOptionLevel         <-  as.factor(HR_Company$StockOptionLevel)
HR_Company$emp_overstaying          <-  as.factor(HR_Company$emp_overstaying)


## Lets check summary of the data frame
summary(HR_Company)
str(HR_Company)

## Summary shows column over_18 contains one value Y and employee_count also contain one value 1 ,standard_hours also is 8 for all the rows  hence these 
## Three columns can be deleted

HR_Company <- HR_Company[,-c(14,21,23)]
HR_Company1 <- HR_Company

## THERE are 3 binary variables Attrition,Gender,and $emp_overstaying
## Convert to binary variable

levels(HR_Company1$Attrition)
levels(HR_Company1$Attrition) <- c(0,1)

## NO as 0 and Yes as 1

levels(HR_Company1$Gender)
levels(HR_Company1$Gender) <- c(1,0)

## Female as 1 and Male as 0

levels(HR_Company1$emp_overstaying)
levels(HR_Company1$emp_overstaying) <- c(0,1)

## NO as 0 and Yes as 1
## Copy all categorical column to a common data frame

employeedata <- HR_Company1[,c(1,2:6,9:10,12:13,15:17,21)]

## dummy variable creation (using model.matrix)

dummy <- data.frame(sapply(employeedata,function(x) data.frame(model.matrix(~x-1,data = employeedata))))

## delete the first column emp_id x from dummy
dummy <- dummy[,-1]


## SCALING OF CONTINOUS VARIABLE data in HR_Company1

HR_Company1[,c(7,11,18:20,22:28)] <- sapply(HR_Company1[,c(7,11,18:20,22:28)], function(x) scale(x))

## ADD THE dummy table to HR_Company1 

HR_Company1 <- cbind(HR_Company1[,-c(1:6,9:10,12:13,15:17)],dummy)


## Final dataframe on which to build the model
str(HR_Company1)
View(HR_Company1)

HR_vis <- HR_Company
##============ Visualization =======

#Creating Buckets for Average officeHours, Age, Income variables
#For Age
HR_vis$Age_Bucket[HR_vis$Age<='20']<-'Very Young'
HR_vis$Age_Bucket[HR_vis$Age>'20' & HR_vis$Age<='30']<-'Young'
HR_vis$Age_Bucket[HR_vis$Age>'30' & HR_vis$Age<='55']<-'Adult'
HR_vis$Age_Bucket[HR_vis$Age>='55']<-'Old'

#For AVerage Office Hours
HR_vis$Average_Bucket[HR_vis$Average<='6']<-'Less'
HR_vis$Average_Bucket[HR_vis$Average>'6' & HR_vis$Average<='9']<-'Preffered'
HR_vis$Average_Bucket[HR_vis$Average>'9']<-'OverTime'

#For Income
HR_vis$Income_Bucket[HR_vis$MonthlyIncome<='20000']<-'Less'
HR_vis$Income_Bucket[HR_vis$MonthlyIncome>'20000'& HR_vis$MonthlyIncome<='40000']<-'Moderate'
HR_vis$Income_Bucket[HR_vis$MonthlyIncome>'40000'& HR_vis$MonthlyIncome<='90000']<-'High'
HR_vis$Income_Bucket[HR_vis$MonthlyIncome>'90000']<-'Very High'


# Plot showing Average Office Hours against Attrition
ggplot(HR_vis,aes(x=HR_vis$Average_Bucket, fill=HR_vis$Average_Bucket))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

# Plot showing Age Buckets against Attrition
ggplot(HR_vis,aes(x=HR_vis$Age_Bucket, fill=HR_vis$Age_Bucket))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

# Plot showing Income against Attrition
ggplot(HR_vis,aes(x=HR_vis$Income_Bucket, fill=HR_vis$Income_Bucket))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

# Plot showing Marital Status against Attrition
ggplot(HR_vis,aes(x=HR_vis$MaritalStatus, fill=HR_vis$MaritalStatus))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

# Plot showing Gender against Attrition
ggplot(HR_vis,aes(x=HR_vis$Gender, fill=HR_vis$Gender))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

# Plot showing Business Travel against Attrition
ggplot(HR_vis,aes(x=HR_vis$BusinessTravel, fill=HR_vis$BusinessTravel))+geom_bar(position="dodge")+facet_grid(~HR_vis$Attrition)

ggplot(HR_vis,aes(x=Attrition, fill=EducationField))+geom_bar(position="fill")
ggplot(HR_vis,aes(x=Attrition, fill=JobRole))+geom_bar(position="dodge")

ggplot(HR_vis,aes(x=factor(PerformanceRating), fill=PerformanceRating))+geom_bar(position="dodge")+ facet_grid(~Attrition)
ggplot(HR_vis,aes(x=factor(WorkLifeBalance)))+geom_bar(position="dodge")+ facet_grid(~Attrition)



##============ splitting the data between train and test ======
set.seed(100)

indices = sample.split(HR_Company1, SplitRatio = 0.7)

train = HR_Company1[indices,]

test = HR_Company1[!(indices),]


##============ Logistic Regression =========

## Initial model
model_1 <- glm(Attrition ~., data = train,family = "binomial")
summary(model_1)

## Stepwise selection using stepAIC
#library(MASS)
model_2 <- stepAIC(model_1, direction="both")

summary(model_2)

## AS PER STEPAIC remove the below variables 
# + YearsAtCompany + EducationField.xHuman.Resources + MonthlyIncome  + JobLevel.x2
# + EnvironmentSatisfaction.x2 + PercentSalaryHike + EnvironmentSatisfaction.x4
# + JobRole.xManufacturing.Director + EducationField.xOther + StockOptionLevel.x3
# + no_of_leavestaken + PerformanceRating.x3  + PerformanceRating.x4  + JobLevel.x3
# + JobLevel.x4 + JobRole.xSales.Representative + EducationField.xMarketing + Education.x5
# + EducationField.xTechnical.Degree + Gender + JobInvolvement.x4  + StockOptionLevel.x1
# + JobInvolvement.x2   + JobRole.xHuman.Resources   + Department.xResearch...Development
# + Department.xSales + JobLevel.x1 + StockOptionLevel.x2   + Education.x1  + Education.x4
# + DistanceFromHome + JobRole.xHealthcare.Representative + JobRole.xManager + WorkLifeBalance.x2
# + WorkLifeBalance.x4 + EnvironmentSatisfaction.x3 + Education.x3 + JobInvolvement.x1 + StockOptionLevel

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + emp_overstaying + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + Education.x2 + EducationField.xLife.Sciences + 
                 EducationField.xMedical + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 StockOptionLevel.x0 + JobLevel.x5, family = "binomial", data = train)	

summary(model_3)
# library(car)
vif(model_3)

## Removing emp_overstaying1 based on high vif and low significance

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + Education.x2 + EducationField.xLife.Sciences + 
                 EducationField.xMedical + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 StockOptionLevel.x0 + JobLevel.x5, family = "binomial", data = train)	

summary(model_4)
vif(model_4)

## Cannot remove any variable based on vif as almost all variables have low vif and one with hgher vif is highly significant

## Remvoing EducationField.xMedical due to lower significance

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + Education.x2 + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 StockOptionLevel.x0 + JobLevel.x5, family = "binomial", data = train)	

summary(model_5)
vif(model_5)


## Remove StockOptionLevel.x0 due to lower significance

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + Education.x2 + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 JobLevel.x5, family = "binomial", data = train)	

summary(model_6)
vif(model_6)

## Remove Education.x2 due to lower significance

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 JobLevel.x5, family = "binomial", data = train)	

summary(model_7)
vif(model_7)

## Remove EducationField.xLife.Sciences due to lower significance

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources +  
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 JobLevel.x5, family = "binomial", data = train)	

summary(model_8)
vif(model_8)

## Remove JobLevel.x5 due to lower significance

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources +  
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried  
                 , family = "binomial", data = train)	

summary(model_9)
vif(model_9)

## Remove JobInvolvement.x3  due to lower significance

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_Hours + EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources +  
                 JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried  
               , family = "binomial", data = train)	

summary(model_10)
vif(model_10)


## Remove JobRole.xResearch.Scientist  due to lower significance

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                  BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  
                  JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_11)
vif(model_11)

## Remove JobRole.xLaboratory.Technician  due to lower significance

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                  BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_12)
vif(model_12)

## Remove JobRole.xSales.Executive  due to lower significance

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                  BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  JobRole.xResearch.Director + 
                   MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_13)
vif(model_13)

## Remove JobRole.xResearch.Director  due to lower significance

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                  BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_14)
vif(model_14)


## Remove BusinessTravel.xNon.Travel  due to lower significance

model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_15)
vif(model_15)

## Remove WorkLifeBalance.x3  due to lower significance

model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_Hours + EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x1 + BusinessTravel.xTravel_Frequently + 
                  Department.xHuman.Resources +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried  
                , family = "binomial", data = train)	

summary(model_16)
vif(model_16)

## field considered in final mode 
# Age, NumCompaniesWorked, TotalWorkingYears , TrainingTimesLastYear 
# YearsSinceLastPromotion, YearsWithCurrManager , Average_Hours 
# EnvironmentSatisfaction.x1 , JobSatisfaction.x1, JobSatisfaction.x2 
# JobSatisfaction.x3, WorkLifeBalance.x1 , BusinessTravel.xTravel_Frequently 
# Department.xHuman.Resources , MaritalStatus.xDivorced , MaritalStatus.xMarried


## final Model for Evaluation on test data
final_model <- model_16

##============ Model Evaluation - Test Data ========

## predicted probabilities of Attrition  for test data

test_pred <- predict(final_model,type = "response",newdata = test[,-2])

#lets check summary 
summary(test_pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.001022 0.038910 0.097430 0.158800 0.211500 0.914900 

test$probability <- test_pred
View(test)

## Use the probability cutoff of 50%.
test_pred_atrrition <- factor(ifelse(test$probability  >= 0.50, "Yes", "No"))
test_actual_atrrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_atrrition,test_pred_atrrition  )

# 50%                   test_pred_atrrition
# test_actual_atrrition   No  Yes
#                     No  1081   24
#                     Yes  155   57



## Use the probability cutoff of 60%.
test_pred_atrrition1 <- factor(ifelse(test$probability  >= 0.60, "Yes", "No"))
test_actual_atrrition1 <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_pred_atrrition1,test_actual_atrrition1 )

# 60%               test_actual_atrrition1
# test_pred_atrrition1   No  Yes
#                   No  1092  177
#                   Yes   13   35


# install.packages("caret")
# library(e1071)
# library(caret)


## confusion Matrix
test_conf <- confusionMatrix(test_actual_atrrition1,test_pred_atrrition1 , positive = "Yes")
test_conf$byClass[1]

test_conf

# Confusion Matrix and Statistics
#           Reference
# Prediction   No  Yes
# No  1092   13
# Yes  177   35
# 
# Accuracy : 0.8557          
# Sensitivity : 0.72917         
# Specificity : 0.86052         



##============ Choose the cutoff value ======== 

## Find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test_actual_atrrition,predicted_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


## to check the above function for cutoff value from 30% to highest value of probability 0.8959000 i.e 89.5%
s = seq(0.1,0.91,length=100)
#creating a output matrix of size 70
OUT1 <- matrix(0,100,3)


for(i in 1:99){
  OUT1[i,] <- perform_fn(s[i])
}
OUT1
#putting cut off value,sensitivity,Specificity and Accuracy in a single dataframe
#plotting them to check the cut off value 
OUT <- data.frame(s,OUT1[,1],OUT1[,2],OUT1[,3])
colnames(OUT)[1] <- "cutoff"
colnames(OUT)[2] <- "sensitivity"
colnames(OUT)[3] <- "specificity"
colnames(OUT)[4] <- "accuracy"
plot(OUT$cutoff,OUT$sensitivity,xlab = "CUTOFF",cex.axis=1.5,lwd=2,type = "l",col="red")
lines(OUT$cutoff,OUT$specificity,col="green",cex.axis=1.5,lwd=2)
lines(OUT$cutoff,OUT$accuracy,col="pink",cex.axis=1.5,lwd=2)
legend(0.7,0.2,col=c("red","green","pink"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
locator()

#the locator shows the cut off value at 0.50  
cutoff <- 0.50
test_cutoff_attrition <- factor(ifelse(test$probability >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_actual_atrrition,test_cutoff_attrition,positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec
#accuracy as 86.4%
#sensitivity as 70.37%
#Specificity as 87.5%


View(test)

##============ XXXXKS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_atrrition <- ifelse(test_actual_atrrition=="Yes",1,0)

#library(ROCR)
#library(caret)
#on testing  data
str(test_cutoff_attrition)
pred_object_test<- prediction(test_cutoff_attrition, test_actual_atrrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


##============ XXXXLift & Gain Chart ======

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

atrrition_decile = lift(test_actual_atrrition, test_pred, groups = 10)




       

