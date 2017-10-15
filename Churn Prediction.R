
## CHURN PREDICTION MODEL FOR TELECOM ##

#----------------------------------------------------------------------------------
# I have taken below approach to obtain the 'Churn Prediction Model' 
#----------------------------------------------------------------------------------
# Examine the data set, variables, rows, number of churn , structure , missing values, descriptive stats, 
# Understanding correlation bewteen the variables
# Prepared one more data set by removing outliers

# I did the below models for 'original data set' and 'data set after removing outliers'

# Split the Data Set into 80 ~ 20 for Train and Test
# Split the data into 80~ 20 having equal proportion of '0' & '1'
# undresampling with 483 'Zeros' and 483 'Ones'
# undersampling with 1449 'zeros' and 483 'ones'
# undersampling with 2415 'zeros' and 483 'ones'

# Testing the Models and finding the Accuracy, ROC and AUC
# Choose the best model
# Finally adjusting the threshold probability for prediction by building a funciton
# Conclusion
#----------------------------------------------------------------------------------


## Import data set
data<-read.csv("D:/Data Analytics with Excel R and Tabaleu/Churn Prediction/Churn.csv")

## Understand the data set
View(data)      
nrow(data)                     # 3333 rows
head(data)
str(data)                      # State need to be converted to factor. alternately it can be eliminated to avoid the complexity of the model
names(data)
table(data$Churn)              # 483 (17%) customers churn and 2850 do not 
class(data)                    # data frame
summary(data)

data$State <- as.factor(data$State)

## Data Cleaning: some data might be missing or corrupted, therefore we need to prepare the dataset for our analysis
## visual take on the missing values 

library(Amelia)
missmap(data, main = "Missing values vs observed")        # No missing values observed

## Examining correlations among variables

library(car)

pairs(~Churn+Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins
      +Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge
      +Eve.Calls+Eve.Charge+Night.Calls+Intl.Calls+Intl.Charge, data=data)
# there are some direct correlation viz., Day Mins~Day Charge, Eve Mins ~ Eve Charge , Int Min ~ Int Charge

# original data set
data<-read.csv("D:/Data Analytics with Excel R and Tabaleu/Churn Prediction/Churn.csv")

# data set after removing outliers
data_9<-read.csv("D:/Data Analytics with Excel R and Tabaleu/Churn Prediction/Churn_clean2.csv")
data_9$State <- as.factor(data_9$State)
str(data_9)

# Split the original Data Set into 80 ~ 20 for Train and Test------------------------------
set.seed(1)                           # repeatability
index<-sample(1:nrow(data),0.8*nrow(data))
index

# training data set
train_data<-data[index,-c(21)]     # omit phone number & state otherwise it would be extremely complex
View(train_data)
table(train_data$Churn)                # 0 = 2282 , 1 =384

# test data set
test_data<-data[-index,]     
View(test_data)

str(train_data)
str(test_data)

# Model 1---------------------------------------------------------------------------
glm_model1.null<-glm(Churn~1,family=binomial(link="logit"),data=train_data)
summary(glm_model1.null)

glm_model1.full<-glm(Churn ~., family=binomial(link="logit"), data=train_data)

step(glm_model1.null, scope = list(upper=glm_model1.full), direction="both", test="Chisq", data=train_data)
glm_model1 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                    VMail.Plan + Eve.Charge + Intl.Charge + Intl.Calls + Night.Mins + 
                    Intl.Mins + VMail.Message, family = binomial(link = "logit"), 
                  data = train_data)

# Model 2---------------------------------------------------------------------------
# Split the data into 80~ 20 having equal proportion of '0' & '1'
data_1<-data[data$Churn==1,]
set.seed(1)
nrow(data_1)
ind_1<-sample(1:nrow(data_1),0.8*nrow(data_1))     

data_0<-data[data$Churn==0,]
set.seed(1)
nrow(data_0)
ind_0<-sample(1:nrow(data_0),0.8*nrow(data_0))

train_data1<-data[c(ind_1,ind_0) , -c(21) ]
as.data.frame(train_data1)
str(train_data1)
table(train_data1$Churn)

test_data1<-data[-c(ind_1,ind_0),]

glm_model2.null<-glm(Churn~1,family=binomial(link="logit"),data=train_data1)
summary(glm_model2.null)

glm_model2.full<-glm(Churn ~., family=binomial(link="logit"), data=train_data1)
step(glm_model2.null, scope = list(upper=glm_model2.full), direction="both", test="Chisq", data=train_data1)
glm_model2 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                    Eve.Mins + VMail.Plan + Intl.Charge + Intl.Calls + Night.Charge + 
                    Intl.Mins + VMail.Message, family = binomial(link = "logit"), 
                  data = train_data1)


# Model 3-----------------------------------------------------------------------
# undresampling with 483 Zeros and 483 Ones
library(ROSE)
data_483 <- ovun.sample(Churn ~ ., data = data, method = "under", N = 966, seed = 1)$data
data_483 <- data_483[, -c(21)]
names(data_483)
table(data_483$Churn)

glm_model483.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_483)
glm_model483.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_483)

step(glm_model483.null, scope = list(upper=glm_model483.full), direction="both", test="Chisq", data=data_483)

glm_model3<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                   VMail.Plan + Eve.Mins + Intl.Charge + Night.Mins + Intl.Calls + 
                   VMail.Message + Day.Calls + Intl.Mins + Day.Charge, family = binomial(link = "logit"), 
                 data = data_483)

# Model 4-------------------------------------------------------------------------
# undersampling with 1449 zeros and 483 ones
data_1449 <- ovun.sample(Churn ~ ., data = data, method = "under", N = 1932, seed = 1)$data
data_1449 <- data_1449[, -c(21)]
names(data_1449)
table(data_1449$Churn)

glm_model1449.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_1449)
glm_model1449.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_1449)

step(glm_model1449.null, scope = list(upper=glm_model1449.full), direction="both", test="Chisq", data=data_1449)

glm_model4<-glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                  VMail.Plan + Eve.Mins + Intl.Charge + Night.Charge + Intl.Calls + 
                  VMail.Message, family = binomial(link = "logit"), data = data_1449)

# Model 5--------------------------------------------------------------------------
# undersampling with 2415 zeros and 483 ones
data_2415 <- ovun.sample(Churn ~ ., data = data, method = "under", N = 2898, seed = 1)$data
data_2415 <- data_2415[, -c(21)]
names(data_2415)
table(data_2415$Churn)

glm_model2415.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_2415)
glm_model2415.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_2415)

step(glm_model2415.null, scope = list(upper=glm_model2415.full), direction="both", test="Chisq", data=data_2415)

glm_model5<-glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                  VMail.Plan + Eve.Mins + Intl.Charge + Intl.Calls + Night.Charge + 
                  VMail.Message, family = binomial(link = "logit"), data = data_2415)

#+==================================================================================
# Split the 'data set obtained after removing outliers' into 80 ~ 20 for Train and Test
set.seed(1)                           # repeatability
index<-sample(1:nrow(data_9),0.8*nrow(data_9))
index

# training data set
train_data9<-data_9[index,-c(21)]     # omit phone number & state
View(train_data9)
table(train_data9$Churn)                # 0 = 2266 , 1 =383

# test data set
test_data9<-data_9[-index,]     
View(test_data9)

str(train_data9)
str(test_data9)


# Model 91---------------------------------------------------------------------------
glm_model91.null<-glm(Churn~1,family=binomial(link="logit"),data=train_data9)
summary(glm_model91.null)

glm_model91.full<-glm(Churn ~., family=binomial(link="logit"), data=train_data9)

step(glm_model91.null, scope = list(upper=glm_model91.full), direction="both", test="Chisq", data=train_data9)
glm_model91 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                     VMail.Plan + Eve.Mins + Intl.Charge + Intl.Calls + Night.Charge, 
                   family = binomial(link = "logit"), data = train_data9)

# Model 92---------------------------------------------------------------------------
# Split the data into 80~ 20 having equal proportion of '0' & '1'
data_91<-data_9[data_9$Churn==1,]
set.seed(1)
nrow(data_91)
ind_91<-sample(1:nrow(data_91),0.8*nrow(data_91))     

data_90<-data_9[data_9$Churn==0,]
set.seed(1)
nrow(data_90)
ind_90<-sample(1:nrow(data_90),0.8*nrow(data_90))

train_data91<-data_9[c(ind_91,ind_90) , -c(21) ]
as.data.frame(train_data91)
str(train_data91)
table(train_data91$Churn)

test_data91<-data_9[-c(ind_91,ind_90),]

glm_model92.null<-glm(Churn~1,family=binomial(link="logit"),data=train_data91)
summary(glm_model92.null)

glm_model92.full<-glm(Churn ~., family=binomial(link="logit"), data=train_data91)
step(glm_model92.null, scope = list(upper=glm_model92.full), direction="both", test="Chisq", data=train_data91)
glm_model92 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                     VMail.Plan + Eve.Mins + Intl.Charge + Intl.Calls + Night.Charge + 
                     Intl.Mins + VMail.Message + Night.Calls, family = binomial(link = "logit"), 
                   data = train_data91)


# Model 93-----------------------------------------------------------------------
# undresampling with 483 Zeros and 483 Ones
library(ROSE)
data_9_483 <- ovun.sample(Churn ~ ., data = data_9, method = "under", N = 966, seed = 1)$data
data_9_483 <- data_9_483[, -c(21)]
names(data_9_483)
table(data_9_483$Churn)

glm_model9483.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_9_483)
glm_model9483.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_9_483)

step(glm_model9483.null, scope = list(upper=glm_model9483.full), direction="both", test="Chisq", data=data_9_483)

glm_model93<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                    Eve.Mins + VMail.Plan + Intl.Calls + Intl.Charge + Day.Calls, 
                  family = binomial(link = "logit"), data = data_9_483)

# Model 94-------------------------------------------------------------------------
# undersampling with 1449 zeros and 483 ones
data_9_1449 <- ovun.sample(Churn ~ ., data = data_9, method = "under", N = 1932, seed = 1)$data
data_9_1449 <- data_9_1449[, -c(21)]
names(data_9_1449)
table(data_9_1449$Churn)

glm_model91449.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_9_1449)
glm_model91449.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_9_1449)

step(glm_model91449.null, scope = list(upper=glm_model91449.full), direction="both", test="Chisq", data=data_9_1449)

glm_model94<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Charge + 
                    Eve.Mins + VMail.Plan + Intl.Charge + Night.Charge + Intl.Calls + 
                    Day.Calls + Intl.Mins, family = binomial(link = "logit"), 
                  data = data_9_1449)


# Model 95--------------------------------------------------------------------------
# undersampling with 2415 zeros and 483 ones
data_9_2415 <- ovun.sample(Churn ~ ., data = data_9, method = "under", N = 2898, seed = 1)$data
data_9_2415 <- data_9_2415[, -c(21)]
names(data_9_2415)
table(data_9_2415$Churn)

glm_model92415.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_9_2415)
glm_model92415.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_9_2415)

step(glm_model92415.null, scope = list(upper=glm_model92415.full), direction="both", test="Chisq", data=data_9_2415)

glm_model95<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Charge + 
                    VMail.Plan + Eve.Mins + Intl.Calls + Intl.Charge + Night.Charge + 
                    VMail.Message + Day.Calls, family = binomial(link = "logit"), 
                  data = data_9_2415)
#=========================================================================================

# Testing the Models and finding the Accuracy, ROC and AUC

# Check dispersion of models
summary(glm_model1)$deviance / summary(glm_model1)$df.residual
summary(glm_model2)$deviance / summary(glm_model2)$df.residual
summary(glm_model3)$deviance / summary(glm_model3)$df.residual
summary(glm_model4)$deviance / summary(glm_model4)$df.residual
summary(glm_model5)$deviance / summary(glm_model5)$df.residual

summary(glm_model91)$deviance / summary(glm_model91)$df.residual
summary(glm_model92)$deviance / summary(glm_model92)$df.residual
summary(glm_model93)$deviance / summary(glm_model93)$df.residual
summary(glm_model94)$deviance / summary(glm_model94)$df.residual
summary(glm_model95)$deviance / summary(glm_model95)$df.residual
# none of the above models are overfitted

# Finding the best model

# ROC and AUC

library(ROCR)
pred1 <- predict(glm_model1,test_data,type="response")
pr1<-prediction(pred1, test_data$Churn)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1               # model1

pred2 <- predict(glm_model2,test_data1,type="response")
pr2<-prediction(pred2, test_data1$Churn)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)
auc2 <- performance(pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2               # model2

pred3 <- predict(glm_model3,test_data,type="response")
pr3<-prediction(pred3, test_data$Churn)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3)
auc3 <- performance(pr3, measure = "auc")
auc3 <- auc3@y.values[[1]]
auc3                # model3

pred4 <- predict(glm_model4,test_data,type="response")
pr4<-prediction(pred4, test_data$Churn)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
plot(prf4)
auc4 <- performance(pr4, measure = "auc")
auc4 <- auc4@y.values[[1]]
auc4               # model4

pred5 <- predict(glm_model5,test_data,type="response")
pr5<-prediction(pred5, test_data$Churn)
prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
plot(prf5)
auc5 <- performance(pr5, measure = "auc")
auc5 <- auc5@y.values[[1]]
auc5               # model5

pred91 <- predict(glm_model91,test_data9,type="response")
pr91<-prediction(pred91, test_data9$Churn)
prf91 <- performance(pr91, measure = "tpr", x.measure = "fpr")
plot(prf91)
auc91 <- performance(pr91, measure = "auc")
auc91 <- auc91@y.values[[1]]
auc91               # model91

pred92 <- predict(glm_model92,test_data91,type="response")
pr92<-prediction(pred92, test_data91$Churn)
prf92 <- performance(pr92, measure = "tpr", x.measure = "fpr")
plot(prf92)
auc92 <- performance(pr92, measure = "auc")
auc92 <- auc92@y.values[[1]]
auc92               # model92

pred93 <- predict(glm_model93,test_data9,type="response")
pr93<-prediction(pred93, test_data9$Churn)
prf93 <- performance(pr93, measure = "tpr", x.measure = "fpr")
plot(prf93)
auc93 <- performance(pr93, measure = "auc")
auc93 <- auc93@y.values[[1]]
auc93               # model93

pred94 <- predict(glm_model94,test_data9,type="response")
pr94<-prediction(pred94, test_data9$Churn)
prf94 <- performance(pr94, measure = "tpr", x.measure = "fpr")
plot(prf94)
auc94 <- performance(pr94, measure = "auc")
auc94 <- auc94@y.values[[1]]
auc94               # model94

pred95 <- predict(glm_model95,test_data9,type="response")
pr95<-prediction(pred95, test_data9$Churn)
prf95 <- performance(pr95, measure = "tpr", x.measure = "fpr")
plot(prf95)
auc95 <- performance(pr95, measure = "auc")
auc95 <- auc95@y.values[[1]]
auc95               # model95

sort(as.data.frame(table(auc1, auc2, auc3, auc4, auc5, 
                         auc91, auc92, auc93, auc94, auc95)))    # Model 10 has greater accuracy

# Accuracy
library(rpart)
pred1 <- predict(glm_model1,test_data,type="response")
pred1 <- ifelse(pred1 > 0.5,1,0)
plot(test_data$Churn~pred1)
cfmatrix1=table(test_data$Churn,pred1)
cfmatrix1
Accuracy_model1<-((cfmatrix1[1]+cfmatrix1[2,2])/
                    (cfmatrix1[1,1]+cfmatrix1[1,2]+cfmatrix1[2,1]+cfmatrix1[2,2]))
Accuracy_model1               # model1

pred2 <- predict(glm_model2,test_data1,type="response")
pred2 <- ifelse(pred2 > 0.5,1,0)
plot(test_data1$Churn~pred2)
cfmatrix2=table(test_data1$Churn,pred2)
cfmatrix2
Accuracy_model2<-((cfmatrix2[1]+cfmatrix2[2,2])/
                    (cfmatrix2[1,1]+cfmatrix2[1,2]+cfmatrix2[2,1]+cfmatrix2[2,2]))
Accuracy_model2               # model2

pred3 <- predict(glm_model3,test_data,type="response")
pred3 <- ifelse(pred3 > 0.5,1,0)
plot(test_data$Churn~pred3)
cfmatrix3=table(test_data$Churn,pred3)
cfmatrix3
Accuracy_model3<-((cfmatrix3[1]+cfmatrix3[2,2])/
                    (cfmatrix3[1,1]+cfmatrix3[1,2]+cfmatrix3[2,1]+cfmatrix3[2,2]))
Accuracy_model3               # model3

pred4 <- predict(glm_model4,test_data,type="response")
pred4 <- ifelse(pred4 > 0.5,1,0)
plot(test_data$Churn~pred4)
cfmatrix4=table(test_data$Churn,pred4)
cfmatrix4
Accuracy_model3<-((cfmatrix4[1]+cfmatrix4[2,2])/
                    (cfmatrix4[1,1]+cfmatrix4[1,2]+cfmatrix4[2,1]+cfmatrix4[2,2]))
Accuracy_model4               # model4

pred5 <- predict(glm_model5,test_data,type="response")
pred5 <- ifelse(pred5 > 0.5,1,0)
plot(test_data$Churn~pred5)
cfmatrix5=table(test_data$Churn,pred5)
cfmatrix5
Accuracy_model5<-((cfmatrix5[1]+cfmatrix5[2,2])/
                    (cfmatrix5[1,1]+cfmatrix5[1,2]+cfmatrix5[2,1]+cfmatrix5[2,2]))
Accuracy_model5               # model5

pred91 <- predict(glm_model91,test_data9,type="response")
pred91 <- ifelse(pred91 > 0.5,1,0)
plot(test_data9$Churn~pred91)
cfmatrix91=table(test_data9$Churn,pred91)
cfmatrix91
Accuracy_model91<-((cfmatrix91[1]+cfmatrix91[2,2])/
                     (cfmatrix91[1,1]+cfmatrix91[1,2]+cfmatrix91[2,1]+cfmatrix91[2,2]))
Accuracy_model91               # model91

pred92 <- predict(glm_model92,test_data91,type="response")
pred92 <- ifelse(pred92 > 0.5,1,0)
plot(test_data91$Churn~pred92)
cfmatrix92=table(test_data91$Churn,pred92)
cfmatrix92
Accuracy_model92<-((cfmatrix92[1]+cfmatrix92[2,2])/
                     (cfmatrix92[1,1]+cfmatrix92[1,2]+cfmatrix92[2,1]+cfmatrix92[2,2]))
Accuracy_model92               # model92

pred93 <- predict(glm_model93,test_data9,type="response")
pred93 <- ifelse(pred93 > 0.5,1,0)
plot(test_data9$Churn~pred93)
cfmatrix93=table(test_data9$Churn,pred93)
cfmatrix93
Accuracy_model93<-((cfmatrix93[1]+cfmatrix93[2,2])/
                     (cfmatrix93[1,1]+cfmatrix93[1,2]+cfmatrix93[2,1]+cfmatrix93[2,2]))
Accuracy_model93               # model93

pred94 <- predict(glm_model94,test_data9,type="response")
pred94 <- ifelse(pred94 > 0.5,1,0)
plot(test_data9$Churn~pred94)
cfmatrix94=table(test_data9$Churn,pred94)
cfmatrix94
Accuracy_model94<-((cfmatrix94[1]+cfmatrix94[2,2])/
                     (cfmatrix94[1,1]+cfmatrix94[1,2]+cfmatrix94[2,1]+cfmatrix94[2,2]))
Accuracy_model94               # model94

pred95 <- predict(glm_model95,test_data9,type="response")
pred95 <- ifelse(pred95 > 0.5,1,0)
plot(test_data9$Churn~pred95)
cfmatrix95=table(test_data9$Churn,pred95)
cfmatrix95
Accuracy_model95<-((cfmatrix95[1]+cfmatrix95[2,2])/
                     (cfmatrix95[1,1]+cfmatrix95[1,2]+cfmatrix95[2,1]+cfmatrix95[2,2]))
Accuracy_model95               # model95

sort(as.data.frame(table(Accuracy_model1, Accuracy_model2, Accuracy_model3, Accuracy_model4, 
                         Accuracy_model5, Accuracy_model91, Accuracy_model92, Accuracy_model93, Accuracy_model94,
                         Accuracy_model95)))  # Model 1 has greater accuracy  ###### I also tried with models testing for test_data

#-----------------------------------------------------------------------
# From all above testing we can say that  MODEL 1 is best fit
#-----------------------------------------------------------------------

# Adjusting the probability limit in prediction

Accuracyfunction<- function(x){
  glm_model1 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                      VMail.Plan + Eve.Charge + Intl.Charge + Intl.Calls + Night.Mins + 
                      Intl.Mins + VMail.Message, family = binomial(link = "logit"), 
                    data = train_data)
  pred1 <- predict(glm_model1,test_data,type="response")
  pred1 <- ifelse(pred1 > x ,1,0)
  plot(test_data$Churn~pred1)
  cfmatrix1=table(test_data$Churn,pred1)
  cfmatrix1
  Accuracy_model1<-((cfmatrix1[1]+cfmatrix1[2,2])/
                      (cfmatrix1[1,1]+cfmatrix1[1,2]+cfmatrix1[2,1]+cfmatrix1[2,2]))
  paste(Accuracy = Accuracy_model1)}

x<- c(Accuracyfunction(0.3),
      Accuracyfunction(0.4),
      Accuracyfunction(0.5),
      Accuracyfunction(0.6),
      Accuracyfunction(0.7))
as.data.frame(x)
# threshhold probability is between 0.4 to 0.6

x<- c(Accuracyfunction(0.406),
      Accuracyfunction(0.408),
      Accuracyfunction(0.500),
      Accuracyfunction(0.502),
      Accuracyfunction(0.504),
      Accuracyfunction(0.506),
      Accuracyfunction(0.508))
as.data.frame(x)

# Hence Threshhold Probability = 0.506

#============================================================================================
#     CONCLUSION       #
#============================================================================================

# Thus the final Model is as below with threshhold probability = 0.506

glm_model_final <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + VMail.Plan +
                         Eve.Charge + Intl.Charge + Intl.Calls + Night.Mins + Intl.Mins + VMail.Message, 
                       family = binomial(link = "logit"), data = train_data)

summary(glm_model_final)
pred_final <- predict(glm_model_final,test_data,type="response")
pred_final <- ifelse(pred_final > 0.506 ,1,0)

#============================================================================================

#============================================================================================

# for working in Tableau
library(Rserve)
Rserve()

