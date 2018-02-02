# Question 1

# Download the files

download.file(
  'https://raw.githubusercontent.com/ChicagoBoothML/ML2017/master/hw02/Bike_train.csv',
  'Bike_train.csv')
download.file(
  'https://raw.githubusercontent.com/ChicagoBoothML/ML2017/master/hw02/Bike_test.csv',
  'Bike_test.csv')

btrain_df = read.csv("Bike_train.csv") # name training data frame
btest_df = read.csv("Bike_test.csv") # name testing data frame

attach(btrain_df) # attach labels to training set
names(btrain_df) # check varaible names

ntest = nrow(btest_df) # Number of test observations

# Summarize the data

summary(btrain_df) # summary statistics

cor_vector = cor(btrain_df[,1:13], count) # check the correlation among different variables on Count
colnames(cor_vector) = c("Count Correlation")
print(cor_vector)
cor2_vector = cor(btrain_df, btrain_df) # correlation matrix among all variables
print(cor_vector)

hist(count) # check the distribution of count, we see that it's not normally distributed, skewed right
hist(log(count + 1)) #examine log transformation


# examine boxplots to understand relationships

boxplot(count~year, data=btrain_df) #2012 higher average rental and larger variance
boxplot(count~month, data=btrain_df) #Summer months seem to have higher rentals
boxplot(count~day, data=btrain_df) # day of month does not seem to capture much
boxplot(count~hour, data=btrain_df) # Minimal rentals from 0-6 and after 22
boxplot(count~season, data=btrain_df) # Yes, but better captured by month
boxplot(count~holiday, data=btrain_df) #Nothing meaningful
boxplot(count~workingday, data=btrain_df) #Nothing meaningful
boxplot(count~weather, data=btrain_df) # some impact, may be errors with 1.5 and 2.5; nicer weather = more rentals
boxplot(count~temp, data=btrain_df) #warmer weather = more rentals
boxplot(count~atemp, data=btrain_df) #similar relationship to temp
boxplot(count~humidity, data=btrain_df) #lower humidity = more rentals
boxplot(count~windspeed, data=btrain_df) #minimal relationship
boxplot(count~daylabel, data=btrain_df) #increasing over time
dev.off()

summary(count)

par(mfrow=c(2,2))
boxplot(count~hour, data=btrain_df) 
boxplot(count~month, data=btrain_df)
boxplot(count~atemp, data=btrain_df)
boxplot(count~humidity, data=btrain_df)

# Strong signal variables: Humidity, Temp, Hour, Month, Weather

# Check for errors in the data and clean up data matrix

# examine histogram of atmep, temp and temp - atemp
par(mfrow=c(1,3)) 
hist(atemp)
hist(temp)
hist(temp - atemp)
dev.off()

mean(temp - atemp)
min(temp - atemp)
max(temp - atemp)

which.max(temp - atemp)
btrain_df[which.max(temp - atemp),1]
# we see a 23.14 degree difference in temp and atemp in day 595, which seems odd
# Day 595 also has the same atemp recorded for the entire day

# compare to the test set
max(btest_df$temp - btest_df$atemp)
min(btest_df$temp - btest_df$atemp)
# largest delta is 11 degrees

# remove day 595 from training data, error in atemp data
btrain_df = subset(btrain_df, daylabel!=595)
attach(btrain_df)

# Check humidity data

hist(humidity)
which.min(humidity)
humidity[which.min(humidity)]
mean(humidity)
which(humidity==0)
btrain_df[which(humidity==0),1]
# day 69 has humidity recorded at 0

min(btest_df$humidity)
# min humidity in test set is 0

# Remove day 69 from training data, apparent error in humidity data
btrain_df = subset(btrain_df, daylabel!=69)
attach(btrain_df)
min(humidity)

# remove days with "season" of 1.5 and 2.5
btrain_df = subset(btrain_df, weather!= 1.5)
btrain_df = subset(btrain_df,weather!= 2.5)

# convert year, month, day, hour, season, holiday, workingday, weather to factors
btrain_df[,c(2:9)] <- lapply(btrain_df[,c(2:9)],factor)


# Split training set into training and test set
ntrain = nrow(btrain_df) #size of training vector
set.seed(1)
train = sample.int(ntrain, floor(0.8*ntrain))
btrain= btrain_df[train,]
btest= btrain_df[-train,]
attach(btrain)

# Fit regression analysis

# Simple Linear Regression

reg1 = lm(log(count+1) ~ atemp + humidity + month + hour + weather, data=btrain)
summary(reg1)
# Not Bad, 81% R-Squared

# Diagnostics
plot(reg1$fitted.values, reg1$residuals, cex=0.1)
abline(a=0, b=0)
mean(reg1$residuals)
cor(log(count+1), reg1$residuals) # a lot of explanatory power left in residuals

par(mfrow=c(1,2))
hist(rstudent(reg1))
qqnorm(rstudent(reg1), col=4)
abline(a=0, b=1)

summary(exp(reg1$fitted.values)-1)
summary(count)

# Get RMSE on Test Set
reg1.fitted = predict(reg1, newdata=btest) #fitted values, in form y = log(count+1)
reg1.fitted = exp(reg1.fitted) - 1 #transform to Y = Count

MSE.reg1 = sum((btest$count - reg1.fitted)^2) / nrow(btest)
RMSE.reg1 = sqrt(MSE.reg1)
print(RMSE.reg1)



# Forward Stepwise Regression
null = lm(log(count + 1) ~ 1, data=btrain)
full = lm(log(count + 1) ~ . + .^2, data = btrain)
reg.BIC <- step(null, scope=formula(full), direction="forward", k=log(ntrain)) # BIC criterion
summary(reg.BIC)

reg.AIC <- step(null, scope=formula(full), direction="forward") # AIC criterion
summary(reg.AIC)



# LASSO
set.seed(1)
library(glmnet)
names(btrain)
X <- model.matrix(~(daylabel + year + month + day + hour + season + holiday + workingday + weather + temp + atemp + humidity + windspeed)*(daylabel + year + month + day + hour + season + holiday + workingday + weather + temp + atemp + humidity + windspeed), btrain) 
X <- X[,-1]
dim(X)

cvfit <- cv.glmnet(x = X, y = log(count + 1), family="gaussian", alpha=1, standardize=FALSE)
betas <- coef(cvfit, s = "lambda.1se")
model <- which(betas[2:length(betas)]!=0)
colnames(X)[model]
reg.lasso1 <- lm(log(count +1) ~ X[,model])
summary(reg.lasso1)

betas2 <- coef(cvfit, s = "lambda.min")
model2 <- which(betas2[2:length(betas2)]!=0)
colnames(X)[model2]
reg.lasso2 <- lm(log(count +1) ~ X[,model2])
summary(reg.lasso2)

# Compare Regression Models

# AIC Criterion Measure
model.names <- c("reg.AIC", "reg.BIC", "reg.lasso1", "reg.lasso2")
AIC <- c(extractAIC(reg.AIC)[2],
         extractAIC(reg.BIC)[2],
         extractAIC(reg.lasso1)[2],
         extractAIC(reg.lasso2)[2])
AIC_values <- round(AIC, 0)

AIC_Table <- data.frame(model.names, AIC)
AIC_Table

eAIC <- exp(-0.5*(AIC-min(AIC)))
eAIC
probs <- eAIC/sum(eAIC)
AIC_Prob <- round(probs, 4)
print(AIC_Prob)
# Using AIC criterion, AIC is the best model

# BIC Criterion Measure
BIC <- c(extractAIC(reg.AIC, k=log(ntrain))[2],
         extractAIC(reg.BIC, k=log(ntrain))[2],
         extractAIC(reg.lasso1, k=log(ntrain))[2],
         extractAIC(reg.lasso2, k=log(ntrain))[2])
BIC_values <- round(BIC,0)

BIC_Table <- data.frame(model.names, BIC)
BIC_Table

eBIC <- exp(-0.5*(BIC-min(BIC)))
eBIC
BIC.Prob <- round(eBIC/sum(eBIC),4)
print(BIC.Prob)
# Using BIC criterion, BIC is the best model

# AIC And BIC Comparison of all models
model.ranking <- data.frame(model.names, AIC_values, BIC_values, AIC_Prob, BIC.Prob)
model.ranking

# Check distribution of training set residuals
par(mfrow=c(2,2))
hist(rstudent(reg.BIC))
hist(rstudent(reg.AIC))
hist(rstudent(reg.lasso1))
hist(rstudent(reg.lasso2))

# Calculate RMSE's of Regression Models

BIC.fitted = predict(reg.BIC, newdata=btest) #fitted values, in form y = log(count+1)
BIC.fitted = exp(BIC.fitted) - 1 #transform to Y = Count

AIC.fitted = predict(reg.AIC, newdata=btest) #fitted values, in form y = log(count+1)
AIC.fitted = exp(AIC.fitted) - 1 #transform to Y = Count

MSE.BIC = sum((btest$count - BIC.fitted)^2) / nrow(btest)
RMSE.BIC = sqrt(MSE.BIC)

MSE.AIC = sum((btest$count - AIC.fitted)^2) / nrow(btest)
RMSE.AIC = sqrt(MSE.AIC)

reg.names <- c("reg.AIC", "reg.BIC")
RMSE = c(RMSE.AIC, RMSE.BIC)
RMSE.table = data.frame(reg.names,RMSE)
print(RMSE.table)
# Not very good at predicting, large RMSE

lasso.test.X <- model.matrix(~(daylabel + year + month + day + hour + season + holiday + workingday + weather + temp + atemp + humidity + windspeed)*(daylabel + year + month + day + hour + season + holiday + workingday + weather + temp + atemp + humidity + windspeed), btest) 
lasso.test.X <- lasso.test.X[,-1]
lasso.test.X <- as.data.frame(lasso.test.X[,model])
X <- lasso.test.X

Lasso.Pred <- predict(reg.lasso1,newdata = as.data.frame(X[,model]))
Lasso.Pred <- exp((Lasso.Pred))-1

Lasso.RMSE <- sqrt(mean((Lasso.Pred-btest$count)^2))


# KNN Models
library(kknn)
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R") #this has docvknn used below

# Decide what variables we want to include (which ones most impactful in regression)
kx = cbind(hour, atemp, humidity)
head(kx)
mmsc=function(kx) {return((kx-min(kx))/(max(kx)-min(kx)))}
xs = apply(kx,2,mmsc) #apply scaling function to each column of x
head(xs)

set.seed(1)
kv = seq(from = 2, to = 750, by = 20) #these are the k values (k as in kNN) we will try
nk = length(kv)

cvtemp = docvknn(xs,log(count+1),kv,nfold=5)
cvtemp = sqrt(cvtemp/nrow(btrain)) #docvknn returns sum of squares
plot(log(1/kv),cvtemp,type = "l",col="red",lwd = 2,cex.lab = 1.0,xlab = "log(1/k)",ylab = "RMSE")
imin = which.min(cvtemp)
kv[imin]

#Refit best k on entire training set
ddf = data.frame(log(count+1),xs)
kfbest = kknn(log(count+1)~.,ddf,ddf,k=kv[imin], kernel="rectangular")

# test best model against the test set and measure the RMSE
kx.test = cbind(btest$hour, btest$atemp, btest$humidity)
xs.test = apply(kx.test,2,mmsc) #apply scaling function to each column of x
colnames(ddf)[1] <- "trans.count"
head(xs.test)
colnames(xs.test)[1:3] <- c("hour","atemp","humidity")
knnPred = kknn(trans.count~.,ddf,as.data.frame(xs.test),k=kv[imin], kernel="rectangular")

knn.RMSE <- sqrt(mean(((exp(knnPred$fitted.values)-1)-btest$count)^2))


# Decision Trees
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# Start with 1 big tree and prune
tree.train <- subset(btrain,select = c("month","hour","holiday","workingday","weather",
                              "temp","atemp","humidity","windspeed","count"))

temp = rpart((log(count)+1)~., data=tree.train, control=rpart.control(minsplit=5,  
                                                            cp=0.000001,
                                                            xval=0)   
)

rpart.plot(temp)
nbig <- length(unique(temp$where))
cat("size of big tree ",nbig,"\n")

plotcp(temp)
(cptable = printcp(temp))
(bestcp = cptable[which.min(cptable[,"xerror"]),"CP"])  #find the optimal cp (smallest value of xerror)

# prune tree
best.tree <- prune(temp,cp=bestcp)
rpart.plot(best.tree) #plot the beset tree
nbig <- length(unique(best.tree$where))
cat("size of best tree ",nbig,"\n")

# Calc MSE and RMSE of Best.Tree against Test Set
Tree.Pred = predict(best.tree,newdata = btest)
phatL$tree = matrix(phat[,2],ncol=1) 




# Random Forests
#List the packages we need, install if missing, then load all of them
PackageList =c('MASS','gbm','tree','randomForest','rpart') 
NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages)

lapply(PackageList,require,character.only=TRUE)#array function

set.seed(1) #Always set the seed for reproducibility

# Utility Function to Measure Performance
#Start stop watch timer
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")){
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

#Read elapsed time from stopwatch
toc <- function(){
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}



# Random Forest Inputs
p = length(btrain)-1
tic()
frf = randomForest(log(count + 1)~.,              #regression model
                   data=btrain, #data set
                   mtry=p,     #number of variables to sample
                   ntree=1000,  #number of trees to grow
                   nodesize=10,#minimum node size on trees (optional)
                   #maxnodes=20,#maximum number of terminal nodes (optional)
                   importance=TRUE#calculate variable importance measure (optional)
)
toc()

# Predictions and RMSE
rf.predict = predict(frf,newdata=btest)
rf.predict = exp(rf.predict)-1
MSE.rf = sum((btest$count - rf.predict)^2) / nrow(btest)
RMSE.rf = sqrt(MSE.rf)
print(RMSE.rf)

varImpPlot(frf) #check variable importance

# Boosting
# get rid of day and day label from training data set because these are mutually exclusive from testing set
btrain.boost <- btrain[,-c(1,4)]

# Input arguments
set.seed(1)
idv = c(2,4,8)
ntv = c(1000,5000,10000)
shv = c(.1,.05,.01)
setboost = expand.grid(idv,ntv,shv)
colnames(setboost) = c("tdepth","ntree","shrink")
q1 <- list()
q1$boost = matrix(0.0,nrow(btest),nrow(setboost))

for(i in 1:nrow(setboost)) {
  ##fit and predict
  fboost = gbm(log(count + 1)~.,              #regression model
               data=btrain.boost, #data set
               distribution="gaussian",# boost the squared error, "tdist", 'laplace'
               n.trees=setboost[i,2],
               interaction.depth=setboost[i,1],
               shrinkage=setboost[i,3])
  
  phat = predict(fboost,
                 newdata=btest,
                 n.trees=setboost[i,2])
  
  q1$boost[,i] = phat
  print(i)
}

fboost=gbm(log(count + 1)~.,              #regression model
           data=btrain.boost, #data set
           distribution="gaussian",# boost the squared error, "tdist", 'laplace'
           n.trees=5000,          #Total number of trees/iterations
           interaction.depth = 4, #1 means additive, 2 means 2-way interaction, etc
           shrinkage=0.1        #Shrinkage parameter, weak predictor
)

# Predictions and RMSE
nrun = nrow(setboost)
for(j in 1:nrun) {
  print(setboost[j,])
  boost.predict = exp(q1$boost[,j])-1
  boost.RMSE = sqrt(mean((btest$count - boost.predict)^2))
  print(boost.RMSE)
}


# Best Model is Tdepth 8, ntree 10000, shrink 0.01. Will refit entire model on all training data and predict for test
btrain_df[,c(2:9)] <- lapply(btrain_df[,c(2:9)],factor)
btrain.train.boost <- btrain_df[,-c(1,4)]

fboost=gbm(log(count + 1)~.,              #regression model
           data=btrain.train.boost, #data set
           distribution="gaussian",# boost the squared error, "tdist", 'laplace'
           n.trees=10000,          #Total number of trees/iterations
           interaction.depth = 8, #1 means additive, 2 means 2-way interaction, etc
           shrinkage=0.01        #Shrinkage parameter, weak predictor
)

#### Predict on Test Set
btest_df[,c(2:9)] <- lapply(btest_df[,c(2:9)],factor)
btrain.test.boost <- btest_df[,-c(1,4)]

Boost.Test.Pred <- predict(fboost,newdata = btrain.test.boost,n.trees = 10000)

Boost.Test.Pred <- exp(Boost.Test.Pred)-1

Boost.Test.Pred <- as.data.frame(Boost.Test.Pred)
colnames(Boost.Test.Pred)[1] <- "count"

summary(Boost.Test.Pred)
summary(btrain_df$count)

write.csv(Boost.Test.Pred,"Boost.Test.Pred.csv")
