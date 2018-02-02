## Libraries
library("gamlr")
library("rpart")
library("rpart.plot") # package that enhances plotting capabilities for rpart
library("randomForest")

## Evaluation Functions
# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# wht shrinks probs in phat towards .5 --- this helps avoid numerical problems don't use log(0)!
lossf = function(y,phat,wht=0.0000001) {
  if(is.factor(y)) y = as.numeric(y)-1
  phat = (1-wht)*phat + wht*.5
  py = ifelse(y==1, phat, 1-phat)
  return(-2*sum(log(py)))
}

# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# thr is the cut off value - everything above thr is classified as 1
getConfusionMatrix = function(y,phat,thr=0.5) {
  if(is.factor(y)) y = as.numeric(y)-1
  yhat = ifelse(phat > thr, 1, 0)
  tb = table(predictions = yhat, 
             actual = y)  
  rownames(tb) = c("predict_0", "predict_1")
  return(tb)
}

# deviance loss function
# y should be 0/1
# phat are probabilities obtain by our algorithm 
# thr is the cut off value - everything above thr is classified as 1
lossMR = function(y,phat,thr=0.5) {
  if(is.factor(y)) y = as.numeric(y)-1
  yhat = ifelse(phat > thr, 1, 0)
  return(1 - mean(yhat == y))
}

phatL = list() #store the test phat for the different methods here

## CV Function for polynomial functions (if used)
docv = function(x,y,set,nfold,doran=TRUE,verbose=TRUE,...)
{
  #a little error checking
  x = as.matrix(x)
  set = matrix(set, ncol = 1)
  if(!(is.matrix(x) | is.data.frame(x))) {cat('error in docv: x is not a matrix or data frame\n'); return(0)}
  if(!(is.vector(y))) {cat('error in docv: y is not a vector\n'); return(0)}
  if(!(length(y)==nrow(x))) {cat('error in docv: length(y) != nrow(x)\n'); return(0)}
  
  nset = nrow(set); 
  n=length(y) #get dimensions
  
  if(n==nfold) doran=FALSE #no need to shuffle if you are doing them all.
  cat('in docv: nset,n,nfold: ',nset,n,nfold,'\n')
  lossv = rep(0,nset) #return values
  if(doran) {ii = sample(1:n,n); y=y[ii]; x=x[ii,,drop=FALSE]} #shuffle rows
  
  fs = round(n/nfold) # fold size
  for(i in 1:nfold) { #fold loop
    bot = (i-1)*fs+1; 
    top = ifelse(i==nfold,n,i*fs); 
    ii = bot:top
    if(verbose) cat('on fold: ',i,', range: ',bot,':',top,'\n')
    xin = x[-ii,,drop=FALSE]; 
    yin=y[-ii]; 
    xout=x[ii,,drop=FALSE]; 
    yout=y[ii]
    xin = as.vector(xin)
    xout = as.vector(xout)
    datain = data.frame(x = xin, y = yin)
    dataout = data.frame(x = xout, y = yout)
    for(k in 1:nset) 
    { #setting loop
      fit = lm(y ~ poly(x, set[k,]), data = datain)
      yhat = predict(fit,  newdata = dataout)
      lossv[k]=lossv[k]+sqrt(mean((yout-yhat)^2))
    } 
  } 
  return(lossv)
}

## knn CV files
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R") #this has docvknn used below

## Download Files
download.file('https://raw.githubusercontent.com/ChicagoBoothML/ML2017/master/hw02/MovieReview_train.csv',
  'Movie_train.csv')
download.file('https://raw.githubusercontent.com/ChicagoBoothML/ML2017/master/hw02/MovieReview_test.csv',
  'Movie_test.csv')
mtrain_df = read.csv("Movie_train.csv") # name training data frame
mtest_df = read.csv("Movie_test.csv") # name testing data frame
attach(mtrain_df) # attach labels to training set

## Explore Data
names(mtrain_df) # check varaible names
ntrain = nrow(mtrain_df) # number of training observations
ntest = nrow(mtest_df) # number of test observations
summary(mtrain_df)
table(sentiment) # balanced data set
hist(length)

## standardize data
hist(mtrain_df$length)
mtrain_df$length <- log(mtrain_df$length)
hist(mtrain_df$length)

## create training and validation data sets
set.seed(99)
nfold <- 5  # number of folds used in other CV models 
nsamp <- nrow(mtrain_df)*((nfold-1)/nfold) # number of samples to grab per fold
tr <- sample(1:nrow(mtrain_df),nsamp)
df.train = mtrain_df[tr,] #training data
df.valid = mtrain_df[-tr,] #validation data

df.train.x <- df.train[,-392]
df.train.y <- as.data.frame(df.train[,392])
df.valid.x <- df.valid[,-392]
df.valid.y <- as.data.frame(df.valid[,392])
colnames(df.train.y)[1] <- "sentiment"
colnames(df.valid.y)[1] <- "sentiment"


#### Method 1 - Simple Logistic Regression
lm <- glm(sentiment~.,data = df.train,family = "binomial")

## calculate predictions
phat = predict(lm, newdata = df.valid, type="response")
phatL$logit = matrix(phat,ncol=1) 

#### Method 2 - Cross Validated Lasso
set.seed(99)

## generate lasso model (5-fold cross validation)
lasso <- cv.gamlr(x = df.train.x, y = df.train.y, lambda.min.ratio=1e-3,family="binomial")
plot(lasso)

## look at which words are retained
beta <- drop(coef(lasso)) # AICc default selection

o<-order(beta[-1],decreasing=TRUE) # order all coefficients, but the intercept
beta[-1][o[1:20]] # pick top twenty

o.neg <- order(beta[-1],decreasing=FALSE) # order all coefficients, but the intercept, worst words
beta[-1][o.neg[1:20]] # pick worst twenty

## calculate predictions
phat = predict(lasso, newdata = df.valid.x, type="response")
phatL$lasso = matrix(phat,ncol=1) 

#### Method 3 - CV Decision Trees
df.train.tree <- df.train; df.train.tree$sentiment <- as.factor(df.train.tree$sentiment) # convert y to factor
set.seed(99)
big.tree = rpart(sentiment~., data=df.train.tree, control=rpart.control(minsplit=5,  
                                       cp=0.0001,
                                       xval=10,
                                       method = "class")    # we will use 10-fold cv 
)

nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

# let us look at the cross-validation results
#
# the following prints out a table summarizing the output of cross-validation
# you want to find cp corrsponding to the smallest value of xerror 
(cptable = printcp(big.tree))
(bestcp = cptable[ which.min(cptable[,"xerror"]), "CP" ])   # this is the optimal cp parameter
plotcp(big.tree) # plot results
best.tree = prune(big.tree,cp=bestcp,method = "class")
rpart.plot(best.tree)

## calculate predictions
phat = predict(best.tree,newdata = df.valid.x,type = "prob")
phatL$tree = matrix(phat[,2],ncol=1) 

#### Method 4 - CV Random Forest
set.seed(99)

##settings for randomForest
p=ncol(df.train)
mtryv = c(p, sqrt(p))
ntreev = c(500,1000) # number of tree vectors to try
setrf = expand.grid(mtryv,ntreev)  # this contains all settings to try
colnames(setrf)=c("mtry","ntree")
phatL$rf = matrix(0.0,nrow(df.valid),nrow(setrf))  # we will store results here

df.train.rf <- df.train
df.train.rf$sentiment <- as.factor(df.train$sentiment)

df.valid.rf <- df.valid
df.valid.rf$sentiment <- as.factor(df.valid.rf$sentiment)


###fit rf
for(i in 1:nrow(setrf)) {
  #fit and predict
  frf = randomForest(sentiment~., data=df.train.rf, 
                     mtry=setrf[i,1],
                     ntree=setrf[i,2],
                     nodesize=10)
  phat = predict(frf, newdata=df.valid.rf, type="prob")[,2]
  phatL$rf[,i]=phat
}

#### Method 5 - Principle Component Analysis into various models
set.seed(99)
pcawords <- prcomp(df.train.x[,-1], scale=TRUE) # drop length -- might need to go back and scale = true
plot(pcawords, main="")

mtext(side=1, "Review Words PCs",  line=1, font=2)

# first few pcs
round(pcawords$rotation[,1:3],1) 

## calculate pc directions
zreview <- as.data.frame(predict(pcawords))

## merge in length again
lassoPCR.train <- as.data.frame(cbind(df.train.x$length,zreview))
colnames(lassoPCR.train)[1] <- "length"

lassoPCR <- cv.gamlr(x = lassoPCR.train, y = df.train.y, lambda.min.ratio=1e-3,family="binomial")
plot(lassoPCR)

## convert to factors
valid.zreview <- as.data.frame(predict(pcawords,newdata = df.valid.x[,-1]))
PCA.valid.x <- cbind(df.valid$length,valid.zreview)

phat = predict(lassoPCR, newdata = PCA.valid.x, type="response")
phatL$lassoPCR = matrix(phat,ncol=1) 

## PCR Logistic Regression
glm.PCR.df <- cbind(zreview,df.train.y)


BIC.vec <- 0
for(i in 1:50){
BIC.vec[i] <- BIC(glm(sentiment ~ ., data = glm.PCR.df[,c(1:i,391)]))

}

### minimum is 8
glm.PCA. valid.x <- cbind(df.valid$length,valid)
glm.PCR <- glm(sentiment ~ . ,data = glm.PCR.df[,c(1:8,391)], family = "binomial")

phat = predict(glm.PCR, newdata = PCA.valid.x, type="response")

phatL$glm.PCR <- matrix(phat,ncol=1)



#### Method 6 -- Boosting
set.seed(99)
##settings for boosting
idv = c(2,4,8)
ntv = c(1000,5000,10000)
shv = c(.1,.01)

## set y values to numeric
df.train.boost = df.train; df.train.boost$sentiment = as.numeric(df.train.boost$sentiment)
df.valid.boost = df.valid; df.valid.boost$sentiment = as.numeric(df.valid.boost$sentiment)

## fit model
for(i in 1:nrow(setboost)) {
  ##fit and predict
  fboost = gbm(sentiment~., data=df.train.boost, distribution="bernoulli",
               n.trees=setboost[i,2],
               interaction.depth=setboost[i,1],
               shrinkage=setboost[i,3])
  
  phat = predict(fboost,
                 newdata=df.valid.boost,
                 n.trees=setboost[i,2],
                 type="response")
  
  phatL$boost[,i] = phat
  print(i)
}



#### Partial Least Squares
install.packages("textir")
library(textir)

pls.model <- pls(x=df.train.x, y=as.numeric(df.train.y$sentiment), K=25)

phat = predict(pls.model,
               newdata=df.valid.x,
               n.trees=setboost[i,2],
               type="response")

phatL$pls = phat

#### Evaluate Different Models
## Logistic Regression
getConfusionMatrix(df.valid.y$sentiment, phatL[[1]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[1]][,1], 0.5), '\n')

## Lasso (Unfactorized)
getConfusionMatrix(df.valid.y$sentiment, phatL[[5]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[5]][,1], 0.5), '\n')

## Lasso (Factorized)
getConfusionMatrix(df.valid.y$sentiment, phatL[[4]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[4]][,1], 0.5), '\n')

## Tree
getConfusionMatrix(df.valid.y$sentiment, phatL[[3]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[3]][,1], 0.5), '\n')

## Random Forest
nrun = nrow(setrf)
for(j in 1:nrun) {
  print(setrf[j,])
  print("Confusion Matrix:")
  print(getConfusionMatrix(df.valid.boost$sentiment, phatL[[4]][,j], 0.5))
  cat('Missclassification rate = ', lossMR(df.valid.boost$sentiment, phatL[[4]][,j], 0.5), '\n')
}


## Boosting
nrun = nrow(setboost)
for(j in 1:nrun) {
  print(setboost[j,])
  print("Confusion Matrix:")
  print(getConfusionMatrix(df.valid.boost$sentiment, phatL[[6]][,j], 0.5))
  cat('Missclassification rate = ', lossMR(df.valid.boost$sentiment, phatL[[6]][,j], 0.5), '\n')
}

## PLS
getConfusionMatrix(df.valid.y$sentiment, phatL[[7]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[7]][,1], 0.5), '\n')


## GLM PCR
getConfusionMatrix(df.valid.y$sentiment, phatL[[8]][,1], 0.5)
cat('Missclassification rate = ', lossMR(df.valid.y$sentiment, phatL[[8]][,1], 0.5), '\n')


#### to test against presence vs actual count
test <- mtrain_df[]
test <- as.data.frame(cbind(test$length,(test[,c(2:391)] > 0) + 0,test$sentiment))
colnames(test)[c(1,392)] <- c("length","sentiment")

## repeated above procedure - ended up with worse predictions

