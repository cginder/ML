## Homework 3
## Josh Elder and Curt Ginder
## 2/22/18

## Load the data

PackageList =c('MASS','gbm','tree','randomForest','rpart','caret','ROCR','readxl','data.table','R.utils') 
NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages)
lapply(PackageList,require,character.only=TRUE)#array function



gitURL="https://github.com/ChicagoBoothML/MLClassData/raw/master/KDDCup2009_Customer_relationship/";
DownloadFileList=c("orange_small_train.data.gz","orange_small_train_appetency.labels.txt",
                   "orange_small_train_churn.labels.txt","orange_small_train_upselling.labels.txt")
LoadFileList=c("orange_small_train.data","orange_small_train_appetency.labels.txt",
               "orange_small_train_churn.labels.txt","orange_small_train_upselling.labels.txt")

for (i in 1:length(LoadFileList)){
  if (!file.exists(LoadFileList[[i]])){
    if (LoadFileList[[i]]!=DownloadFileList[[i]]) {
      download.file(paste(gitURL,DownloadFileList[[i]],sep=""),destfile=DownloadFileList[[i]])
      gunzip(DownloadFileList[[i]])
    }else{
      download.file(paste(gitURL,DownloadFileList[[i]],sep=""),destfile=DownloadFileList[[i]])}}
}

na_strings <- c('',
                'na', 'n.a', 'n.a.',
                'nan', 'n.a.n', 'n.a.n.',
                'NA', 'N.A', 'N.A.',
                'NaN', 'N.a.N', 'N.a.N.',
                'NAN', 'N.A.N', 'N.A.N.',
                'nil', 'Nil', 'NIL',
                'null', 'Null', 'NULL')

X=as.data.table(read.table('orange_small_train.data',header=TRUE,
                           sep='\t', stringsAsFactors=TRUE, na.strings=na_strings))
Y_churn    =read.table("orange_small_train_churn.labels.txt", quote="\"")
Y_appetency=read.table("orange_small_train_appetency.labels.txt", quote="\"")
Y_upselling=read.table("orange_small_train_upselling.labels.txt", quote="\"")


######################################################################################

################
## Data Cleaning


## First, remove variables with a high percentage of NA values

# Calculate # of na values for each variable
na_count <-sapply(X, function(y) sum(length(which(is.na(y))))) 
na_count

plot(na_count)

# Choose cutoff value for deleting variables

n = nrow(X) # number of instances in training set
cut = 0.5*n # identify variables where 50% or more of instances are NA
p = length(X) # 230 variables

# Remove variables where > 50% of values are NA
cut_list = na_count[na_count > cut] # identify variables
exclude_var = names(cut_list) # list names
XS=X[,!(names(X) %in% exclude_var),with=FALSE]
names(XS)

na_count_xs <-sapply(XS, function(y) sum(length(which(is.na(y)))))
plot(na_count_xs)
na_count_xs

length(XS) # 69 variables remain




## Create a new NA level for factor variables

str(XS)
p = length(XS)

# Before loop
is.factor(XS[[66]]) # variable 66 is a factor variable
length(which(is.na(XS[[66]]))) # variable 66 has 5211 NA values
levels(XS[[66]]) # NA is not currently represented as a level

for (i in 1:p) {
  if (is.factor(XS[[i]])){                   #Isolate factor variables
    CurrentColumn=XS[[i]]                    #Extraction of current column
    idx=is.na(CurrentColumn)                 #Locate the NAs
    CurrentColumn=as.character(CurrentColumn)#Convert from factor to characters
    CurrentColumn[idx]=paste(i,'_NA',sep="") #Add the new NA level strings
    CurrentColumn=as.factor(CurrentColumn)   #Convert back to factors
    XS[[i]]=CurrentColumn
  }
}

# Check that loop works
is.factor(XS[[66]]) # variable 66 is still a factor variable
length(which(is.na(XS[[66]]))) # 0 NA values
levels(XS[[66]]) # New Level was created
table(XS[[66]]) # New level has 5211 values

is.factor(XS[[61]])
length(which(is.na(XS[[61]]))) # 0 NA values
levels(XS[[61]]) # New Level was created
table(XS[[61]]) # New level has 703 values






## Combine Levels to reduce total number of levels within a factor variable

Thres_Low=249;      #Low Bucket
Thres_Medium=499;   #Medium Bucket
Thres_High=999;     #Large bucket

# Before loop
levels(XS[[69]]) # 30 levels
table(XS[[69]]) # multiple levels with minimal data points

for (i in 1:p) {
  if (is.factor(XS[[i]])){                   #Isolate factor variables
    CurrentColumn=XS[[i]]                    #Extraction of current column
    CurrentColumn_Table=table(CurrentColumn) #Tabulate the frequency
    levels(CurrentColumn)[CurrentColumn_Table<=Thres_Low]=paste(i,'_Low',sep="")
    CurrentColumn_Table=table(CurrentColumn) #Tabulate the new frequency 
    levels(CurrentColumn)[CurrentColumn_Table>Thres_Low & CurrentColumn_Table<=Thres_Medium ]=paste(i,'_Medium',sep="")
    CurrentColumn_Table=table(CurrentColumn) #Tabulate the new frequency
    levels(CurrentColumn)[CurrentColumn_Table>Thres_Medium & CurrentColumn_Table<=Thres_High ]=paste(i,'_High',sep="")
    XS[[i]]=CurrentColumn
  }
}

# After loop
levels(XS[[69]]) # 9 levels



## Replace NA values within numeric variables with the average 

# Check a numeric variable column with NA's
na_count_xs
class(XS[[1]]) # variable 1 is an integer
is.numeric(XS[[1]]) # true
mean(XS[[1]]) #NA
mean(XS[[1]][!is.na(XS[[1]])]) # 1326.437

for (i in 1:p) {
  if (is.numeric(XS[[i]])){
    CurrentColumn=XS[[i]]
    CurrentColumn[is.na(CurrentColumn)] <- mean(CurrentColumn[!is.na(CurrentColumn)])
    XS[[i]] = CurrentColumn
  }
}

# Check that loop is working
mean(XS[[1]]) # 1326.437
mean(XS[[2]]) # 1326.437

na_count_fin <-sapply(XS, function(y) sum(length(which(is.na(y))))) 
plot(na_count_fin)

# no more variables with NA's left





#########################
## Variable Selection

# First, combine X and Y data frames; we'll choose Churn for our prediction model

churn.df = cbind(XS,Y_churn)
str(churn.df)
hist(churn.df$V1)
churn.df$V1 = as.factor(churn.df$V1)

#Re-name levels
levels(churn.df$V1)[levels(churn.df$V1)=="-1"] <- "No Churn"
levels(churn.df$V1)[levels(churn.df$V1)=="1"] <- "Churn"
levels(churn.df$V1)
table(churn.df$V1)

# Separate data into training and test sets
set.seed(1)
train = sample.int(n, floor(0.80*n))
xtrain = churn.df[train,] #training set with 80% of training samples
xtest = churn.df[-train,] #test set with 20% of training samples

# Create a smaller training subset for variable selection
set.seed(1)
train = sample.int(nrow(xtrain), floor(0.25*nrow(xtrain)))
xvs = xtrain[train,] #1000 observations

# Run a random forest model and check variable importance
p = length(xvs)

rf = randomForest(xvs$V1~.,              #regression model
                   data=xvs, #data set
                   mtry=sqrt(p),     #number of variables to sample
                   ntree=1000,  #number of trees to grow
                   importance=TRUE#calculate variable importance measure (optional)
)

varImpPlot(rf)

var_imp = data.frame(rf$importance)
var_imp = var_imp[order(var_imp[,4], decreasing=TRUE),] #sort data frame by Mean Decrease Gini
hist(var_imp[,4]) #check distribution of variable importance

var_sel = var_imp[var_imp$MeanDecreaseGini > 20,]
nrow(var_sel) #31 variables

plot(var_sel$MeanDecreaseGini, col="white", main = "RF Variable Importance", ylab = "Mean Decrease Gini")
text(x = 1:31, y =var_sel$MeanDecreaseGini, labels = rownames(var_sel), cex=0.6)
dev.off()

## Create subset of training data frame based on the top 31 variables found from the Random Forest Model

var_names = rownames(var_sel)

xtrain2 = xtrain[,(names(xtrain) %in% var_names),with=FALSE] 
xtrain2$Churn = xtrain$V1

xtest2 = xtest[,(names(xtest) %in% var_names),with=FALSE] 
xtest2$Churn = xtest$V1







#####################################
## Train and validate the model

## Split training set into training and validation set
set.seed(1)
train = sample.int(nrow(xtrain2), floor(0.75*nrow(xtrain2)))
xtrain3 = xtrain2[train,] #training set with 75% of training samples
xval = xtrain2[-train,] #test set with 25% of training samples

## Initiate Loss Functions

# define loss function
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
  # some models predict probabilities that the data belongs to class 1,
  # This function convert probability to 0 - 1 labels
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

# Initiate Lists
phatV = list()
phatT = list()

#################
## Random Forests

ntrain = nrow(xtrain3)
nval = nrow(xval)
ntest = nrow(xtest2)
p = length(xtrain3)

# Initiate Parameters
m = round(sqrt(p)-1,0)
ntree = c(100, 500, 1000, 2000)
nodesize = c(10,20)
rf_params = expand.grid(m, ntree, nodesize)
colnames(rf_params) = c("Variables", "Trees", "Node Size")
phatV$rf = matrix(0.0, nval, nrow(rf_params))
phatT$rf = matrix(0.0, ntest, nrow(rf_params))

for (i in 1:nrow(rf_params)) {
  frf = randomForest(xtrain3$Churn~., data=xtrain3, #data set
                     mtry=m,     #number of variables to sample
                     ntree=rf_params[i,2],  #number of trees to grow
                     nodesize = rf_params[i,3])
  phat = predict(frf, newdata = xval, type="prob")[,2]
  phatV$rf[,i]=phat
  print(i)
}

phatV$rf

##########
## Boosting

## Convert Churn back to a numerical value with 0 and 1

xtrainB = xtrain3
xtrainB$Churn = as.numeric(xtrainB$Churn) - 1
xvalB = xval
xvalB$Churn = as.numeric(xval$Churn) - 1

hist(xtrainB$Churn)
hist(xvalB$Churn)

tree_depth = c(5, 10) 
tree_num = c(500, 1000, 2000) 
lambda=c(0.1, 0.01) 
boosting.params = expand.grid(tree_depth,tree_num,lambda) 
colnames(boosting.params) = c("tdepth","ntree","shrink")
phatV$boost = matrix(0.0, nval, nrow(boosting.params))  # we will store results here

set.seed(1)
for(i in 1:nrow(boosting.params)) { 
  boost_fit =gbm(xtrainB$Churn~.,data=xtrainB,distribution="bernoulli", 
                 interaction.depth=boosting.params[i,1],
                 n.trees=boosting.params[i,2],
                 shrinkage=boosting.params[i,3]) 
  phat = predict(boost_fit,
                 newdata=xvalB,
                 n.trees=boosting.params[i,2],
                 type="response") 
  phatV$boost[,i] = phat
  print(i)
} 



# Check Deviance Loss

lossL = list() 
nmethod = length(phatV) 
for(i in 1:nmethod) { 
  nrun = ncol(phatV[[i]]) 
  lvec = rep(0,nrun) 
  for(j in 1:nrun) 
    lvec[j] = lossf(xval$Churn, phatV[[i]][,j]) 
  lossL[[i]]=lvec; names(lossL)[i] = names(phatV)[i] 
} 
lossv = unlist(lossL)

# Plot the deviance loss for our models on the validation set
par(mfrow=c(1,1)) 
plot(lossv, ylab="loss on validation", type="n") 
nloss=0 
for(i in 1:nmethod) { 
  ii = nloss + 1:ncol(phatV[[i]]) 
  points(ii,lossv[ii],col=i,pch=17) 
  nloss = nloss + ncol(phatV[[i]]) } 
legend("topright",legend=names(phatV),col=1:nmethod,pch=rep(17,nmethod))

# Check which one is the minimum
lossv
which.min(lossv) #Boost 9
boosting.params[9,]
# Parameters: Depth = 5, Trees = 1000, Shrinkage = 0.01





#####################################################

## Re-fit RF to entire training + validation set

xtrainB2 = xtrain2
xtrainB2$Churn = as.numeric(xtrain2$Churn) - 1
xtestB = xtest2
xtestB$Churn = as.numeric(xtestB$Churn) - 1

boost_best = gbm(xtrainB2$Churn~.,data=xtrainB2,distribution="bernoulli", 
                 interaction.depth=5,
                 n.trees=1000,
                 shrinkage=0.01)


phat_btest = predict(boost_best,
                             newdata=xtestB,
                             n.trees=1000,
                             type="response") 

# Get confusion matrix on test set
getConfusionMatrix(xtestB$Churn, phat_btest, 0.5)
lossMR(xtestB$Churn, phat_btest, 0.5)
# 7.3% misclassificaiton

# Loss on test test
lossf(xtestB$Churn, phat_btest)
# 4883

# Get confusion matrix on training set
getConfusionMatrix(xtrainB2$Churn, boost_best$fit, 0.5)
lossMR(xtrainB2$Churn, boost_best$fit, 0.5)
# 7.3% misclassificaiton


# ROC Curves for Boosting Model
plot(c(0,1), c(0,1), xlab ="FPR", ylab="TPR", main="ROC Curve: Boosting Model", cex.lab=1, type="n")
pred = prediction(phat_btest, xtestB$Churn)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
lines(perf@x.values[[1]], perf@y.values[[1]],col=1)
abline(0,1, lty=2)





## Best RF Model
frf_best = randomForest(xtrain2$Churn~., data=xtrain2, #data set
                   mtry=m,     #number of variables to sample
                   ntree=2000,  #number of trees to grow
                   nodesize = 10)

phat_frtrain = predict(frf_best, type="prob")[,2]
phat_frtest = predict(frf_best, newdata = xtest2, type="prob")[,2]

# Get confusion matrix on test set
getConfusionMatrix(xtest2$Churn, phat_test, 0.5)
lossMR(xtest2$Churn, phat_test, 0.5)
# 7.4% misclassification

# Loss on Test Set
lossf(xtest2$Churn, phat_test)
# 5025



