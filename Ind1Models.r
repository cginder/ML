# Libraries
require(gamlr)
require(randomForest)


# split into training/validation set
set.seed(99)
nfold <- 5  # number of folds used in other CV models 
nsamp <- nrow(df.train)*((nfold-1)/nfold) # number of samples to grab per fold
tr <- sample(1:nrow(df.train),nsamp)
df.tr = df.train[tr,] #training data
df.valid = df.train[-tr,] #validation data

X.tr <- df.tr[,-82]
Y.tr <- as.data.frame(df.tr[,82])
colnames(Y.tr)[1] <- "SalePrice"
X.valid <- df.valid[,-82]
Y.valid <- as.data.frame(df.valid[,82])
colnames(Y.valid)[1] <- "SalePrice"

pred <- list() # list for predictions

## Marginal Regression to Find Variables of Interest
mr.reg <- list()
for (i in 1:ncol(X.tr)){
  univ.reg <- glm(df.tr$SalePrice ~ df.tr[[i]])
  univ.sum <- summary(univ.reg)
  mr.reg[i] <- univ.sum$coefficients[2,4]
}
names(mr.reg) <- colnames(X.tr)
mr.reg <- as.numeric(mr.reg)

hist(mr.reg) ## lots of p-values < 0.1

source("fdr.R")

print(cutoff <- fdr_cut(mr.reg,0.01))
sum(mr.reg<=cutoff)

# cutoff is 0.0065, with 58 significant variables remaining

# create new training df based on significant MDr


## Lasso Regression
set.seed(99)

X.tr.lasso <- sparse.model.matrix(~ .^2, data = X.tr)[,-1]
X.valid.lasso <- sparse.model.matrix(~ .^2, data = X.valid)[,-1]

  
lasso1 <- cv.gamlr(y = Y.tr$SalePrice, x = X.tr.lasso,family = "gaussian",lambda.min.ratio=1e-3)
plot(lasso1)

pred$lasso <- predict(lasso1,newdata = X.valid.lasso)


## Random Forest
p = length(df.train)-1

rf.model1 = randomForest(SalePrice~.,              #regression model
                   data=df.train, #data set
                   mtry=p,     #number of variables to sample
                   ntree=1000,  #number of trees to grow
                   nodesize=10,#minimum node size on trees (optional)
                   maxnodes=20,#maximum number of terminal nodes (optional)
                   importance=TRUE#calculate variable importance measure (optional)
)

pred$rf <- predict(rf.model1,newdata = df.valid)


## Model Comparison
RMSE <- list()

# Lasso
RMSE$Lasso <- sqrt(mean((pred$lasso - df.valid$SalePrice)^2))
RMSE$RF <- sqrt(mean((pred$rf - df.valid$SalePrice)^2))

  