download.file(
    paste("https://raw.githubusercontent.com/ChicagoBoothML/MLClassData/master/",
          "HumanActivityRecognitionUsingSmartphones/ParseData.R",sep=""),
    "ParseData.R")
source("ParseData.R")

# load data
data = parse_human_activity_recog_data()

###### fun starts here

library(h2o)

# start h2o server
h2oServer = h2o.init(nthreads = -1)

# load data into h2o format
Xtrain = as.h2o(data$X_train, destination_frame = "Xtrain")
Ytrain = as.h2o(data$y_train, destination_frame = "Ytrain")
Xtest = as.h2o(data$X_test, destination_frame = "Xtest")
Ytest = as.h2o(data$y_test, destination_frame = "Ytest")

head(Xtrain)
names(Xtrain)
str(Xtrain)
dim(Xtrain)
dim(Ytrain)

# train a simple neural network
p = ncol(Xtrain)
simpleNN = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                            training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                            hidden = 10,      # 1 hidden layer with 10 neurons
                            epochs = 5,       # this is a test run, so 5 epochs is fine
                            model_id = "simple_nn_model"
                            )

phat = h2o.predict(simpleNN, Xtest) # compute probabilities for new data 

o = h2o.confusionMatrix(simpleNN, h2o.cbind(Xtest, Ytest)) # compute confusion matrix
names(o)
o$Error

# not bad, but can we do better???????

# simple model worked.... maybe try something deeper
simpleDL = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                            training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                            hidden = c(50, 20),      # 2 hidden layers with 50 and 20 neurons 
                            epochs = 5,       # this is a test run, so 5 epochs is fine
                            l1 = 1e-5,          # regularize
                            model_id = "simple_dl_model"
                            )
  
h2o.confusionMatrix(simpleDL, h2o.cbind(Xtest, Ytest)) # compute confusion matrix

# hmmmm.... not much improvement, can you help me and find a better model????

### here are some tree models

# random forest
rf.model = h2o.randomForest(x=1:p, y=p+1,     # specify which columns are features and which are target
                            training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                            ntrees = 5,                                   # this poor forest only has 5 trees
                            min_rows = 20,                                # each leaf needs to have at least 20 nodes
                            max_depth = 10,                               # we do not want too deep trees
                            model_id = "simple_rf_model"
                            )

# random forest
rf.model = h2o.randomForest(x=1:p, y=p+1,     # specify which columns are features and which are target
                            training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                            ntrees = 50,                                  # this poor forest only has 5 trees
                            min_rows = 20,                                # each leaf needs to have at least 20 nodes
                            max_depth = 10,                               # we do not want too deep trees
                            model_id = "simple_rf_model",
                            nfold = 5
                            )

h2o.confusionMatrix(rf.model, h2o.cbind(Xtest, Ytest)) # compute confusion matrix

# boosting

gbm.model =  h2o.gbm(x=1:p, y=p+1,     # specify which columns are features and which are target
                     training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                     ntrees = 5,                                   # this poor boosting model only has 5 trees
                     min_rows = 20,                                # each leaf needs to have at least 20 nodes
                     max_depth = 3,                                # we want shallow trees
                     model_id = "simple_gbm_model"
                     )

h2o.confusionMatrix(gbm.model, h2o.cbind(Xtest, Ytest)) # compute confusion matrix



############################################################################################################
# Testing out new models

# Neural Networks

# Try 3 layers
set.seed(1)
DL1 = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                       training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                       hidden = c(10,10,10),  # 3 layers with 10 neurons each     
                       epochs = 5,       
                       l1 = 1e-5,          # regularize
                       model_id = "dl1_model"
)

h2o.confusionMatrix(DL1, h2o.cbind(Xtest, Ytest)) # compute confusion matrix
# 6.3%

# More neurons per layer
set.seed(1)
DL2 = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                       training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                       hidden = c(50,50,50),  # 3 layers with 50 neurons each       
                       epochs = 5,       
                       l1 = 1e-5,          # regularize
                       model_id = "dl2_model"
)

h2o.confusionMatrix(DL2, h2o.cbind(Xtest, Ytest)) # compute confusion matrix
# 6.8%, too complex

# change function type from rectifier to tanh
set.seed(1)
DL3 = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                       training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                       hidden = c(10,10,10),  # 3 layers with 10 neurons each       
                       epochs = 5,       
                       l1 = 1e-5,          # regularize
                       activation = "Tanh",
                       model_id = "dl3_model"
)

h2o.confusionMatrix(DL3, h2o.cbind(Xtest, Ytest)) # compute confusion matrix
# 5.3%

# Loop through multiple parameters for a single layer neural net

neurons = c(5,10,20,50)
epoch = c(5,10)
activ = c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout")
nn.params = expand.grid(neurons,epoch,activ)
colnames(nn.params) = c("Neurons", "Epochs", "Function")
num_params = nrow(nn.params)
error_vec = rep(0,num_params)

set.seed(1)
for(i in 1:num_params) {
  nn = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                        training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                        hidden = nn.params[i,1],         
                        epochs = nn.params[i,2],
                        activation = as.character(nn.params[i,3])
  )
  conf = h2o.confusionMatrix(nn, h2o.cbind(Xtest, Ytest))
  error_vec[i] = conf$Error[7]
  print(i)         
}

nn.params[which.min(error_vec),]
# Parameters: 10 neurons, 5 epochs, Tanh

error_vec[which.min(error_vec)]
# 4.8% misclassificaiton rate

# Loop with 2 hidden layers

neurons = c(5,10,20)
epoch = c(5,10)
nn.params = expand.grid(neurons,epoch)
colnames(nn.params) = c("Neurons", "Epochs")
num_params = nrow(nn.params)
error_vec = rep(0,num_params)

set.seed(1)
for(i in 1:num_params) {
  nn = h2o.deeplearning(x=1:p, y=p+1,     # specify which columns are features and which are target
                        training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                        hidden = c(nn.params[i,1],nn.params[i,1]),         
                        epochs = nn.params[i,2],
                        activation = "RectifierWithDropout"
  )
  conf = h2o.confusionMatrix(nn, h2o.cbind(Xtest, Ytest))
  error_vec[i] = conf$Error[7]
  print(i)         
}

nn.params[which.min(error_vec),]
# Parameters: 20 neurons, 5 epochs

error_vec[which.min(error_vec)]
# 6.8% misclassificaiton rate


#############
# Try more boosting models

tree_depth = c(5,10)
tree_num = c(100, 500, 1000)
boosting_params = expand.grid(tree_depth, tree_num)
colnames(boosting_params) = c("Depth", "Trees")
num_params = nrow(boosting_params)
error_vec = rep(0,num_params)

set.seed(1)
for (i in 1:num_params) {
  gbm.model =  h2o.gbm(x=1:p, y=p+1,     # specify which columns are features and which are target
                     training_frame = h2o.cbind(Xtrain, Ytrain),   # combine features with labels
                     max_depth = boosting_params[i,1],
                     ntrees = boosting_params[i,2]
                     )
  conf = h2o.confusionMatrix(gbm.model, h2o.cbind(Xtest, Ytest))
  error_vec[i] = conf$Error[7]
  print(i)
}

boosting_params[which.min(error_vec),]
# Parameters: depth = 5, trees = 1000

error_vec[which.min(error_vec)]
# 6.4% misclassificaiton rate
