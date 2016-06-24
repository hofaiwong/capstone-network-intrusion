library(e1071)
library(caret)
library(klaR)

#---------------------- Define functions ---------------------------#
#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive: ',contingency[2,2]/sum(contingency[,2]),'\n')
  cat('False negative: ',contingency[2,1]/sum(contingency[,1]),'\n')
  return(contingency)
}

#Factorize columns
factor.bin = function(df) {
  bin.col = data.frame(matrix(ncol = 2, nrow = ncol(df)))
  colnames(bin.col) = c('col','unique')
  for (i in 1:ncol(df)) {
    bin.col[i,1]=i
    bin.col[i,2]=length(unique(df[,i]))
    # bin.col[i,3]=max(df[,i])
    # bin.col[i,4]=min(df[,i])
  }
  bin.col = bin.col[bin.col$unique==2,1]
  for (i in bin.col){
    df[,i] = as.factor(df[,i])
  }
  for (i in c('protocol_type','service','flag')) {
    df[,i] = as.factor(df[,i])
  }
  return(df)
}

#---------------------- Load data for shuffled ---------------------------#
colnames(KDD.shuffle)[42:43]=c('outcome','outcome.response')
KDD.shuffle[43] <- ifelse(KDD.shuffle$outcome == 'normal',0,1)
bayes_train = KDD.shuffle[shuffle.train, -c(20,42)]
bayes_test = KDD.shuffle[-shuffle.train, -c(20,42)]

#---------------------- Load data for unshuffled ---------------------------#
colnames(KDD.train)[42]='outcome'
colnames(KDD.train)[43]='outcome.response'
KDD.train[43] <- ifelse(KDD.train$outcome == 'normal',0,1)

colnames(KDD.test)[42]='outcome'
colnames(KDD.test)[43]='outcome.response'
KDD.test[43] <- ifelse(KDD.test$outcome == 'normal',0,1)

bayes_train = KDD.train[, -c(20,42)]
bayes_test = KDD.test[, -c(20,42)]

bayes_train = factor.bin(bayes_train)
bayes_test = factor.bin(bayes_test)

#---------------------- Split test and train into predictor and labels ---------------------------#
# bayes_train_labels = as.factor(bayes_train[,41])
# bayes_train = bayes_train[,-41]
# bayes_test_labels = as.factor(bayes_test[,41])
# bayes_test = bayes_test[,-41]

#---------------------- Reduce data size for quick testing ---------------------------#
# set.seed(10)
# index = sample(1:nrow(bayes_train),0.1*nrow(bayes_train)) #Temp
# bayes_train=bayes_train[index,]
# bayes_train_labels=bayes_train_labels[index]

#---------------------- Tune and train model ---------------------------#
tune.control = tune.control(sampling="cross",
                            sampling.aggregate=mean, 
                            cross=10)

obj = tune(naiveBayes, 
           outcome.response~.,
           data = bayes_train,
           ranges = list(laplace = 0:2),
           tunecontrol = tune.control)
#Shuffled, 10-CV: laplace = 1; class error: 0.120899 
#Unshuffled, 10-CV: laplace = 1; class error: 0.1071897 

best.nb = obj$best.model
bayes_test_pred = predict(best.nb, bayes_test[,1:40])
performance(bayes_test_pred, bayes_test[,41])
# Shuffled
# Accuracy:  0.8741516 
# True positive:  0.8755515 
# False negative:  0.1271542 
# prediction
# truth     0     1
# 0 10180  1354
# 1  1483  9526

# Unshuffled
# Accuracy:  0.7779799 
# True positive:  0.6702252 
# False negative:  0.07960865 
# prediction
# truth    0    1
# 0 8937 4232
# 1  773 8601

#---------------------- Naive Bayes using e1071 without CV---------------------------#
# #Train model
# bayes_classifier = naiveBayes(bayes_train, bayes_train_labels)
# bayes_classifier
# 
# #Performance on train
# bayes_train_pred = predict(bayes_classifier, bayes_train)
# performance(bayes_train_pred, bayes_train_labels)
# 
# #Performance on test
# bayes_test_pred = predict(bayes_classifier, bayes_test)
# performance(bayes_test_pred, bayes_test_labels)


#---------------------- Naive Bayes using caret ---------------------------#
# # train a naive bayes model
# train_control <- trainControl(method="cv", number=3)
# model <- train(outcome.response~., 
#                data=bayes_train,
#                trControl=train_control,
#                method="nb")
# print(model)
# # make predictions
# predictions <- predict(model, bayes_test[,-41])
# confusionMatrix(predictions, bayes_test[,41])
