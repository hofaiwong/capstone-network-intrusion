#################################################
####       Network Intrusion Detection       ####
#### Logistic Regression with Lasso - GLMNET ####
#################################################

library(glmnet)
library(car)
library(MASS)

###################
#### Functions ####
###################

#Define standardization function for train
Standard_self<-function(data) {
  data=(data-mean(data))/sd(data)
  return (data)
}

#Define standardization function for test
Standard<-function(data,ave,sd) {
  data=(data-ave)/sd
  return (data)
}

#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive: ',contingency[2,2]/sum(contingency[,2]),'\n')
  cat('False negative: ',contingency[2,1]/sum(contingency[,1]),'\n')
  return(contingency)
}



#########################
#### Scale and split ####
#########################

#Finding binary columns to exclude from scaling
bin.col = data.frame(matrix(ncol = 4, nrow = dim(new.KDD.train)[2]-1))
colnames(bin.col) = c('col','unique','max','min')
for (i in 1:(dim(new.KDD.train)[2]-1)) {
  bin.col[i,1]=i
  bin.col[i,2]=length(unique(new.KDD.train[,i]))
  bin.col[i,3]=max(new.KDD.train[,i])
  bin.col[i,4]=min(new.KDD.train[,i])
}
#bin.col[bin.col$unique==2,]
bin.col = bin.col[bin.col$unique==2,1]

#Scaling non-binary columns in train
new.KDD.train.scaled = sapply(new.KDD.train[,-c(bin.col,122)], Standard_self)
new.KDD.train.scaled = cbind(new.KDD.train[,c(bin.col,122)], new.KDD.train.scaled)
new.KDD.train.scaled = new.KDD.train.scaled[,colnames(new.KDD.train)]

#Scaling non-binary columns in test
ave = sapply(new.KDD.train[,-c(bin.col,122)], mean)
sd = sapply(new.KDD.train[,-c(bin.col,122)], sd)
new.KDD.test.scaled = sapply(new.KDD.test[,-c(bin.col,122)], function(x) Standard(x,ave,sd))
new.KDD.test.scaled = cbind(new.KDD.test[,c(bin.col,122)], new.KDD.test.scaled)
new.KDD.test.scaled = new.KDD.test.scaled[,colnames(new.KDD.test)]


#####################
#### Logit Lasso ####
#####################

#Creating the data matrices for the glmnet() function.
x.train = model.matrix(outcome.response ~ ., new.KDD.train.scaled)[, -1]
y.train = new.KDD.train.scaled$outcome.response
x.test = model.matrix(outcome.response ~ ., new.KDD.test.scaled)[, -1]
y.test = new.KDD.test.scaled$outcome.response

#Creating training and test sets
set.seed(0)
train = sample(1:nrow(x.train), 7*nrow(x.train)/10)

#Fitting the logistic regression on a grid of lambda. Alpha = 1 for lasso penalty
grid = 10^seq(0, -5, length = 200)
logit.models = glmnet(x.train[train, ], y.train[train],
                      alpha = 1,
                      lambda = grid,
                      family="binomial")
plot(logit.models,
     xvar = "lambda",
     label = TRUE,
     main = "Logistic Regression with Lasso penalty\n")


#Cross-validation
set.seed(0)
logit.cv = cv.glmnet(x.train[train, ], y.train[train], 
                     alpha = 1,            #Lasso penalty
                     nfolds = 10,          #k-fold CV
                     type.measure='class', #Misclassification measure
                     family="binomial",    #Logistic regression
                     lambda = grid)
plot(logit.cv, main = "Logistic Regression with Lasso penalty\n")


#Best lambda
logit.cv$lambda.min #1.122668e-05 with k=5; 1.189534e-05 with k=10; 9.011018e-05 with k=10 and shuffled data
log(logit.cv$lambda.min) #-11.39722 with k=5; -11.33936 with k=10; -9.314477 with k=10 and shuffled data
lambda = exp(-4) #lamdba.min still keeps ~100 features. Need to balance complexity and accuracy


#Checking performance of model - train data, test subset
logit.test.class = predict(logit.cv, 
                           s = lambda, 
                           type = 'class',
                           newx = x.train[-train, ])
performance(y.train[-train], logit.test.class) 
#94.5% accuracy with lambda=exp(-3); 97.6% with lambda.min
#lambda=exp(-3): 94.5% (seed=0), k=5
#lambda.min: 97.6% (seed=0), k=5
#lambda=exp(-3): 0.9451471 (seed=100), k=5
#lambda=exp(-3): 0.9445914 (seed=30), k=5
#lambda=exp(-3): 0.9447767 (seed=0), k=10
#Shuffled, exp(-4), k=10: 0.9402784 

#Checking performance of model - test data
logit.test.class.final = predict(logit.cv, 
                                 s = lambda, 
                                 type = 'class',
                                 newx = x.test)
performance(y.test, logit.test.class.final) 
#69.8% accuracy with lambda=exp(-3)
#lambda=exp(-3): 69.8% (seed=0), k=5
#lambda.min: 
#lambda=exp(-3): 0.6972896% (seed=100), k=5
#lambda=exp(-3): 0.6970235 (seed=30), k=5
#lambda=exp(-3): 0.6978219 (seed=0), k=10
#Shuffled, exp(-4), k=10: 0.868651 

#Coefficients: 
logit.coef = predict(logit.cv, 
                     s = lambda, 
                     type = 'coefficients')
sum(logit.coef!=0)-1 #Keep ~110 features with lambda.min, 19 with lambda=exp(-4)
logit.coef
logit.nonzero = predict(logit.cv, 
                        s = lambda, 
                        type = 'nonzero')
colnames(x.train)[logit.nonzero[,1]]

#############################
#### Performance Summary ####
#############################

#Plot lambda vs coefficients vs accuracy
summary.plot = function(x,y) {
  res = data.frame(matrix(ncol = 3, nrow = length(grid)))
  colnames(res) = c('lambda','accuracy','coef')
  for (i in 1:length(grid)) {
    #Insert lambda
    res[i,1] = grid[i]
    
    #Insert accuracy %
    pred.class = predict(logit.cv, s = grid[i], type = 'class', newx = x)
    t = table(truth = y, prediction = pred.class)
    res[i,2] = sum(diag(t))/sum(t)
    
    #Insert coef count
    pred.coef = predict(logit.cv, s = grid[i], type = 'coefficients')
    res[i,3] = sum(pred.coef!=0)-1
  }
  plot(res$coef, res$accuracy, main = "Model accuracy by number of features",
       xlab="Count of features", ylab="Accuracy", pch=16)
  return(res)
}
summary.plot(x.train[-train,], y.train[-train])
summary.plot(x.test, y.test)

#####################
#### Diagnostics ####
#####################

#formula = as.formula(paste("outcome.response ~", paste(colnames(x)[logit.nonzero[,1]], collapse = " + ")))
model.var = colnames(x.train)[logit.nonzero[,1]]
formula = as.formula(paste("outcome.response ~", paste(model.var, collapse = " + ")))
logit.glm = glm(formula, 
                family = "binomial", 
                data = new.KDD.train.scaled)
summary(logit.glm) #"wrong_fragment" and "dst_host_serror_rate" are not significant

model.var = model.var[-c(11,18)] #remove the 2 insignificant features
formula = as.formula(paste("outcome.response ~", paste(model.var, collapse = " + ")))
logit.glm = glm(formula, 
                family = "binomial", 
                data = new.KDD.train.scaled)
summary(logit.glm) #all coefficients are significant

#Goodness of fit test i.e. Test of deviance
pchisq(logit.glm$deviance, logit.glm$df.residual, lower.tail = FALSE) #p-value of 1 > 0.05 cutoff so we fail to reject null hypothesis; model is appropriate

#McFadden's pseudo R^2 based on the deviance
1 - logit.glm$deviance/logit.glm$null.deviance #0.8584628 variance explained

#Variance Inflation Factor
vif(logit.glm) #all < 5, can consider no multicollinearity to deal with

#Checking performance of prediction
logit.pred = round(logit.glm$fitted.values)
performance(new.KDD.train$outcome.response, logit.pred)

#Checking the model summary and assumptions
plot(logit.glm)
influencePlot(logit.glm)
avPlots(logit.glm)
confint(logit.glm)