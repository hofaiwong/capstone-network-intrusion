#################################################
####       Network Intrusion Detection       ####
####                  SVM                    ####
#################################################

library(e1071)

#Define accuracy function
accuracy = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat(sum(diag(contingency))/sum(contingency),'\n')
  return(contingency)
}

#Scaling
#new.KDD.train.scale = as.data.frame(scale(new.KDD.train[, -123]))

#Creating training and test sets
set.seed(0)
train.index = sample(1:nrow(new.KDD.train), 7*nrow(new.KDD.train)/10)
train = new.KDD.train[train.index,]
test = new.KDD.train[-train.index,]


#######################
#### Linear Kernel ####
#######################

#CV for tuning
set.seed(0)
cv.svc.linear = tune(svm,
                     outcome.response ~ .,
                     data = train,
                     kernel = "linear",
                     ranges = list(cost = 10^(seq(-2, -.2, length = 10))))
cv.svc.linear

plot(cv.svc.linear$performances$cost,
     cv.svc.linear$performances$error,
     xlab = "Cost",
     ylab = "Error Rate",
     type = "l")

#Best cost ~ 
#Error rate ~ 
#Stabilized?

#Summary of best model
summary(cv.svc.linear$best.model) #X support vectors.

#Best model accuracy
best.svc.linear.pred = predict(cv.svc.linear$best.model, test)
accuracy(test$outcome.response, best.svc.linear.pred) #X% accuracy

#Constructing the final model.
final.svc.linear = svm(outcome.response ~ .,
                       data = new.KDD.train,
                       kernel = "linear",
                       cost = cv.svc.linear$best.model$cost)
summary(final.svc.linear)
final.svc.linear$index
final.svc.linear.pred = predict(final.svc.linear, new.KDD.train)
accuracy(new.KDD.train[,'outcome.response'], final.svc.linear.pred) #X% accuracy


#######################
#### Radial Kernel ####
#######################

#CV for tuning
set.seed(0)
cv.svm.radial = tune(svm,
                     outcome.response ~ .,
                     data = train,
                     kernel = "radial",
                     ranges = list(cost = seq(.75, 1.25, length = 5),
                                   gamma = seq(.55, .95, length = 5)))
cv.svm.radial

library(rgl)
plot3d(cv.svm.radial$performances$cost,
       cv.svm.radial$performances$gamma,
       cv.svm.radial$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Best cost ~
#Best gamma ~
#Error rate ~
#Stabilized?

#Summary of best model
summary(cv.svm.radial$best.model)

#Best model accuracy
best.svm.radial.pred = predict(cv.svm.radial$best.model, test)
accuracy(test$outcome.response, best.svm.radial.pred) #X% accuracy

#Constructing and visualizing the final model.
final.svm.radial = svm(outcome.response ~ .,
                       data = new.KDD.train,
                       kernel = "radial",
                       cost = cv.svm.radial$best.model$cost,
                       gamma = cv.svm.radial$best.model$gamma)
summary(final.svm.radial)
final.svm.radial$index
final.svm.radial.pred = predict(final.svm.radial, new.KDD.train)
accuracy(new.KDD.train[,'outcome.response'], final.svm.radial.pred) #X% accuracy
