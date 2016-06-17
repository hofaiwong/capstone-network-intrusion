#EDA
#logistic
#Ridge
#K-means clustering
setwd("~/Documents/Network_Fraud/")
FieldNames <-read.csv("Field Names.csv", header = FALSE,
                      stringsAsFactors = FALSE)

KDD.test <-read.csv("KDDTest+.csv", header = FALSE,
                    stringsAsFactors = FALSE)

KDD.train <-read.csv("KDDTrain+.csv", header = FALSE,
                     stringsAsFactors = FALSE)

column.names <- FieldNames[,1] #41 columns 
colnames(KDD.test) <- column.names # rename columns
colnames(KDD.train)<- column.names
colnames(KDD.train)[42] <- 'outcome'
KDD.train$outcome <- as.factor(KDD.train$outcome)

KDD.train$outcome.response <- ifelse(KDD.train$outcome == 'normal',0,1)

View(KDD.train) #44 cols  0.465% are malicious 
View(KDD.test)
#Dealing with 3 Categorical Variables, 0/1, expanding ncols, replace into new.KDD.train
library(nnet)
service_<-as.data.frame(class.ind(KDD.train$service))
protocol_type_<-as.data.frame(class.ind(KDD.train$protocol_type))
flag_<-as.data.frame(class.ind(KDD.train$flag))
new_ <- cbind(service_, protocol_type_, flag_) #84
new.KDD.train <-cbind(duration=KDD.train$duration, new_, KDD.train[,5:41], outcome.response=KDD.train[,44])
dim(new.KDD.train) #[1] 125973    123
View(new.KDD.train)

#K-means cluster analysis

#logistic - youtuber took the col 2 to 4 out. I could try that? 
#amy- logistic, ridge
amy <-glm(formula = outcome.respond ~ ., family = "binomial", data = new) # categorial variables split 0,1 against y outcome.respond
summary(amy)


