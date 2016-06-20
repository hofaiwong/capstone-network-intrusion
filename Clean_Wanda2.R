#################################################
####       Network Intrusion Detection       ####
####           Data Load and Prep            ####
#################################################

library(nnet)

#Load csv files
FieldNames <-read.csv("./data/Field Names.csv", header = FALSE,
                      stringsAsFactors = FALSE)
column.names <- FieldNames[,1] #41 columns 

KDD.train <-read.csv("./data/KDDTrain+.csv", header = FALSE,
                     stringsAsFactors = FALSE)

KDD.test <-read.csv("./data/KDDTest+.csv", header = FALSE,
                    stringsAsFactors = FALSE)

#Function to prep, munge and dummify train and test data
prep = function(df) {
  colnames(df) <- column.names #Rename columns
  names(df)[42] <- "outcome"
  df$outcome <- as.factor(df$outcome)
  df$outcome.response <- ifelse(df$outcome == 'normal',0,1)
  
  #Dealing with 3 Categorical Variables, 0/1, expanding ncols, replace into new.KDD.*
  service_<-as.data.frame(class.ind(df$service))
  protocol_type_<-as.data.frame(class.ind(df$protocol_type))
  flag_<-as.data.frame(class.ind(df$flag))
  new <- cbind(service_, protocol_type_, flag_)
  cat('Dummy features:',dim(new)[2],'\n')
  new.df = cbind(duration=df$duration, new, df[,5:41], outcome.response=df[,44])
  cat('New dim:',dim(new.df))
  return(new.df)
}

#Run function on train
new.KDD.train = prep(KDD.train) #84 dummy features, new dim: 125973 123
mean(new.KDD.train$outcome.response==1) #46.5% malicious connections
View(new.KDD.train)

#Run function on test
new.KDD.test = prep(KDD.test) #77 dummy features, new dim: 22543 116
mean(new.KDD.test$outcome.response==1) #56.9% malicious connections
View(new.KDD.test)

#Comparing columns in test and train
a = sapply(colnames(new.KDD.test), function(i) ifelse(i %in% colnames(new.KDD.train), TRUE, FALSE))
which(a==FALSE)
b = sapply(colnames(new.KDD.train), function(i) ifelse(i %in% colnames(new.KDD.test), TRUE, FALSE))
which(b==FALSE)

#Add missing levels in test, set to 0
for (i in names(b[b==FALSE])) {
  new.KDD.test[,i] = 0
}
