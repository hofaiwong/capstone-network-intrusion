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
colnames(KDD.train) <- column.names #Rename columns

KDD.test <-read.csv("./data/KDDTest+.csv", header = FALSE,
                    stringsAsFactors = FALSE)
colnames(KDD.test) <- column.names #Rename columns

KDD.shuffle = rbind(KDD.train, KDD.test)
set.seed(0)
shuffle.train = sample(1:nrow(KDD.shuffle), nrow(KDD.train))
new.KDD.train = KDD.shuffle[shuffle.train, ]
new.KDD.test = KDD.shuffle[-shuffle.train, ]

#Function to prep, munge and dummify train and test data
prep = function(df) {
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
new.KDD.train = prep(new.KDD.train) #84 dummy features, new dim: 125973 123
mean(new.KDD.train$outcome.response==1) #46.5% malicious connections

#Run function on test
new.KDD.test = prep(new.KDD.test) #77 dummy features, new dim: 22543 116
mean(new.KDD.test$outcome.response==1) #56.9% malicious connections

#Comparing columns in test and train
a = sapply(colnames(new.KDD.test), function(i) ifelse(i %in% colnames(new.KDD.train), TRUE, FALSE))
which(a==FALSE)
b = sapply(colnames(new.KDD.train), function(i) ifelse(i %in% colnames(new.KDD.test), TRUE, FALSE))
which(b==FALSE)

#Remove columns with only one value: num_outbound_cmds in train and test
for (i in 1:dim(new.KDD.test)[2]) {
  if (length(unique(new.KDD.test[,i])) < 2) {
    cat(names(new.KDD.test)[i],'\n')
  }
}
new.KDD.train = subset(new.KDD.train, select=-c(num_outbound_cmds))
new.KDD.test = subset(new.KDD.test, select=-c(num_outbound_cmds))

#Group missing levels in train and add grouped column in test
new.KDD.train$grp.lvl = 0
for (i in names(b[b==FALSE])) {
  for (j in 1:nrow(new.KDD.train)){
    if (new.KDD.train[j,i]==1) {
      new.KDD.train$grp.lvl[j]=1  
    }
  }
}
new.KDD.train = subset(new.KDD.train, select=-which(b==FALSE))
new.KDD.test$grp.lvl = 0

# #Add missing levels in test, set to 0, and reorder to match train
# for (i in names(b[b==FALSE])) {
#   new.KDD.test[,i] = 0
# }
# new.KDD.test = new.KDD.test[colnames(new.KDD.train)]

#Check if columns match between train and test
names(new.KDD.test)==names(new.KDD.train)

rm(a, b, i)
