
#Principle Component Classifier
##########Reference##########################################################################
#Based on A Novel Anomaly Detection Scheme Based on Principle Component Classifier
#NRL Release Number 03-1221.1-2312
#Authors: Mei-Ling Shyu, Shu-Ching Chen, Kanoksri Sarinnapakorn, and LiWu Chang
##############################################################################################
setwd("C:/Users/Joseph/Desktop/NY/Final_project")
#setwd("C:/Users/Joseph/Desktop/NY/Capestone_fraud/R_CODES")
#df = read.table("C:/Users/Joseph/Desktop/NY/Capestone_fraud/R_CODES/training_fraud.txt", header = TRUE)
alpha=10
alpha_max=2.5*exp(3) #40 1:3 / 4:31     | 40 1:3/25:31
alpha_min=0.25*exp(5)   #exp(8)1:3 /4:31 | 40.5*exp(7)*1:3/25:31
df = read.table("C:/Users/Joseph/Desktop/NY/Final_project/training_fraud.txt", header = TRUE)
df_true_test=read.table("C:/Users/Joseph/Desktop/NY/Final_project/test.txt")
df_true_test$V42=as.numeric(df_true_test$V42) 
for (i in 1:nrow(df_true_test))
{
  if (df_true_test$V42[i]==17)#17 is the normal by attack type through inspection of original data type
  {df_true_test$V42[i]=1}
  else
  {df_true_test$V42[i]=-1}
}

colnames(df_true_test)=colnames(df)
library(psych)
library(caret)
library(nnet)
library(RCurl)
library(Metrics)

Standard_self<-function(data) {
  data=(data-mean(data))/sd(data)
  return (data)
}

Standard_self1<-function(data) {
  data=(data-mean(data))/(max(data)-min(data))
  return (data)
}

Standard<-function(data,ave,sd) {
  data=(data-ave)/sd
  return (data)
}

#Standard(dfTestNew[ ,c(2:42)],ME_train[1:41],SD_train[1:41])



df$is_host_login<-(sapply(as.numeric(df$is_host_login), function(x) ifelse(x > 0, 1, -1)))#??
df$is_guest_login<-(sapply(as.numeric(df$is_guest_login), function(x) ifelse(x > 0, 1, -1)))#??
df$Land<-(sapply(as.numeric(df$Land), function(x) ifelse(x > 0, 1, -1)))
df$logged_in<-(sapply(as.numeric(df$logged_in), function(x) ifelse(x > 0, 1, -1)))
df$root_shell<-(sapply(as.numeric(df$root_shell), function(x) ifelse(x > 0, 1, -1)))
df$attack<-(sapply(df$attack, function(x) ifelse(x == 'normal', 1, -1)))

#id_Service<-as.data.frame(class.ind(df$Service))
#id_Service<-sapply(id_Service, function(x) ifelse(x > 0, 1, -1))
#id_Protocal<-as.data.frame(class.ind(df$protocal_type))
#id_Protocal<-sapply(id_Protocal, function(x) ifelse(x > 0, 1, -1))
#id_Flag<-as.data.frame(class.ind(df$Flag))
#id_Flag<-sapply(id_Flag, function(x) ifelse(x > 0, 1, -1))
df<-df[-c(2:4)]
#df<-cbind(id_Protocal,id_Service,id_Flag,df)
#df<- subset(df, select = -c(num_outbound_cmds))
df_response<-df$attack
df<-subset(df, select = -c(Land,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_train<-subset(df, attack==1)#Select normal network connection as the training set
ave_train=sapply(df_train[,-32],mean)#The same mean will be used for standarlization for test set
sd_train=sapply(df_train[,-32],sd)#The same standard deviation will be used for test set
df_train[,c(1:31)]=sapply(df_train[,c(1:31)],Standard_self)

df_train<-subset(df_train,select=-c(attack))
#df<-subset(df, select = -c(attack))


df_train.pca <- prcomp(df_train,center = FALSE,scale. = FALSE) 
#df_train.pca <- prcomp(df_train,center = TRUE,scale. = TRUE) 
plot(df_train.pca, type = "l")
log=summary(df_train.pca)
#We observe PC1+PC2 capture 50% of variance and PC6 onward captures the final 20 percent of variance
#This sets the important summation bounds for Principle Component Classifier
ev <- (df_train.pca$sdev)^2
#loadings=df.pca$rotation #[ROW=PREDICTORS.COL=PCs]
#Here we will construct our PCC in function
#pcsamples=scale(df_train, df_train.pca$center, df_train.pca$scale) %*% df_train.pca$rotation 
pcsamples<-predict(df_train.pca,df_train)
response<-as.vector(1:nrow(df_train))
distance_large<-as.vector(1:nrow(df_train))
pcdistance_small<-as.vector(1:nrow(df_train))
summ1=0.0
summ2=0.0
for (i in 1:length(response))
{
  summ1=0.0
  for (j in 1:5)
  {summ1=summ1+pcsamples[i,j]^2/ev[j]}
  distance_large[i]=summ1
} 
hist(log(distance_large))
#hist((distance_large),breaks=1000,probability=TRUE)

for (i in 1:length(response))
{
  summ2=0.0
  for (j in 15:31)
  {summ2=summ2+pcsamples[i,j]^2/ev[j]}
  pcdistance_small[i]=summ2
} 
hist(log(pcdistance_small))
#hist(log(pcdistance_small),breaks=1000,probability=TRUE)

for (i in 1:length(distance_large))
{ 
  if ( distance_large[i] > alpha_max || pcdistance_small[i] > alpha_min)
    
  {response[i]=-1.0}
  
  else
  {response[i]=1.0}
}


print(summary(as.factor(response)))
############################################Prediction of Test Set###########################################################
df_test<-subset(df, attack==-1)
df_test<-subset(df_test,select=-c(attack))

#Standardization
for (i in 1:31)
{
  df_test[,i]=Standard(df_test[,i],ave_train[i],sd_train[i])
}

#Standard(dfTestNew[ ,c(1:32)],ave_train[1:32],sd_train[1:32])

pcsamples_test<-predict(df_train.pca,df_test)
#pcsamples_test=scale(df_test, df_train.pca$center, df_train.pca$scale) %*% df_train.pca$rotation 
response_test<-as.vector(1:nrow(df_test))
distance_large_test<-as.vector(1:nrow(df_test))
pcdistance_small_test<-as.vector(1:nrow(df_test))
summ1=0.0
summ2=0.0
for (i in 1:length(response_test))
{
  summ1=0.0
  for (j in 1:5)
  {summ1=summ1+pcsamples_test[i,j]^2/ev[j]}
  distance_large_test[i]=summ1
} 
hist(log(distance_large_test))
#hist((distance_large),breaks=1000,probability=TRUE)

for (i in 1:length(response_test))
{
  summ2=0.0
  for (j in 15:31)
  {summ2=summ2+pcsamples_test[i,j]^2/ev[j]}
  pcdistance_small_test[i]=summ2
} 
hist(log(pcdistance_small_test))
#hist(log(pcdistance_small_test),breaks=1000,probability=TRUE)

for (i in 1:length(response_test))
{ 
  if ( distance_large_test[i] > alpha_max || pcdistance_small_test[i] > alpha_min)
  {response_test[i]=-1.0}
  
  else
  {response_test[i]=1.0}
}

print(summary(as.factor(response_test)))
#print(confusionMatrix(Presponse_true_test, df_true_test_response))
#########################################True Test Set Prediction###############################################

#FieldNames <-read.csv("C:/Users/Joseph/Desktop/NY/capstone-network-intrusion/data/Field Names.csv", header = FALSE,
#                      stringsAsFactors = FALSE)
#column.names <- FieldNames[,1] #41 columns 

#KDD.train <-read.csv("C:/Users/Joseph/Desktop/NY/capstone-network-intrusion/data/KDDTrain+.csv", header = FALSE,
#                     stringsAsFactors = FALSE)
#colnames(KDD.train) <- column.names #Rename columns

#KDD.test <-read.csv("C:/Users/Joseph/Desktop/NY/capstone-network-intrusion/data/KDDTest+.csv", header = FALSE,
 #                   stringsAsFactors = FALSE)
#colnames(KDD.test) <- column.names #Rename columns

#Combine train+test, shuffle and split
#KDD.shuffle = rbind(KDD.train, KDD.test)
#set.seed(0)
#shuffle.train = sample(1:nrow(KDD.shuffle), nrow(KDD.train))
#new.KDD.train.shuffle = KDD.shuffle[shuffle.train, ]
#new.KDD.train.shuffle=subset(new.KDD.train.shuffle,select = -c(43))
#colnames(new.KDD.train.shuffle)[42]='attack'
#new.KDD.test.shuffle = KDD.shuffle[-shuffle.train,]
#new.KDD.test.shuffle=subset(new.KDD.test.shuffle,select=-c(43))
#colnames(new.KDD.test.shuffle)[42]='attack'
#df_true_test=new.KDD.test.shuffle


#df_true_test$is_host_login<-(sapply(as.numeric(df_true_test$is_host_login), function(x) ifelse(x > 0, 1, -1)))#??
#df_true_test$is_guest_login<-(sapply(as.numeric(df_true_test$is_guest_login), function(x) ifelse(x > 0, 1, -1)))#??
#df_true_test$land<-(sapply(as.numeric(df_true_test$land), function(x) ifelse(x > 0, 1, -1)))
#df_true_test$logged_in<-(sapply(as.numeric(df_true_test$logged_in), function(x) ifelse(x > 0, 1, -1)))
#df_true_test$root_shell<-(sapply(as.numeric(df_true_test$root_shell), function(x) ifelse(x > 0, 1, -1)))
#df_true_test$attack<-(sapply(df_true_test$attack, function(x) ifelse(x == 'normal', 1, -1)))





df_true_test_a<-subset(df_true_test, attack==-1)
df_true_test<-df_true_test[-c(2:4)]
df_true_test_a<--df_true_test_a[-c(2:4)]


df_true_test_response<-df_true_test$attack
#df_true_test<-subset(df_true_test, select = -c(land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test<-subset(df_true_test, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test_a<-subset(df_true_test_a, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))



for (i in 1:31)
{
  df_true_test[,i]=Standard(df_true_test[,i],ave_train[i],sd_train[i])
}

#pcsamples_true_test=scale(df_true_test, df_train.pca$center, df_train.pca$scale) %*% df_train.pca$rotation
names(df_true_test)=names(df_test)
pcsamples_true_test<-predict(df_train.pca,df_true_test)

Presponse_true_test<-as.vector(1:nrow(df_true_test))
distance_large_true_test<-as.vector(1:nrow(df_true_test))
pcdistance_small_true_test<-as.vector(1:nrow(df_true_test))


summ1=0.0
summ2=0.0
for (i in 1:length(Presponse_true_test))
{
  summ1=0.0
  for (j in 1:5)
  {summ1=summ1+pcsamples_true_test[i,j]^2/ev[j]}
  distance_large_true_test[i]=summ1
} 
hist(log(distance_large_true_test))
#hist((distance_large),breaks=1000,probability=TRUE)

for (i in 1:length(Presponse_true_test))
{
  summ2=0.0
  for (j in 15:31)
  {summ2=summ2+pcsamples_true_test[i,j]^2/ev[j]}
  pcdistance_small_true_test[i]=summ2
} 
hist(log(pcdistance_small_true_test))
#hist(log(pcdistance_small),breaks=1000,probability=TRUE)
#summary(Presponse_true_test)
for (i in 1:length(Presponse_true_test))
{ 
if ( distance_large_true_test[i] > alpha_max || pcdistance_small_true_test[i] > alpha_min )
  {Presponse_true_test[i]=-1.0
  }
  
  else
  {Presponse_true_test[i]=1.0}
}

print(confusionMatrix(Presponse_true_test, df_true_test_response))



#Diagnostic for correlation by PCA for test set and real_test_set 
#df_test<-subset(df, attack==-1)
#df_test<-subset(df_test,select=-c(attack))
#fa.parallel(cor(df_test), #The data in question.
#            n.obs = 31, #Since we supplied a covaraince matrix, need to know n.
#            fa = "pc", #Display the eigenvalues for PCA.
#            n.iter = 100)
#pc_bodies = principal(cor(df_test), #The data in question.
#                      nfactors = 2, #The number of PCs to extract.
#                      rotate = "none")
#pc_bodies

#df_test.pca <- prcomp(df_test,center = FALSE,scale. = FALSE) 
#abline(h = 1) #Adding a horizontal line at 1.
#df_true_test<-subset(df_true_test, attack==-1)

df_true_test_a.pca <- prcomp(df_true_test_a,center = FALSE,scale. = FALSE) 









#PCC<-function(ev,model,data_to_predict,p,q,c) {
#Input vector ev: eigenvalues for principle eigen-direction 
#Input data_to_predict: samples of data to predict
#Input model from data_training: samples of data for training
#Input integer p; the number of the first p componets to use as a dominant PCC which captures 
#larger than 50 percent of variance 
#Input c threshold: tuned to the value so that 0.9899 of empirical distribution of principle component distance 
#is contained  
#Input integer q: the qth PC onwards where 20% of variance is contained   
#Output binary response vector in numerics  

#Cpcdistance_large=as.vector(1:nrow(data_to_predict))
#pcdistance_small=as.vector(1:nrow(data_to_predict))
#response=as.vector(1:nrow(data_to_predict))
#pcsamples=predict(model,data_to_predict)
#summ1=0.0
#summ2=0.0
#for (i in 1:length(response))
#  {
#   summ1=0.0
#   for (j in 1:p)
#   {summ1=summ1+pcsamples[i,j]^2/ev[j]}
#   Cpcdistance_large[i]=summ1
#} 
#hist((Cpcdistance_large),breaks=1000,probability=TRUE)

#for (i in 1:length(response))
#{ summ2=0.0
#  for (j in q:dim(data_to_predict)[2])
#  {summ2=summ2+pcsamples[i,j]^2/ev[j]}
#  pcdistance_small[i]=summ2
#} 
#print(pcdistance_large)
#hist(log(pcdistance_small),breaks=1000,probability=TRUE)
#print(pcdistance_large)
#print(c)
#for (i in 1:length(response))
#{ 
# if ( Cpcdistance_large[i]-c > 0)
# {Cpcdistance_large[i]<-1}
#}
#  else
#    {pcdistance_large[i]=-1}
#}
#print(pcdistance_large)
#pcdistance_large(ifelse(pcdistance_large> 3.5, 1, -1))#1 :attack -1:nonattack
#print(summary(pcdistance_large))

#pcdistance_small=sapply(pcdistance_small, function(x) ifelse(x > 0, 1, -1))
 #for (i in 1:length(response))
# {
#   if(Cpcdistance_large[i]==1 & pcdistance_small[i]==1)
#   {response[i]=1}
#   else
#   {response[i]=-1}
   
# }
#return (response)
#}
#debug(PCC)
#results=PCC(ev,df.pca,df,2,6,4.0)
#summary(results)






