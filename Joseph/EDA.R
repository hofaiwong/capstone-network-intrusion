#EDA by PCA
##########Reference##########################################################################
#Based on A Novel Anomaly Detection Scheme Based on Principle Component Classifier
#NRL Release Number 03-1221.1-2312
#Authors: Mei-Ling Shyu, Shu-Ching Chen, Kanoksri Sarinnapakorn, and LiWu Chang
##############################################################################################
setwd("C:/Users/Joseph/Desktop/NY/Final_project")
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

df$is_host_login<-(sapply(as.numeric(df$is_host_login), function(x) ifelse(x > 0, 1, -1)))#??
df$is_guest_login<-(sapply(as.numeric(df$is_guest_login), function(x) ifelse(x > 0, 1, -1)))#??
df$Land<-(sapply(as.numeric(df$Land), function(x) ifelse(x > 0, 1, -1)))
df$logged_in<-(sapply(as.numeric(df$logged_in), function(x) ifelse(x > 0, 1, -1)))
df$root_shell<-(sapply(as.numeric(df$root_shell), function(x) ifelse(x > 0, 1, -1)))
df$attack<-(sapply(df$attack, function(x) ifelse(x == 'normal', 1, -1)))

df<-df[-c(2:4)]
df_response<-df$attack
df<-subset(df, select = -c(Land,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_train<-subset(df, attack==1)#Select normal network connection as the training set
ave_train=sapply(df_train[,-32],mean)#The same mean will be used for standarlization for test set
sd_train=sapply(df_train[,-32],sd)#The same standard deviation will be used for test set
df_train[,c(1:31)]=sapply(df_train[,c(1:31)],Standard_self)
df_train<-subset(df_train,select=-c(attack))

fa.parallel(cor(df_train), #The data in question.
                        n.obs = 31, #Since we supplied a covaraince matrix, need to know n.
                        fa = "pc", #Display the eigenvalues for PCA.
                        n.iter = 100)
            pc_bodies = principal(cor(df_train), #The data in question.
                                  nfactors = 2, #The number of PCs to extract.
                                  rotate = "none")
            factor.plot(pc_bodies)
            
df_train.pca <- prcomp(df_train,center = FALSE,scale. = FALSE)
library(FactoMineR)
names(df_train)=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31')
result_train <- PCA(df_train) 

#df_train.pca <- prcomp(df_train,center = TRUE,scale. = TRUE) 
plot(df_train.pca, type = "l")
log=summary(df_train.pca)
ev <- (df_train.pca$sdev)^2

############################################Prediction of Test Set###########################################################
df_test<-subset(df, attack==-1)
df_test<-subset(df_test,select=-c(attack))
#Standardization
df_test[,c(1:31)]=sapply(df_test[,c(1:31)],Standard_self)
names(df_test)=names(df_train)
result_test <- PCA(df_test) 


df_true_test_n<-subset(df_true_test, attack==1)
df_true_test_n<-df_true_test_n[,-c(2:4)]

df_true_test_n<-subset(df_true_test_n, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
#df_true_test_n<-subset(df_true_test_n,select=-c(Urgent,num_shells))
#df_true_test_a<-subset(df_true_test_a, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test_n<-subset(df_true_test_n,select=-c(Urgent,num_shells))
#df_true_test<-subset(df_true_test, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test_n[,c(1:29)]=sapply(df_true_test_n[,c(1:29)],Standard_self)

names(df_true_test_n)=names(df_train)=c('1','2','3','5','6','7','8','9','10','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31')
result_true_test_n <- PCA(df_true_test_n) 



df_true_test_a<-subset(df_true_test, attack==-1)
df_true_test_a<-df_true_test_a[,-c(2:4)]

df_true_test_a<-subset(df_true_test_a, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test_a<-subset(df_true_test_a,select=-c(Urgent,num_shells))
#df_true_test_a<-subset(df_true_test_a, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))

#df_true_test<-subset(df_true_test, select = -c(Land,attack,is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df_true_test_a[,c(1:29)]=sapply(df_true_test_a[,c(1:29)],Standard_self)
names(df_true_test_a)=names(df_train)=c('1','2','3','5','6','7','8','9','10','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31')
result_true_test_a <- PCA(df_true_test_a) 






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









