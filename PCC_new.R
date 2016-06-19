setwd("C:/Users/Joseph/Desktop/NY/Capestone_fraud/R_CODES")

require(psych)
require(caret)
require(nnet)


#Principle Component Classifier
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


PCC<-function(ev,model,data_to_predict,p,q,c) {
  #Input vector ev: eigenvalues for principle eigen-direction 
  #Input data_to_predict: samples of data to predict
  #Input model from data_training: samples of data for training
  #Input integer p; the number of the first p componets to use as a dominant PCC which captures 
  #larger than 50 percent of variance 
  #Input c threshold: tuned to the value so that 0.9899 of empirical distribution of principle component distance 
  #is contained  
  #Input integer q: the qth PC onwards where 20% of variance is contained   
  #Output binary response vector in numerics
  threshold=c
  response=as.numeric(as.vector(1:nrow(data_to_predict)))
  output=as.numeric(as.vector(1:nrow(data_to_predict)))
  distance_1arge=as.numeric(as.vector(1:nrow(data_to_predict)))
  pcsamples=predict(model,data_to_predict)
  for (i in 1:length(response))
  {
     summ1=0.0
    for (j in 1:p){
      summ1=summ1+pcsamples[i,j]^2/ev[j]
      }
     distance_1arge[i]=summ1
  #print(pcdistance_1arge[i])
  
   } 
  
  for (i in 1:length(response))
  { 
    if ( distance_large[i] > threshold)
    {
      distance_small[i]=1
      }
    
    else
    {
      distance_large[i] = -1
      }
  }
  
  
  
  distance_small=as.numeric(as.vector(1:nrow(data_to_predict)))
  
  
  summ1=0.0
  summ2=0.0
  
  #head(pcdistance_1arge)
  #hist((pcdistance_1arge),breaks=1000,probability=TRUE)
  
  for (i in 1:length(response))
  { summ2=0.0
  for (j in q:dim(data_to_predict)[2])
  {summ2=summ2+pcsamples[i,j]^2/ev[j]}
  distance_small[i]=summ2
  } 
  #for (i in 1:length(response))
  #{print(pcdistance_1arge[i])}
  #hist(log(pcdistance_small),breaks=1000,probability=TRUE)
  #print(pcdistance_large)
  #print(c)
  
  for (i in 1:length(response))
  { 
    if ( distance_small[i] > threshold)
     {distance_small[i]=1}
    
    else
     {distance_small[i] = -1}
  }
  
  
  
  
  #print(pcdistance_1arge)
  #pcdistance_1arge(ifelse(pcdistance_1arge> 3.5, 1, -1))#1 :attack -1:nonattack
  #print(summary(pcdistance_1arge))
  
  #pcdistance_small=sapply(pcdistance_small, function(x) ifelse(x > 0, 1, -1))
  for (i in 1:length(response))
  {
    if(distance_1arge[i]==1 & distance_small[i]==1)
    {output[i]=1}
    else
    {output[i]=-1}
    
  }
  return (output)
}











Cdf = read.table("C:/Users/Joseph/Desktop/NY/Capestone_fraud/R_CODES/training_fraud.txt", header = TRUE)

Cdf$is_host_login<-(sapply(as.numeric(Cdf$is_host_login), function(x) ifelse(x > 0, 1, -1)))#??
Cdf$is_guest_login<-(sapply(as.numeric(Cdf$is_guest_login), function(x) ifelse(x > 0, 1, -1)))#??
Cdf$Land<-(sapply(as.numeric(Cdf$Land), function(x) ifelse(x > 0, 1, -1)))
Cdf$logged_in<-(sapply(as.numeric(Cdf$logged_in), function(x) ifelse(x > 0, 1, -1)))
Cdf$root_shell<-(sapply(as.numeric(Cdf$root_shell), function(x) ifelse(x > 0, 1, -1)))
Cdf$attack<-(sapply(Cdf$attack, function(x) ifelse(x == 'normal', 1, -1)))



id_Service<-as.data.frame(class.ind(Cdf$Service))
id_Service<-sapply(id_Service, function(x) ifelse(x > 0, 1, -1))
id_Protocal<-as.data.frame(class.ind(Cdf$protocal_type))
id_Protocal<-sapply(id_Protocal, function(x) ifelse(x > 0, 1, -1))
id_Flag<-as.data.frame(class.ind(Cdf$Flag))
id_Flag<-sapply(id_Flag, function(x) ifelse(x > 0, 1, -1))
Cdf<-Cdf[-c(2:4)]
Cdf<-cbind(id_Protocal,id_Service,id_Flag,Cdf)
Cdf<- subset(Cdf, select = -c(num_outbound_cmds))
Cdftrain_response<-1*Cdf$attack
Cdf<-subset(Cdf, attack==-1)

Cdf<-subset(Cdf, select = -c(attack,urh_i,tftp_u))

#for (j in 1:length(names(df))) {
#  names(df)[j]=j
#}
Cdf[ ,c(1:44,46:51,53:62,64:98,100:length(colnames(Cdf)))]<-as.data.frame(sapply(Cdf[ ,c(1:44,46:51,53:62,64:98,100:length(colnames(Cdf)))],Standard_self1))

#for (j in 1:length(names(df))) {
#  names(df)[j]=j
#}

Cdf<-subset(Cdf, select = -c(45,52,99))


Cdf.pca <- prcomp(Cdf,center = FALSE,scale. = FALSE) 

varPCA=plot(Cdf.pca, type = "l")
log=summary(Cdf.pca)
#We observe PC1+PC2 capture 50% of variance and PC6 onward captures the final 20 percent of variance
#This sets the important summation bounds for Principle Component Classifier
ev <- (Cdf.pca$sdev)^2
loadings=Cdf.pca$rotation #[ROW=PREDICTORS.COL=PCs]
#Here we will construct our PCC in function

#pcsamples=predict(df.pca,df)
#response=as.vector(1:nrow(df))
#pcdistance_1arge=as.vector(1:nrow(df))
#pcdistance_small=as.vector(1:nrow(df))
#summ1=0.0
#summ2=0.0
#for (i in 1:length(response))
#{
#  summ1=0.0
#  for (j in 1:2)
#  {summ1=summ1+pcsamples[i,j]^2/ev[j]}
#  pcdistance_1arge[i]=summ1
#} 
#hist(pcdistance_1arge)
#hist((pcdistance_1arge),breaks=1000,probability=TRUE)

#for (i in 1:length(response))
#{
#  summ2=0.0
#  for (j in 6:116)
#  {summ2=summ2+pcsamples[i,j]^2/ev[j]}
#  pcdistance_small[i]=summ2
#} 
#hist(pcdistance_small)
#hist(log(pcdistance_small),breaks=1000,probability=TRUE)


#debug(PCC)
results=PCC(ev,Cdf.pca,Cdf,2,6,4.0)
summary(results)



#head(predict(df.pca,df))

#Construct the prediction measure
#library(caret)
#confusionMatrix((data, reference, positive = NULL, 
#                dnn = c("Prediction", "Reference"), 
#               prevalence = NULL, ...))


#We can use this to predict the loadings for each predictive variables
#predict(df.pca,newdata)






#library(corrplot)
#bcor <- cor(df)

#1.Use pcomp to extract information for percentage of vraiance and loadings
#2.Predict the training itself
#3.Move to test set
#4.Reducing the information from nonattack sampling





#bcor[45,]=0.0 #standard error=0 and therefore no correlation
#bcor[,45]=0.0
#bcor[45,45]=1.0
#bcor[52,]=0.0
#bcor[,52]=0.0
#bcor[52,52]=1.0
#bcor[99,]=0.0
#bcor[,99]=0.0
#bcor[99,99]=1.0


#library(FactoMineR)
#result <- PCA(df)
#fa.parallel(bcor, #The data in question.
#            n.obs = nrow(bcor), #Since we supplied a covaraince matrix, need to know n.
#            fa = "pc", #Display the eigenvalues for PCA.
#            n.iter = 150) #Number of simulated analyses to perform.
#abline(h = 1) #
#pc_normal = principal(bcor, #The data in question.
#                          nfactors = 20, #The number of PCs to extract.
#                          rotate = "none")

#fa.diagram(bcor)

#pc_bodies = principal(bcor, #The data in question.
#                      nfactors = 2, #The number of PCs to extract.
#                      rotate = "none")
#pc_bodies

#factor.plot(pc_bodies,
#            labels = colnames(bcor))


#write.table(bcor, "c:/cor_matrix.txt", sep="\t") 



