df = read.table("~/Desktop/Capstone/training_fraud.txt", header = TRUE)

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


library(psych)
library(caret)
library(nnet)
library(RCurl)
library(Metrics)

df$is_host_login<-(sapply(as.numeric(df$is_host_login), function(x) ifelse(x > 0, 1, -1)))#??
df$is_guest_login<-(sapply(as.numeric(df$is_guest_login), function(x) ifelse(x > 0, 1, -1)))#??
df$Land<-(sapply(as.numeric(df$Land), function(x) ifelse(x > 0, 1, -1)))
df$logged_in<-(sapply(as.numeric(df$logged_in), function(x) ifelse(x > 0, 1, -1)))
df$root_shell<-(sapply(as.numeric(df$root_shell), function(x) ifelse(x > 0, 1, -1)))
df$attack<-(sapply(df$attack, function(x) ifelse(x == 'normal', 1, -1)))


df<-df[-c(2:4)]
df_response<-df$attack
df<-subset(df, attack==1)
df_normal_res<-df$attack #all 1s
df<-subset(df, select = -c(attack))
df<-subset(df, select = -c(is_guest_login,is_host_login,root_shell,logged_in,wrong_fragment,num_outbound_cmds))
df[,c(1:32)]=sapply(df[,c(1:32)],Standard_self1)
df.pca <- prcomp(df,center = FALSE,scale. = FALSE) 
plot(df.pca, type = "l")
ev <- (df.pca$sdev)^2
pcsamples<-predict(df.pca,df)
distance_1arge<-as.vector(1:nrow(df))

for (i in 1:nrow(df))
{
  summ1=0.0
  for (j in 1:2)
  {summ1=summ1+pcsamples[i,j]^2/ev[j]}
  distance_1arge[i]=summ1
}
#The bug appear near this part
for (i in 1:nrow(df))
{ 
  if ( distance_large[i] > 10.0)
  {distance_large[i]=-1.0}
  else
  {distance_large[i]=1.0}
}