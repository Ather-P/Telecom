setwd("D:\\Course\\R\\Telecom")
Telecom_dataset = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
lapply(Telecom_dataset, function(x) sum(is.na(x)))

# Either remove or replace 11 rows with missing values in Total charges column
# Removing NA's
Telecom_dataset =na.omit(Telecom_dataset)

sum(is.na((Telecom_dataset$TotalCharges)))


lapply(Telecom_dataset, function(x) table(x))


#  Convert into Binaries for Gender SeniorCitizen Partner Dependents PhoneService paperlessBilling Churn

convert_to_binary = c("Partner","Dependents","PhoneService","PaperlessBilling","Churn")

for(i in convert_to_binary){
  Telecom_dataset[,i] = as.numeric(Telecom_dataset[,i]=="Yes") 
}

Telecom_dataset[,"gender_is_male"] = as.numeric(Telecom_dataset[,"gender"]=="Male")
Telecom_dataset = Telecom_dataset %>% 
  select(-gender)

table(Telecom_dataset$tenure)

glimpse(Telecom_dataset)


To_create_DUmmies = c('MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies')
                    
Telecom_dataset[,"MultipleLines_Yes"] = as.numeric(Telecom_dataset[,"MultipleLines"]=="Yes")
Telecom_dataset[,"MultipleLines_No"] = as.numeric(Telecom_dataset[,"MultipleLines"]=="No")

Telecom_dataset[,"OnlineSecurity_Yes"] = as.numeric(Telecom_dataset[,"OnlineSecurity"]=="Yes")
Telecom_dataset[,"OnlineSecurity_No"] = as.numeric(Telecom_dataset[,"OnlineSecurity"]=="No")

Telecom_dataset[,"OnlineBackup_Yes"] = as.numeric(Telecom_dataset[,"OnlineBackup"]=="Yes")
Telecom_dataset[,"OnlineBackup_No"] = as.numeric(Telecom_dataset[,"OnlineBackup"]=="No")

Telecom_dataset[,"DeviceProtection_Yes"] = as.numeric(Telecom_dataset[,"DeviceProtection"]=="Yes")
Telecom_dataset[,"DeviceProtection_No"] = as.numeric(Telecom_dataset[,"DeviceProtection"]=="No")

Telecom_dataset[,"TechSupport_Yes"] = as.numeric(Telecom_dataset[,"TechSupport"]=="Yes")
Telecom_dataset[,"TechSupport_No"] = as.numeric(Telecom_dataset[,"TechSupport"]=="No")

Telecom_dataset[,"StreamingTV_Yes"] = as.numeric(Telecom_dataset[,"StreamingTV"]=="Yes")
Telecom_dataset[,"StreamingTV_No"] = as.numeric(Telecom_dataset[,"StreamingTV"]=="No")

Telecom_dataset[,"StreamingMovies_Yes"] = as.numeric(Telecom_dataset[,"StreamingMovies"]=="Yes")
Telecom_dataset[,"StreamingMovies_No"] = as.numeric(Telecom_dataset[,"StreamingMovies"]=="No")


for(i in To_create_DUmmies){
  Telecom_dataset[,i] = NULL
}

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

dummies = c("InternetService","Contract")


for(i in dummies){
  Telecom_dataset = CreateDummies(Telecom_dataset, i , 10)
}

Telecom_dataset= CreateDummies(Telecom_dataset, "PaymentMethod", 10)

glimpse(Telecom_dataset)

# e = ggplot(Telecom_dataset, aes(customerID,MonthlyCharges))
# e+geom_point()
# 
# b = ggplot(Telecom_dataset, aes(customerID,TotalCharges))
# b+geom_boxplot()

# write.csv(Telecom_dataset, "Telecom.csv",row.names = F)

ld_all = read.csv("Telecom.csv")

ld_all$customerID = NULL


library(pROC)
library(car)

set.seed(2)
s=sample(1:nrow(ld_all),0.8*nrow(ld_all))
ld_train=ld_all[s,] ## 80% Train data
ld_test=ld_all[-s,] ## 20% Train Test Data
ld_test_predict=ld_test %>% select(-Churn)
for_vif=lm(Churn~.,data = ld_train)
summary(for_vif)
log_fit = glm(Churn~., data= ld_train)
log_fit = step(log_fit)
formula(log_fit)


log_fit = glm(Churn ~ SeniorCitizen + Dependents + tenure + PaperlessBilling + 
                MonthlyCharges + TotalCharges + MultipleLines_Yes + OnlineSecurity_Yes + 
                OnlineSecurity_No + TechSupport_Yes + StreamingTV_Yes + StreamingMovies_Yes + 
                InternetService_DSL + Contract_Twoyear + Contract_Month_to_month + 
                PaymentMethod_Electroniccheck, data= ld_train, family = 'binomial')


summary(log_fit)

val.score=predict(log_fit,newdata = ld_test_predict, type='response')

train.score=predict(log_fit,newdata = ld_train,type='response')
real=ld_train$Churn

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99999,Sn=99999,Sp=99999,KS=9999,F5=9999,F.1=9999,M=9999)

for(cutoff in cutoffs){
  
  ## Conversion into hard calsses
  predicted=as.numeric(train.score>cutoff)
  
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(100*FP+TP)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

final_cutoff = 0.293

ld_test_predict$Churn = val.score

ld_test_predict[ld_test_predict>=0.293]=1
ld_test_predict[ld_test_predict<0.293]=0

ld_test_predict$Churn


pred <- ld_test$Churn
true <- ld_test_predict$Churn
(sum(pred==true, na.rm=T) + sum(is.na(pred) & is.na(true))) / length(pred)









