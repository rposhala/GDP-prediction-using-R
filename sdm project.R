rm(list = ls())
library(DAAG)                 
library(lattice)
library(MASS)
library("geneplotter")
library(ggplot2)
library(readr)
library("Hmisc")
library(corrplot)
library("dlookr")
library(dplyr)
library(tidyr)
library(ruler)
library(data.table)
library("ElemStatLearn")
library("class")
library("ISLR")
library(glmnet)
library(pls)
library(leaps)
library(tidyverse)
library(caret)
library(klaR)  
library(bestglm)
library(corrplot)
library("DataExplorer")
library('GGally')
library(boot)
library('readxl')
library('varhandle')
library('dtw')
library(randomForest)
library(tree)
library('rpart')
library("dataseries")


##############    data parsing ############

my_data = data.frame(read_excel('Popular Indicators .xlsx'))
data <- t(read_excel('Data_Extract_From_World_Development_Indicators_IND.xlsx'))
ind_data <- data.frame(data)[-c(1,2,3),]
ind_data[] <- lapply(ind_data, as.character)
colnames(ind_data) <- ind_data[1,]
colnames(ind_data) <- as.factor(colnames(ind_data))
ind_data <- ind_data[-1,]

ind_data[ind_data==".."]<-NA
#length(which(is.na(ind_data$PA.NUS.PPP.05)))

new_ind_data = data.frame(total = 1: dim(ind_data)[1])
ind_data_matrix <- as.matrix(ind_data)

#new_ind_data[i] <- fill.NAs(new_ind_data[i])

for (i in colnames(ind_data) ){
  if (length(which(is.na(ind_data_matrix[,i]))) < 15){
    temp <- as.numeric(ind_data_matrix[,i])
    c <- NA
    
    for (j in 1:length(temp)){ 
      if (is.na(temp[j])){
        temp[j] = c
      }
      c <- temp[j]
    }
    new_ind_data[i] <- temp
  }
}

new_ind_data <- new_ind_data[,-1]
rownames(new_ind_data) <- rownames(ind_data)

new_ind_data[is.na(new_ind_data)] <- 0

name_code <- data.frame(data)[c(3,4),]
name_code[] <- lapply(name_code, as.character)
colnames(name_code) <- name_code[2,]
colnames(name_code) <- as.factor(colnames(name_code))
name_code <- name_code[-2,]

####### correlation plots#######
R1 <- cor(new_ind_data)

subs1 <- subset(melt(R1), value > .99 & value < 1.0)
i60_71=new_ind_data[1:12,]
i72_83=new_ind_data[13:24,]
i84_95=new_ind_data[25:36,]
i96_07=new_ind_data[37:48,]
i08_17=new_ind_data[49:58,]


gdp_cor_subs1 = subs1[which(subs1$Var1 == 'NY.GDP.PCAP.CD'),]

r60_71 = cor(i60_71)
r72_83 = cor(i72_83)
r84_95 = cor(i84_95)
r96_07 = cor(i96_07)
r08_17 = cor(i08_17)

subs_1 <- subset(melt(r60_71), value > .95 & value < 1.0)
subs_2 <- subset(melt(r72_83), value > .99 & value < 1.0)
subs_3 <- subset(melt(r84_95), value > .80 & value < 1.0)
subs_4 <- subset(melt(r96_07), value > .99 & value < 1.0)
subs_5 <- subset(melt(r08_17), value > .99 & value < 1.0)




gdp_cor_60_71 = subs_1[which(subs_1$Var1 == 'NY.GDP.PCAP.CD'),]
gdp_cor_72_83 = subs_2[which(subs_2$Var1 == 'NY.GDP.PCAP.CD'),]
gdp_cor_84_95 = subs_3[which(subs_3$Var1 == 'NY.GDP.PCAP.CD'),]
gdp_cor_96_07 = subs_4[which(subs_4$Var1 == 'NY.GDP.PCAP.CD'),]
gdp_cor_08_17 = subs_5[which(subs_5$Var1 == 'NY.GDP.PCAP.CD'),]



plot_correlation()


# corrplot(r60_71)
# corrplot(r72_83)
# corrplot(r84_95)
# corrplot(r96_07)
# corrplot(r08_17)



######################  static model ####################
#########################################################

glm_fit = glm(NY.GDP.PCAP.CD ~ . ,data = new_ind_data[1:50,])
for (i in 56:59){
  gdp_predict[i-55] = predict(glm_fit,new_ind_data[i,],type='response')
}


##############     Forward  subset selection   ###############
regfit_fwd <- regsubsets(NY.GDP.PCAP.CD~., data = new_ind_data[1:50,], nbest = 1, nvmax = 50, method = "forward")
fwd_sum = summary(regfit_fwd)

fwd= which(fwd_sum$adjr2 == max(fwd_sum$adjr2))                 ######   going with adjusted r2   because for the others wew are getting low accuracy or only dependent on one coefficient  
                                                                ######   min cp - gives only one coefficient, rss - gives very high error, hence preferred max adjr2
fwd_coef = coef(regfit_fwd,fwd)

col_name = colnames(new_ind_data)
fwd_mod_pred= rep(0,4)
for(i in 56:59){
  fwd_mod_pred[i-55] = as.matrix(new_ind_data[i, col_name %in% names(fwd_coef)]) %*% fwd_coef[names(fwd_coef) %in% col_name]
}


##############     Backward subset selction   ###############
regfit_bwd <- regsubsets(NY.GDP.PCAP.CD~., data = new_ind_data[1:50,], nbest = 1, nvmax = 50, method = "forward")
bwd_sum = summary(regfit_bwd)

bwd= which(bwd_sum$adjr2 == max(bwd_sum$adjr2))
bwd_coef = coef(regfit_bwd,bwd)

col_name = colnames(new_ind_data)

bwd_mod_pred= rep(0,4)
for (i in 56:59){
bwd_mod_pred[i-55] = as.matrix(new_ind_data[i, col_name %in% names(bwd_coef)]) %*% bwd_coef[names(bwd_coef) %in% col_name]
}

###############   PCA #################

pcr_fit = pcr(NY.GDP.PCAP.CD~., data = new_ind_data[1:55,], x = TRUE)
pcr_sum <- summary(pcr_fit)

pcr_pred= rep(0,4)
for (i in 56:59){
pcr_pred[i-55] = predict(pcr_fit, new_ind_data[i,], ncomp = 56)
}
###############    Random Forest  ###########

ctr_mod =rpart.control(minsplit=15,xval=10,cp = 0)

ind.rf=rpart(NY.GDP.PCAP.CD ~ . , data = new_ind_data[1:55,])
rf_pred = rep(0,4)
for(i in 56:59){
rf_pred[i-55] = predict(ind.rf,new_ind_data[i,], type = "vector") 
}

############## pruned model ###############

cp_min = which.min(ind.rf$cptable[,4])
pruned_model <- prune(ind.rf, cp = ind.rf$cptable[cp_min,1])
prune_pred = rep(0,4)
for (i in 56:59){
prune_pred[i-55]= predict(pruned_model,new_ind_data[i,], type = "vector")
}

##################   Xtreme Gradiant Boost    ####################
model_xgb <- train(
  NY.GDP.PCAP.CD~., data = new_ind_data[1:55,], method = "xgbTree",
  trControl = trainControl("cv", number = 3500)
)



model_xgb$bestTune                            # Best tuning parameter
for (i in 56:59){
predicted.classes[i-55] <- model_xgb%>%predict(new_ind_data[i,])
# mean(predicted.classes == new_ind_data[i,])
# View(predicted.classes)
# varImp(model)
RMSE(predicted.classes, new_ind_data[i,])
}

glm_err = sqrt(mean((ytrue-gdp_predict)^2))
fwd_err = sqrt(mean((ytrue-fwd_mod_pred)^2))
bwd_err = sqrt(mean((ytrue-bwd_mod_pred)^2))
pcr_err = sqrt(mean((ytrue-pcr_pred)^2))
rf_err = sqrt(mean((ytrue-rf_pred)^2))
prune_err = sqrt(mean((ytrue-prune_pred)^2))
err = cbind(glm_err,fwd_err,bwd_err,pcr_err,rf_err,prune_err)

#########################################################################
#############################     Dynamic Model   #######################



gdp_predict_d = rep(0,4)
fwd_mod_pred_d = rep(0,4)
bwd_mod_pred_d = rep(0,4)
rf_pred_d = rep(0,4)
pcr_pred_d= rep(0,4)
prune_pred_d = rep(0,4)




for (i in 56:59){
  
  glm_fit_d = glm(NY.GDP.PCAP.CD ~ . ,data = new_ind_data[1:i-1,])
  
  gdp_predict_d[i-55] = predict(glm_fit_d,new_ind_data[i,],type='response')
  
  
  
  ##############     Forward  subset selection   ###############
  regfit_fwd_d <- regsubsets(NY.GDP.PCAP.CD~., data = new_ind_data[1:i-1,], nbest = 1, nvmax = 50, method = "forward")
  fwd_sum_d = summary(regfit_fwd_d)
  
  fwd_d= which(fwd_sum_d$adjr2 == max(fwd_sum_d$adjr2))                 ######   going with adjusted r2   because for the others wew are getting low accuracy or only dependent on one coefficient  
  ######   min cp - gives only one coefficient, rss - gives very high error, hence preferred max adjr2
  fwd_coef_d = coef(regfit_fwd_d,fwd_d)
  
  col_name = colnames(new_ind_data)
  
  fwd_mod_pred_d[i-55] = as.matrix(new_ind_data[i, col_name %in% names(fwd_coef_d)]) %*% fwd_coef_d[names(fwd_coef_d) %in% col_name]
  
  
  ##############     Backward subset selction   ###############
  regfit_bwd_d <- regsubsets(NY.GDP.PCAP.CD~., data = new_ind_data[1:i-1,], nbest = 1, nvmax = 50, method = "forward")
  bwd_sum_d = summary(regfit_bwd_d)
  
  bwd_d= which(bwd_sum_d$adjr2 == max(bwd_sum_d$adjr2))
  bwd_coef_d = coef(regfit_bwd_d,bwd_d)
  
  col_name = colnames(new_ind_data)
  
  
  bwd_mod_pred_d[i-55] = as.matrix(new_ind_data[i, col_name %in% names(bwd_coef_d)]) %*% bwd_coef_d[names(bwd_coef_d) %in% col_name]
  
  
  ###############   PCA #################
  
  pcr_fit_d = pcr(NY.GDP.PCAP.CD~., data = new_ind_data[1:i-1,], x = TRUE)
  pcr_sum_d <- summary(pcr_fit_d)
  
  
  pcr_pred_d[i-55] = predict(pcr_fit_d, new_ind_data[i,], ncomp = 50)
  ###############    Random Forest  ###########
  
  ctr_mod =rpart.control(minsplit=15,xval=10,cp = 0)
  
  ind.rf_d=rpart(NY.GDP.PCAP.CD ~ . , data = new_ind_data[1:i-1,])
 
  
  rf_pred_d[i-55] = predict(ind.rf_d,new_ind_data[i,], type = "vector") 
  
  ############## pruned model ###############
  
  cp_min = which.min(ind.rf_d$cptable[,4])
  pruned_model_d <- prune(ind.rf_d, cp = ind.rf_d$cptable[cp_min,1])
  # prune_pred = rep(0,4)
  prune_pred_d[i-55]= predict(pruned_model_d,new_ind_data[i,], type = "vector")
  
  
  # ##################   Xtreme Gradiant Boost    ####################
  # model_xgb <- train(
  #   NY.GDP.PCAP.CD~., data = new_ind_data[1:55,], method = "xgbTree",
  #   trControl = trainControl("cv", number = 3500)
  # )
  
  
  
#   model_xgb$bestTune                            # Best tuning parameter
#   for (i in 56:59){
#     predicted.classes[i-55] <- model_xgb%>%predict(new_ind_data[i,])
#     # mean(predicted.classes == new_ind_data[i,])
#     # View(predicted.classes)
#     # varImp(model)
#     RMSE(predicted.classes, new_ind_data[i,])
#   }
# }

}







