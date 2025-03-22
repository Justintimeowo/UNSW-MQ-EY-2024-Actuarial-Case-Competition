set.seed(70)

install.packages('readr')
install.packages('dplyr')
install.packages("readxl")
install.packages('tidyselect')
install.packages('tidyverse')
install.packages('writexl')
library(readr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(readxl)
library(writexl)

macro_data<-read_xlsx("Macro_data.xlsx")
attach(macro_data)

train.data<-sample(nrow(macro_data),0.75*nrow(macro_data))


refinance_fit<-lm(Ext.Refinancing ~ Inflation.p.a +
                      Real_GDP_Growth +
                      House_Deposits +
                      Total_Balances +
                      Cash_Rate +
                      Unemployment_Rate +
                      GDI_Growth, data = macro_data[train.data,])
summary(refinance_fit)
anova(refinance_fit)

model_refinance<-predict(refinance_fit,newdata=macro_data[train.data,],type="response")
MSE.refinance<-sum(((macro_data$Ext.Refinancing[train.data]-model_refinance)/macro_data$Ext.Refinancing[train.data])^2)/length(model_refinance)
predict_refinance<-predict(refinance_fit,newdata=macro_data[-train.data,],type="response")
MSE.predict.refinance<-sum(((macro_data$Ext.Refinancing[-train.data]-predict_refinance)/macro_data$Ext.Refinancing[-train.data])^2)/length(predict_refinance)


direct_entry_fit<-lm(Direct_Entry ~ Inflation.p.a +
                      Real_GDP_Growth +
                      House_Deposits +
                      Total_Balances +
                      Cash_Rate +
                      Unemployment_Rate +
                      GDI_Growth, data = macro_data[train.data,])
summary(direct_entry_fit)
anova(direct_entry_fit)

model_direct_entry<-predict(direct_entry_fit,newdata=macro_data[train.data,],type="response")
MSE.direct_entry<-sum(((macro_data$Direct_Entry[train.data]-model_direct_entry)/macro_data$Direct_Entry[train.data])^2)/length(model_direct_entry)
predict_direct_entry<-predict(direct_entry_fit,newdata=macro_data[-train.data,],type="response")
MSE.predict.direct_entry<-sum(((macro_data$Direct_Entry[-train.data]-predict_direct_entry)/macro_data$Direct_Entry[-train.data])^2)/length(predict_direct_entry)

predict_data<-read_xlsx("Predict_Data.xlsx")
refinance_new_predict<-predict(refinance_fit, newdata=predict_data, type="response")
direct_entry_new_predict<-predict(direct_entry_fit, newdata=predict_data, type="response")
predict_trend<-data.frame(Period=predict_data$Period, Ext.Refinancing=refinance_new_predict,Direct_Entry=direct_entry_new_predict)

write.csv(predict_trend, file="Model_Predictions.csv")
