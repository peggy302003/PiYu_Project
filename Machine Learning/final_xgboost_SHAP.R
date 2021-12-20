
rm(list=ls())
cat("\014")
# Xgboost model training
# install.packages("xgboost")
library(xgboost)
library(caTools)
library(dplyr)
library(tidyr)
library(tidyverse)
library('caret')
library('lattice')
library('Matrix')
cars <- read.csv('cars.csv')
cars2 <- cars %>% drop_na()
cars1 <- subset(cars2,select=-c(model_name,engine_type,is_exchangeable))
# create new variable
luxury_brand=c('Audi','BMW','Land Rover','Lincoln',
               'Mercedes-Benz','ГАЗ','Porsche')
cars1$brand_status=ifelse(cars1$manufacturer_name %in% luxury_brand,1,0)

# Convert True False Value to 1 and 0
convertlogic <- function(x) {  
  if (x =='True') {
    x=1} else {
      x=0}
}

convertls <-c('feature_0','feature_1','feature_2',
              'feature_3','feature_4','feature_5',
              'feature_6','feature_7','feature_8',
              'feature_9','has_warranty','engine_has_gas')

cars1[,convertls] <- sapply(cars1[,convertls], convertlogic)
dmy <- dummyVars("~.",data=cars1)
trsf <- data.frame(predict(dmy,newdata=cars1))
y=trsf$price_usd
y
X=subset(trsf,select=-c(price_usd))
set.seed(1234)
spl <- sample.split(trsf$price_usd,0.7)
train=subset(trsf,spl==TRUE)
test=subset(trsf,spl==FALSE)
X_train <- subset(train,select=-c(price_usd))
bst=xgboost(data=data.matrix(X_train),
            label = train$price_usd,
            max.depth=15,
            nrounds=120,
            objective='reg:linear',
            eval_metric='mae')

X_test <- subset(test,select=-c(price_usd))
y_pred <- predict(bst,data.matrix(X_test))
y_test = test$price_usd
MAE = sum(abs(y_pred-y_test))/length(y_pred)
MAE

MSE = sum((y_pred-y_test)**2)/length(y_pred)
MSE

# Explainable AI- SHAP
#--------notes------
# The calculation of SHAP value is very time consuming. Make sure to do test run before you throw the whole data set in.
# In this case I only throw 5000 rows into the SHAP model to analysis.

#install.packages("SHAPforxgboost")
library("SHAPforxgboost")
library("ggplot2")
# select the rows you need for SHAP. The columns should be the same as the columns you use for model training.
test_sample <-X_test[sample(nrow(X_test),5000,replace=FALSE),]
# calculate SHAP value 
shap_values <- shap.values(xgb_model = bst, X_train = as.matrix(test_sample))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(test_sample))
# generate graph
fig_list <- lapply(names(shap_values$mean_shap_score)[1:6], 
                   shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)


# Abnormal value Case analysis
test$y_pred = predict(bst,data.matrix(X_test))
head(test)
incorrect = test[test$y_pred<10000 & test$price_usd>20000 & test$price_usd<30000 ,]
incorrect
incorrect
test_result
usual_case = test[test$price_usd<10000,]
mean(usual_case$year_produced)
mean(usual_case$drivetrainfront)
mean(usual_case$engine_capacity)
mean(usual_case$odometer_value)
mean(usual_case$transmissionautomatic)

mean(incorrect$year_produced)
mean(incorrect$drivetrainfront)
mean(incorrect$engine_capacity)
mean(incorrect$odometer_value)
mean(incorrect$transmissionautomatic)

cars2[c(23317,23325,26547,35376,35330),]
unique(cars2$manufacturer_name)
test[35330,]

# baseline
MAE = sum(abs(y_pred-y_test))/length(y_pred)
MAE

MSE = sum((y_pred-y_test)**2)/length(y_pred)
MSE

predict = mean(train$price_usd) 
real = test$price_usd
test_baseline <- data.frame(predict, real)
head(test_baseline)

MAE = sum(abs(test_baseline$predict-test_baseline$real))/nrow(test_baseline)
MAE
MSE = sum((test_baseline$predict-test-test_baseline$real)**2)/length(test_baseline)
print(MSE)
sum(test_baseline$predict-test-test_baseline$real)

# Select top20 column to increase training efficiency 
select_col = c("year_produced","drivetrainfront","engine_capacity","odometer_value","transmissionautomatic","engine_fueldiesel",             
"number_of_photos", "duration_listed" ,"body_typeminibus" , 
"up_counter" , "body_typesuv" ,"manufacturer_nameToyota",       
"manufacturer_nameMercedes.Benz", "manufacturer_nameVolkswagen","manufacturer_nameRenault" ,   
"manufacturer_nameAudi","manufacturer_nameNissan","manufacturer_nameFord",         
"manufacturer_nameВАЗ" ,"body_typesedan","price_usd")              


set.seed(1234)
spl <- sample.split(trsf$price_usd,0.7)
train_2=subset(trsf,spl==TRUE,select=select_col)
test_2=subset(trsf,spl==FALSE,select=select_col)
X_train_2 <- subset(train_2,select=-c(price_usd))
bst_2=xgboost(data=data.matrix(X_train_2),
            label = train$price_usd,
            max.depth=15,
            nrounds=180,
            objective='reg:linear',
            eval_metric='mae')

X_test_2 <- subset(test_2,select=-c(price_usd))
y_pred_2 <- predict(bst,data.matrix(X_test_2))
y_test_2 = test_2$price_usd

MAE2 = sum(abs(y_pred_2-y_test_2))/length(y_pred)
MAE2

MSE2 = sum((y_pred_2-y_test_2)**2)/length(y_pred)
MSE2


shap_contrib <- predict(bst_2,
                        (as.matrix(test2)),
                        predcontrib = TRUE)

shap_contrib

test2 = head(X_test_2,1000)
shap_values <- shap.values(xgb_model = bst_2, X_train = as.matrix(test2))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(test2))
fig_list <- lapply(names(shap_values$mean_shap_score)[1:4], 
                   shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)


shap.plot.summary(shap_long)
g1 <- shap.plot.dependence(data_long = shap_long, x = 'year_produce', y = 'year_produce', color_feature = 'year_produce') + ggtitle("(A) SHAP values of year_produce vs. year_produce")


shap_values
shap_values$shap_score

colnames(X_test)



tb = head(xgb.importance(colnames(X_train), model = bst),40)

unique(tb$Feature)

               
test_df = X_train[1:10,c('year_produced','drivetrainfront')]
test2 = head(X_train,10)
shap_values <- shap.values(xgb_model = bst, X_train = as.matrix(test2))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(test2))

shap_values
shap_values$shap_score
shap.plot.summary(shap_long)
g1 <- shap.plot.dependence(data_long = shap_long, x = 'dayint', y = 'dayint', color_feature = 'Column_WV') + ggtitle("(A) SHAP values of Time trend vs. Time trend")
head(shap_long,100)
