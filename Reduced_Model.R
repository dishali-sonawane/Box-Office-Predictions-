#import data from google sheet
install.packages('gsheet')
library(gsheet)
final_reduced_dataset<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1OLm1zTiRz5vd6ytuo8FWZT7H27FED3WhlPU9Z3qwszY/edit#gid=1819026991')


#drop x1 column
final_reduced_dataset$X1<-NULL

#change column name
colnames(final_reduced_dataset)[colnames(final_reduced_dataset) == '149Pictures'] <- 'OnefortyninePictures'
colnames(final_reduced_dataset)[colnames(final_reduced_dataset) == 'HomeBoxOffice(HBO)'] <- 'HomeBoxOffice'
colnames(final_reduced_dataset)[colnames(final_reduced_dataset) == 'LeStudioCanal+'] <- 'LeStudioCanal'

#time slice the data

for(i in 1:nrow(final_reduced_dataset)){
  if(final_reduced_dataset$year[i]>=1970){
    final_reduced_dataset$modern_year[i]<-1
  }
  else{
    final_reduced_dataset$modern_year[i]<-0
  }
}

final_reduced_dataset$year<-NULL



#dummy for friday and date before 15 
for(i in 1:nrow(final_reduced_dataset)){
  if(final_reduced_dataset$Fri[i]==1 & final_reduced_dataset$day[i]>=15){
    final_reduced_dataset$month_end_fri[i]<-1
  }
  else{
    final_reduced_dataset$month_end_fri[i]<-0
  }
  if(final_reduced_dataset$Fri[i]==0 & final_reduced_dataset$day[i]>=15){
    final_reduced_dataset$month_end_not_fri[i]<-1
  }
  else{
    final_reduced_dataset$month_end_not_fri[i]<-0
  }
  if(final_reduced_dataset$Fri[i]==1 & final_reduced_dataset$day[i]<15){
    final_reduced_dataset$month_start_fri[i]<-1
  }
  else{
    final_reduced_dataset$month_start_fri[i]<-0
  }
  if(final_reduced_dataset$Fri[i]==0 & final_reduced_dataset$day[i]<15){
    final_reduced_dataset$month_start_not_fri[i]<-1
  }
  else{
    final_reduced_dataset$month_start_not_fri[i]<-0
  }
}

final_reduced_dataset$day<-NULL



#build the linear regression model

names(final_reduced_dataset)[names(final_reduced_dataset) == 'Kennedy/MarshallCompanyThe'] <- 'KennedyMarshallCompanyThe'

row.number <- sample(x=1:nrow(final_reduced_dataset), size=0.75*nrow(final_reduced_dataset))
sample_training = final_reduced_dataset[row.number, ]
Sample_validation = final_reduced_dataset[-row.number,]


reduced_model<-lm(revenue~., data = sample_training)
summary(reduced_model)

#reduce the model using selection model
#reduce the mnodel using backward selection method
step <- step(reduced_model, direction="backward")
step$anova # display results

step <- step(reduced_model, direction="forward")
step$anova

step <- step(reduced_model, direction="both")
step$anova

#reduced model

linear_reg_model<-lm(revenue~ budget + animation + drama + history + horror + dansk + 
                       language + steven_spielberg_ + martin_scorsese_ + tim_burtonofi + 
                       crew_count + keyword_count + budget_cast_ratio + budget_runtime_ratio + 
                       cast_count + BlueSkyStudios + BookshopProductions + DreamWorksAnimation + 
                       EuropaCorp + GreatAmericanFilmsLimitedPartnership + HomeBoxOffice + 
                       IngeniousFilmPartners + LeStudioCanal + Lucasfilm + MarvelStudios + 
                       TigProductions + WaltDisneyPictures + WingNutFilms + Uruguay + 
                       Aug + Fri, data = sample_training)

summary(linear_reg_model)

#predict the accuracy of the model on Validation data 
pred <- predict(linear_reg_model,Sample_validation)
observed_revenue<-Sample_validation$revenue

head(pred)
head(observed_revenue)

SSE <- sum((observed_revenue - pred) ^ 2)
SST <- sum((observed_revenue - mean(observed_revenue)) ^ 2)
r2 <- 1 - SSE/SST
rmse <- sqrt(mean((pred - observed_revenue)^2))

r2
rmse

# plot observed vs predicted regression line
plot(pred,observed_revenue, main = "Observed vs Predicted",
     xlab = "Predicted Revenue", ylab = "Observed Revenue",
     pch = 19, frame = FALSE)
abline(linear_reg_model, col = "blue")


#build random forest model

Needed <- c("rpart","caret","rattle","randomForest","randomForestSRC","tree","e1071","caTools")
install.packages(Needed, dependencies = TRUE)

library(rpart)
library(caret)
library(rattle)
library(randomForest)
library(randomForestSRC)
library(tree)
library(e1071)
library(caTools)

set.seed(12345)
row.number <- sample(x=1:nrow(final_reduced_dataset), size=0.75*nrow(final_reduced_dataset))
sample_training = final_reduced_dataset[row.number, ]
Sample_validation = final_reduced_dataset[-row.number,]

modfit<-randomForest(revenue~ .,data=sample_training)

modfit

#reduced random forest model based on significant variables from backward selection
random_for_model<-randomForest(revenue~ budget + animation + drama + history + horror + dansk + 
                                 language + steven_spielberg_ + martin_scorsese_ + tim_burtonofi + 
                                 crew_count + keyword_count + budget_cast_ratio + budget_runtime_ratio + 
                                 cast_count + BlueSkyStudios + BookshopProductions + DreamWorksAnimation + 
                                 EuropaCorp + GreatAmericanFilmsLimitedPartnership + HomeBoxOffice + 
                                 IngeniousFilmPartners + LeStudioCanal + Lucasfilm + MarvelStudios + 
                                 TigProductions + WaltDisneyPictures + WingNutFilms + Uruguay + 
                                 Aug + Fri,data=sample_training)

random_for_model

#predict the accuracy of the model on Validation data 
pred <- predict(random_for_model,Sample_validation)
observed_revenue<-Sample_validation$revenue

head(pred)
head(observed_revenue)

SSE <- sum((observed_revenue - pred) ^ 2)
SST <- sum((observed_revenue - mean(observed_revenue)) ^ 2)
r2 <- 1 - SSE/SST
rmse <- sqrt(mean((pred - observed_revenue)^2))

r2
rmse

feat_imp<-as.data.frame(importance(random_for_model))
par(mar=c(5,16,4,1)+.1)
barplot((feat_imp$IncNodePurity),names.arg = row.names(feat_imp),horiz=TRUE,las=1)
title("Feature importance based on IncNodePurity value",adj=0)
