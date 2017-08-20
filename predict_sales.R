#import train and test data
sales.train<-read.csv("Train_UWu5bXk.csv",header=T,stringsAsFactors=FALSE)
sales.test<-read.csv("Test_u94Q5KV.csv",header=T,stringsAsFactors=FALSE)

#compute missing values

sum(is.na(sales.train))
sum(is.na(sales.test))

#replace some missing values with mean

sales.train$Item_Weight[is.na(sales.train$Item_Weight)]<-mean(sales.train$Item_Weight,na.rm=T)
sales.test$Item_Weight[is.na(sales.test$Item_Weight)]<-mean(sales.test$Item_Weight,na.rm=T)

#analyze values in Item_Fat_Content

unique(sales.train$Item_Fat_Content)
unique(sales.test$Item_Fat_Content)

#deal with redundant levels

sales.train$Item_Fat_Content[sales.train$Item_Fat_Content=="LF" | sales.train$Item_Fat_Content=="Low Fat" | sales.train$Item_Fat_Content=="low fat"]<-"LF"
sales.train$Item_Fat_Content[sales.train$Item_Fat_Content=="reg" | sales.train$Item_Fat_Content=="Regular"]<-"Reg"

sales.test$Item_Fat_Content[sales.test$Item_Fat_Content=="LF" | sales.test$Item_Fat_Content=="Low Fat" | sales.test$Item_Fat_Content=="low fat"]<-"LF"
sales.test$Item_Fat_Content[sales.test$Item_Fat_Content=="reg" | sales.test$Item_Fat_Content=="Regular"]<-"Reg"

#replace 0 with mean value in Item_Visibility

sales.train$Item_Visibility[sales.train$Item_Visibility==0]<-mean(sales.train$Item_Visibility[sales.train$Item_Visibility!=0])
sales.test$Item_Visibility[sales.test$Item_Visibility==0]<-mean(sales.test$Item_Visibility[sales.test$Item_Visibility!=0])

#break down item types into 3 categories

broad_food<-c()
broad_drink<-c()
broad_nonc<-c()
for(i in 1:nrow(sales.train))
{
  broad_food[i]<-!is.na(pmatch("FD",sales.train$Item_Identifier[i]))
  broad_drink[i]<-!is.na(pmatch("DR",sales.train$Item_Identifier[i]))
  broad_nonc[i]<-!is.na(pmatch("NC",sales.train$Item_Identifier[i]))
}

sales.train$Item_Type[broad_food]<-"Food"
sales.train$Item_Type[broad_drink]<-"Drinks"
sales.train$Item_Type[broad_nonc]<-"Non-Consumables"

broad_food<-c()
broad_drink<-c()
broad_nonc<-c()
for(i in 1:nrow(sales.test))
{
  broad_food[i]<-!is.na(pmatch("FD",sales.test$Item_Identifier[i]))
  broad_drink[i]<-!is.na(pmatch("DR",sales.test$Item_Identifier[i]))
  broad_nonc[i]<-!is.na(pmatch("NC",sales.test$Item_Identifier[i]))
}

sales.test$Item_Type[broad_food]<-"Food"
sales.test$Item_Type[broad_drink]<-"Drinks"
sales.test$Item_Type[broad_nonc]<-"Non-Consumables"

#find years of operation of store

sales.train$Outlet_Establishment_Year<-2013-sales.train$Outlet_Establishment_Year
sales.test$Outlet_Establishment_Year<-2013-sales.test$Outlet_Establishment_Year


#replace blank values in Outlet_Size with NA

sales.train$Outlet_Size[sales.train$Outlet_Size==""]<-NA
sales.test$Outlet_Size[sales.test$Outlet_Size==""]<-NA


#delete variables that are not required anymore
rm(list=setdiff(ls(), c("sales.test","sales.train")))

#convert character to factors
for(i in 2:ncol(sales.train))
{
  if(is.character((sales.train[,i])))
  {
    sales.train[,i]<-as.factor(sales.train[,i])
  }
}

#convert character to factors

for(i in 2:ncol(sales.train))
{
  if(is.character((sales.train[,i])))
  {
    sales.train[,i]<-as.factor(sales.train[,i])
  }
}

for(i in 2:ncol(sales.test))
{
  if(is.character((sales.test[,i])))
  {
    sales.test[,i]<-as.factor(sales.test[,i])
  }
}

#remove co-related columns to avoid singularities

sales.train<-sales.train[,-c(7,9)]
sales.test<-sales.test[,-c(7,9)]

#divide training data into train and test data to evaluate different metrics

set.seed(1)
train<-sample(1:nrow(sales.train),nrow(sales.train)/2,replace=F)
test=-train
sales.train.x1<-model.matrix(Item_Outlet_Sales~.-Item_Identifier,data=sales.train[train,])[,-1]
sales.train.y1<-sales.train$Item_Outlet_Sales[train]
sales.train.x2<-model.matrix(Item_Outlet_Sales~.-Item_Identifier,data=sales.train[test,])[,-1]

library(glmnet)

#ridge regression with lambda chosen by cross validation

grid<-10^seq(4,-2,length=100)
cv.ridge<-cv.glmnet(sales.train.x1,sales.train.y1,alpha=0,lambda=grid)
ridge.fit<-glmnet(sales.train.x1,sales.train.y1,alpha=0,lambda=grid)

#lasso regression with lambda chosen by cross validation

cv.lasso<-cv.glmnet(sales.train.x1,sales.train.y1,alpha=1,lambda=grid)
lasso.fit<-glmnet(sales.train.x1,sales.train.y1,alpha=1,lambda=grid)

predict.ridge<-predict(ridge.fit,s=cv.ridge$lambda.min,sales.train.x2)

predict.lasso<-predict(lasso.fit,s=cv.lasso$lambda.min,sales.train.x2)

mean((predict.lasso-sales.train$Item_Outlet_Sales[test])^2)>mean((predict.ridge-sales.train$Item_Outlet_Sales[test])^2)

#ridge regression seems to perform slightly better than lasso regression, fit ridge regression model using the full training data and remove unwanted variables

rm(list=setdiff(ls(), c("sales.test","sales.train","grid")))

set.seed(1)
sales.train.x<-model.matrix(Item_Outlet_Sales~.-Item_Identifier,data=sales.train)[,-1]
sales.train.y<-sales.train$Item_Outlet_Sales
cv.ridge<-cv.glmnet(sales.train.x,sales.train.y,alpha=0,lambda=grid)
ridge.fit<-glmnet(sales.train.x,sales.train.y,alpha=0,lambda=grid)

sales.test.x<-model.matrix(~.-Item_Identifier,data=sales.test)[,-1]

#make prediction and write to csv
predict.ridge<-predict(ridge.fit,s=cv.ridge$lambda.min,sales.test.x)

#make prediction and write to csv

sales.test<-read.csv("Test_u94Q5KV.csv",header=T,stringsAsFactors=FALSE)

prediction<-cbind(sales.test$Item_Identifier,sales.test$Outlet_Identifier,predict.ridge)
colnames(prediction)<-c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
write.csv(prediction,file="predict_sales.csv")