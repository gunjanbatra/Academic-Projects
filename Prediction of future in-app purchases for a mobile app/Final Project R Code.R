############################ FTS Project R code #############################



#Feature Extraction - Yanchi Liu
login=read.table("login_201511.txt", sep=',', header=F)
length(unique(login$V1))
length(unique(login$V6))


purchase=read.table("pay_201511.txt", sep=',', header=F)
length(unique(purchase$V1))
length(unique(purchase$V6))

ip <- read.table("ip_city.txt", sep=':', header=F, fill=T, quote="", na.strings=c("","None","NA"))
names(ip) = c("ip", "country", "province", "city")
as.factor(ip$country)
as.factor(ip$province)
as.factor(ip$city)
ip <- ip[complete.cases(ip),]

length(is.element(unique(purchase$V1), unique(login$V1)))
print (intersect(unique(purchase$V1), unique(login$V1)))

View(login[login$V1 == '1869414', ])
View(purchase[purchase$V1 == '1869414', ])


#####
## Feature Extraction
user_data=read.table("results.csv", sep=',', header=F, na.strings=c("","NA"))
names(user_data) = c("uid", "date", "ip", "login", "purchase", "amount")
user_data$date <- as.Date(user_data$date)
user_data <- user_data[1:10000, ]

## login in previous 7 days
n = 7
movsum <- function(x,n=7){filter(x,rep(1,n), sides=1)}
user_data$login_7 <- movsum(user_data$login, n) - user_data$login
user_data$purchase_7 <- movsum(user_data$purchase, n) - user_data$purchase
user_data$login_7 <- user_data$login_7 / (n-1)
user_data$purchase_7 <- user_data$purchase_7 / (n-1)
user_data$pur_aft_login_7 <- user_data$purchase_7 / user_data$login_7

## create label
user_data$label <- rep(0, nrow(user_data))
# label of purchase the current day
user_data[which(user_data$purchase>0), "label"] <- rep(1, length(which(user_data$purchase>0)))
# label of purchase the next day
user_data[which(user_data$purchase>0) - 1, "label"] <- rep(1, length(which(user_data$purchase>0)))
user_data$label <- as.factor(user_data$label)

##
user_purchase <- user_data_comp[which(user_data_comp$purchase != 0), ]
# #ts_purchase <- ts(user_purchase[,c("date", "purchase")])
# #plot(ts_purchase)
plot(table(user_purchase$date))
# 
# par(mfrow=c(2,1))
# plot(user_data$login)
# plot(user_data$purchase)
# cor(user_data$login, user_data$purchase)
# 
# tmp <- user_data_comp[which(user_data_comp$date > '2015-11-06'), ]
# unique(user_data$uid)

## time since last login/purchase
user_data_comp <- user_data[complete.cases(user_data),]

login_diff <- diff(user_data_comp$date)
login_diff[login_diff < 0] <- 0
login_diff <- c(0, login_diff)
length(login_diff)
user_data_comp$login_diff <- login_diff

idx <- user_data_comp$purchase > 0
purchase_diff <- diff(user_data_comp[idx,]$date)
purchase_diff[purchase_diff < 0] <- 0
purchase_diff <- c(0, purchase_diff)
length(purchase_diff)
user_data_comp$purchase_diff <- rep(0, nrow(user_data_comp))
user_data_comp[idx, ]$purchase_diff <- purchase_diff

## user statistics
library(plyr)
user_sum <- ddply(user_data_comp, c("uid"), summarize, login = sum(login), purchase = sum(purchase), amount = sum(amount))
user_avg <- user_sum[, c("login", "purchase", "amount")] / 30
names(user_avg) <- c("avg_login", "avg_purchase", "avg_amount")
user_avg$uid <- user_sum$uid
user_avg$convrate <- user_avg$avg_purchase / user_avg$avg_login


#####
user_data_comp <- merge(user_data_comp, ip, by="ip")
user_data_comp <- merge(user_data_comp, user_avg, by="uid")
user_data_comp <- user_data_comp[, c(1:9,11:19,10)]

write.csv(user_data_comp, file = 'user_data.txt', row.names = FALSE, quote = FALSE)
user_data<- read.table("user_data.txt", sep=',', header=T, fill=T, quote="", na.strings=c("","None","NA"))
user_data$label <- as.factor(user_data$label)

#Logistic Regression and SVM - Yueqing Zhang
## read data
user_data<- read.table("/Users/yueqingzhang/Desktop/2017 FTS project/user_data.txt", sep=',', header=T, fill=T, quote="", na.strings=c("","None","NA"))
user_data$label <- as.factor(user_data$label)
## train and test
set.seed(123)
idx_train <- sample(nrow(user_data), as.integer(nrow(user_data)*0.50))
train_data <- user_data[idx_train,]
test_data <- user_data[-idx_train,]

## Logistic Regression 
library(stats)
# m_lr = glm(label~.-login_7-pur_aft_login_7, set.seed(123), data=train_data, family=binomial)
m_lr = glm(label~.-login_7-pur_aft_login_7, set.seed(123), data=train_data[,-c(1:3, 16:18)], family=binomial)
summary(m_lr)
pred_lr = predict(m_lr, test_data)
pred_lr[pred_lr > .5] = 1
pred_lr[pred_lr <= .5] = 0  
table(observed = test_data$label,predicted =pred_lr)
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_lr)))/nrow(test_data)
accuracy # 0.9781641

## SVM model
library(e1071)
idx <- sample(nrow(train_data), as.integer(nrow(train_data)*0.05))
obj = best.tune(svm, label~., set.seed(123), data = train_data[idx,-c(1:3, 16:18)], kernel = "linear")
summary(obj) # cost:  1 
# gamma:  0.08333333 
m_svm <- svm(label~.,  set.seed(123), data=train_data[idx,-c(1:3, 16:18)], cost = 1, gamma = 0.08333333 ,
             kernel = "linear")
summary(m_svm)
pred_svm = predict(m_svm, test_data)
table(observed = test_data$label,predicted =pred_svm)
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_svm)))/nrow(test_data)
accuracy # 0.9795911

obj = best.tune(svm, label~., set.seed(123), data = train_data[idx,-c(1:3, 16:18)], kernel = "radial")
summary(obj)
m_svm <- svm(label~., data=train_data[idx,-c(1:3, 16:18)], cost = 1, gamma = 0.08333333,
             kernel = "radial")
pred_svm = predict(m_svm, test_data)
table(observed = test_data$label,predicted =pred_svm)
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_svm)))/nrow(test_data)
accuracy # 0.9796714

obj = best.tune(svm, label~., set.seed(123), data = train_data[idx,-c(1:3, 16:18)], kernel = "sigmoid")
summary(obj)
m_svm <- svm(label~.,  set.seed(123), data=train_data[idx,-c(1:3, 16:18)], cost = 1, gamma = 0.08333333,  
             kernel = "sigmoid")
pred_svm = predict(m_svm, test_data)
table(observed = test_data$label,predicted =pred_svm)
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_svm)))/nrow(test_data)
accuracy # 0.9356318

obj = best.tune(svm, label~., set.seed(123), data = train_data[idx,-c(1:3, 16:18)], kernel = "polynomial")
summary(obj)
m_svm <- svm(label~.,  set.seed(123), data=train_data[idx,-c(1:3, 16:18)], cost = 1, gamma = 0.08333333,   
             kernel = "polynomial")
pred_svm = predict(m_svm, test_data)
table(observed = test_data$label,predicted =pred_svm)
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_svm)))/nrow(test_data)
accuracy # 0.9791431

#Boosting and Random Forest - Gunjan Batra
setwd("C:/Users/gunjan/Desktop/Spring 2017/Fin Time Series/Project")
## read data
user_data<- read.table("user_data.txt", sep=',', header=T, fill=T, quote="", na.strings=c("","None","NA"))
user_data$label <- as.factor(user_data$label)
## train and test
set.seed(123)
idx_train <- sample(nrow(user_data), as.integer(nrow(user_data)*0.50))
train_data <- user_data[idx_train,]
test_data <- user_data[-idx_train,]

## Random Forest model
library(randomForest)
m_rf <- randomForest(label ~., data=train_data[,-c(1:3, 16:18)], importance=TRUE, ntree = 300)
pred_rf = predict(m_rf, test_data)
table(observed = test_data$label,predicted =pred_rf)
# predicted
# observed         0      1
# 0   370184   1990
# 1   5461   8472
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_rf)))/nrow(test_data)
accuracy
#[1] 0.9807022

#Adaboost Code using function Ada
library("rpart")
library("ada")
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
gen1 <- ada(label ~., data=train_data[,-c(1:3, 16:18)],type = "gentle", control = control, iter = 300)
gen1 <- addtest(gen1, test_data[,-c(1:3, 16:18)], test_data$label)
summary(gen1)
# Call:
#   ada(label ~ ., data = train_data[, -c(1:3, 16:18)], type = "gentle", 
#       control = control, iter = 300)
# Loss: exponential Method: gentle   Iteration: 300 
# Training Results
# Accuracy: 0.988 Kappa: NA 
# Testing Results
# Accuracy: 0.981 Kappa: NA 
varplot(gen1)
pred_ab = predict(gen1, test_data)
table(observed = test_data$label,predicted =pred_ab)
#              predicted
# observed      0      1
#       0     370120   2054
#       1      5432   8501
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_ab)))/nrow(test_data)
accuracy
#[1] 0.9806116

#Adaboost Code using function boosting
library(adabag);
adadata<- read.table("user_data.txt", sep=',', header=T, fill=T, quote="", na.strings=c("","None","NA"))
adaboost<-boosting(label ~.,data=train_data[,-c(1:3, 16:18)], boos=TRUE, mfinal=2,coeflearn='Breiman')
pred_ab = predict(adaboost, test_data)
table(observed = test_data$label,predicted =pred_ab$class)
#               predicted
# observed      0      1
#        0    370560   1614
#        1      6091   7842
accuracy = sum(diag(table(observed = test_data$label,predicted =pred_ab$class)))/nrow(test_data)
accuracy
#[1] 0.9800444
summary(adaboost)
# Length Class   Mode     
# formula         3 formula call     
# trees           2 -none-  list     
# weights         2 -none-  numeric  
# votes      772214 -none-  numeric  
# prob       772214 -none-  numeric  
# class      386107 -none-  character
# importance     12 -none-  numeric  
# terms           3 terms   call     
# call            6 -none-  call     
adaboost$trees


#Statitics - Yahui Guo
library(moments)
data=read.table("results.txt",header=F,sep = ",")
sumindividual=function(vec,t){
  l=length(vec)/t
  x=rep(0,l)
  for (i in 1:l){
    for (j in 1:t){
      x[i]=vec[t*(i-1)+j]+x[i]
    }
  }
  x
}
averagevec=function(vec,k){
  l=length(vec)
  x=rep(0,l)
  for (i in 1:l){
    x[i]=vec[i]/k
  }
  x
}
sumday=function(vec,t){
  l=length(vec)/t
  x=rep(0,t)
  for (i in 1:t){
    for(j in 1:l){
      x[i]=vec[t*(j-1)+i]+x[i]
    }
  }
  x
}
lt<-sumindividual(data[,4],30)
pt<-sumindividual(data[,5],30)
pamount<-sumindividual(data[,6],30)
avlt<-averagevec(lt,30)
avpt<-averagevec(pt,30)
avpamount<-averagevec(pamount,30)
dailylt<-sumday(data[,4],30)
dailypt<-sumday(data[,5],30)
dailypamount<-sumday(data[,6],30)
hist(lt,breaks = c(20*0:25, 1000,2100))
hist(pt,breaks = c(0:10,120))
hist(pamount,breaks=c(0:50,100,34510))
plot(dailylt,type="l")
plot(dailypt,type="l")
plot(dailypamount,type="l")
skewness(pt)
kurtosis(pt)
summary(lt)
summary(pt)
summary(pamount)
summary(avlt)
summary(avpt)
summary(avpamount)
summary(dailylt)
summary(dailypt)
summary(dailypamount)
mdata<-cbind(avlt,avpt,avpamount)
cov(mdata)
m2data<-cbind(dailylt,dailypt,dailypamount)
cov(m2data)
logdata<-data.frame(loglt=log(avlt+1),
                    logpt=log(avpt+1),
                    logpamount=log(avpamount+1))
pairs(logdata[,1:3])
fit<-lm(logpamount~loglt+logpt,data=logdata)
summary(fit)
plot(fit$residuals)

#Time Series Analysis - Cong Shi

library(fGarch)
library(fUnitRoots)
data=read.table("timeline.txt",header=F,sep = ",")
mintohour=function(vec,t){
  l=length(vec)/t
  x=rep(0,l)
  for (i in 1:l){
    for (j in 1:t){
      x[i]=vec[t*(i-1)+j]+x[i]
    }
  }
  x
}
lt=mintohour(data[,2],12)
pt=mintohour(data[,3],12)
pmt=mintohour(data[,4],12)
ts.plot(lt)
ts.plot(pt)
ts.plot(pmt)
hist(lt)
hist(pt)
hist(pmt)
acf(lt,lag.max=200)
acf(pt,lag.max=200)
acf(pmt,lag.max=200)
dlt=diff(lt,lag=24)
dpt=diff(pt,lag=24)
dpmt=diff(pmt,lag=24)
acf(dlt,lag.max=200)
acf(dpt,lag.max=200)
acf(dpmt,lag.max=200)
pacf(dlt,lag.max=200)
pacf(dpt,lag.max=200)
pacf(dpmt,lag.max=200)
adfTest(dlt,lags=24)
adfTest(dpt,lags=24)
adfTest(dpmt,lags=24)
ar(dpmt)$order
m1=arima(pmt,order=c(7,0,1),seasonal=list(order=c(3,1,1),period=24))
m1
acf(m1$residuals,lag.max=200)
pacf(m1$residuals,lag.max=200)
Box.test(m1$residuals,lag=24,type='Ljung')
tsdiag(m1,gof=24)
m2=arima(pmt,order=c(9,0,1),seasonal=list(order=c(4,1,1),period=24),fixed=c(NA,0,0,0,0,0,NA,0,NA,NA,0,0,NA,NA,NA),transform.pars = FALSE)
m2
acf(m2$residuals,lag.max=200)
pacf(m2$residuals,lag.max=200)
Box.test(m2$residuals,lag=24,type='Ljung')
Box.test(m2$residuals^2,lag=24,type='Ljung')
tsdiag(m2,gof=24)
acf(m2$residuals^2,lag.max=200)
m3=lm(dpmt~dlt+dpt)
summary(m3)
acf(m3$residuals,lag.max=200)
pacf(m3$residuals,lag.max=200)
m4=ar(m3$residuals,method='mle')
m4$order
xmatrix=cbind(dlt,dpt)
m5=arima(dpmt,order=c(9,0,0),seasonal=list(order=c(3,0,1),period=24),fixed=c(NA,NA,0,NA,0,0,NA,0,NA,NA,0,NA,NA,NA,NA),include.mean=F,xreg=xmatrix,transform.pars = FALSE)
m5
acf(m5$residuals,lag.max=200)
pacf(m5$residuals,lag.max=200)
tsdiag(m5,gof=24)
Box.test(m5$residuals,lag=24,type='Ljung')
Box.test(m5$residuals^2,lag=24,type='Ljung')
acf(m5$residuals^2,lag.max=200)
m2test=arima(pmt[1:(length(pmt)-120)],order=c(9,0,1),seasonal=list(order=c(4,1,1),period=24),fixed=c(NA,0,0,0,0,0,NA,0,NA,NA,0,0,NA,NA,NA),transform.pars = FALSE)
p1=predict(m2test,120)
plot(481:length(pmt),pmt[(481:length(pmt))],xlab="Hour", ylab="pmt",type="l")
lines((length(pmt)-119):length(pmt),p1$pred,col="red",lty=5)
lines((length(pmt)-119):length(pmt),p1$pred+0.5*p1$se,col="darkviolet",lty=2)
lines((length(pmt)-119):length(pmt),p1$pred-0.5*p1$se,col="blue",lty=2)
p2=predict(m2,120)
void=rep(-Inf,120)
plot(601:(length(pmt)+120),c(pmt[(601:length(pmt))],void),xlab="Hour", ylab="pmt",type="l")
lines((length(pmt)+1):(length(pmt)+120),p2$pred,col="red",lty=1)
lines((length(pmt)+1):(length(pmt)+120),p2$pred+0.5*p2$se,col="darkviolet",lty=2)
lines((length(pmt)+1):(length(pmt)+120),p2$pred-0.5*p2$se,col="blue",lty=2)
dslt=dlt[1:(length(dlt)-72)]
dspt=dpt[1:(length(dpt)-72)]
dspmt=dpmt[1:(length(dpmt)-72)]
dtlt=dlt[(length(dlt)-71):length(dlt)]
dtpt=dpt[(length(dpt)-71):length(dpt)]
xsmatrix=cbind(dslt,dspt)
xtmatrix=cbind(dtlt,dtpt)
m5test=arima(dspmt,order=c(9,0,0),seasonal=list(order=c(3,0,1),period=24),fixed=c(NA,NA,0,NA,0,0,NA,0,NA,NA,0,NA,NA,NA,NA),include.mean=F,xreg=xsmatrix,transform.pars = FALSE)
p3=predict(m5test,72,newxreg=xtmatrix)
plot(601:length(dpmt),dpmt[(601:length(dpmt))],xlab="Hour", ylab="dpmt",type="l")
lines((length(dpmt)-71):length(dpmt),p3$pred,col="red",lty=5)
lines((length(dpmt)-71):length(dpmt),p3$pred+0.5*p3$se,col="darkviolet",lty=2)
lines((length(dpmt)-71):length(dpmt),p3$pred-0.5*p3$se,col="blue",lty=2)


