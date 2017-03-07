abdc<-read.csv('W:/GWU1.0/data mining/project/ab.csv')
#feature engineering, measure the length of all kinds of description information provided by host
abdc$summary<-as.character(abdc$summary)
for (i in 1:n){
  abdc$lenofsum[i]<-length(strsplit(abdc$summary[i]," ")[[1]])
}

abdc$space<-as.character(abdc$space)
for (i in 1:n){
  abdc$lenofspa[i]<-length(strsplit(abdc$space[i]," ")[[1]])
}

abdc$description<-as.character(abdc$description)
for (i in 1:n){
  abdc$lenofdes[i]<-length(strsplit(abdc$description[i]," ")[[1]])
}
#use 1 and 0 to indicate whether host provide medium and thumbnail.
for (i in 1:n){
  if (abdc$medium_url[i]==""){
    abdc$medim[i]<-0}else{abdc$medim[i]<-1}
}
for (i in 1:n){
  if (abdc$thumbnail_url[i]==""){
    abdc$thn[i]<-0}else{abdc$thn[i]<-1}
}

abdc<-subset(abdc,select=-c(9,10))
colnames(abdc)

abdc$host_about<-as.character(abdc$host_about)
for (i in 1:3720){
  abdc$lenofhost[i]<-length(strsplit(abdc$host_about[i]," ")[[1]])
}
#delete the listing have no resonse_time and rate( which properly mean they have near been asked/ interested by tourists)
abdcsub<-abdc[!abdc$host_response_time=='N/A',]
abdcsub<-abdcsub[!abdcsub$host_response_rate=='N/A',]
abdcsub<-abdcsub[!abdcsub$host_acceptance_rate=='N/A',]

#transfer the response rate and acceptance rate into integer
temp<-as.character(abdcsub$host_response_rate)
temp <- gsub("%","",temp)#as.numeric went wrong because it contains % in the character
abdcsub$host_response_rate<-as.numeric(temp)

temp<-as.character(abdcsub$host_acceptance_rate)
temp <- gsub("%","",temp)#as.numeric went wrong because it contains % in the character
abdcsub$host_acceptance_rate<-as.numeric(temp)

as<-abdcsub
#measure how many way of verifications are used by the host(to make them more reliable.
n<-dim(as)[[1]]
as$host_verifications<-as.character(as$host_verifications)
for (i in 1:n){
  as$lenofhostver[i]<-length(strsplit(as$host_verifications[i],",")[[1]])
}
#checking the variable that might not work as perfect grouping since it's very unbalanced
summary(as$host_has_profile_pic)#f:5t:3088#maybe delete the f one?
summary(as$host_identity_verified)#f:757 t:2336
#combined the levels that have too few listing in to other.
summary(as$property_type)
as$property_type[as$property_type %in% c('Boat','Bungalow','Cabin','Dorm','Other')]<-'Other'
summary(as$room_type)#entire:prive:shared=2022:987:84
#use 0 and 1 indicate whether the host require deposit or not, same as cleaning fee, extra person fee, #min_night requirement
for (i in 1:n){
  if (as$security_deposit[i]==""){
    
    as$sede[i]<-0}else{as$sede[i]<-1}
}
as$sede<-as.factor(as$sede)
table(as$sede)#compare if there is any difference between depose requirement 

for(i in 1:n){
  if (as$cleaning_fee[i]==""){
    as$cleanfee[i]<-0}else{as$cleanfee[i]<-1}
}
as$cleanfee<-as.factor(as$cleanfee)
table(as$cleanfee)

no<-as$extra_people[1]
as$extra_people<-as.character(as$extra_people)
for(i in 1:n){
  if (as$extra_people[i]==no){
    as$extrafee[i]<-0}else{as$extrafee[i]<-1}
}
as$extrafee<-as.factor(as$extrafee)
table(as$extrafee)

for(i in 1:n){
  if (as$minimum_nights[i]==1){
    as$minnight[i]<-'1'}else{as$minnight[i]<-">1"}
}
as$minnight<-as.factor(as$minnight)
table(as$minnight)

write.csv(as,file='W:/GWU1.0/data mining/project/cleaning1.csv')
as2<-read.csv('W:/GWU1.0/data mining/project/cleaning1.csv')

as2$extrafee[is.na(as2$extrafee)]<-0
as2$minnight[as$minimum_nights==1]<-1
as2$minnight[as$minimum_nights!=1]<-'>1'

as2$reviewamount<-as.character(as2$reviewamount)
as2$reviewamount[is.na(as2$reviewamount)]<-'morethan 1'
as2$price<-as.numeric(as$price)

as2$reviews_per_month[!is.na(as2$reviews_per_month)]<-as.numeric(as2$reviews_per_month[!is.na(as2$reviews_per_month)])
write.csv(as2,file='W:/GWU1.0/data mining/project/cleaningdone.csv')

##########################
ab<-read.csv('W:/GWU1.0/data mining/project/cleaningdone.csv')
#select the columns that really apply to our question
c<-c(15:18,22:25,30:36,39,44:46,49,51,54:63,66:81)
abs1<-ab[,c]
abs1$extrafee<-as.factor(abs1$extrafee)
abs1$sede<-as.factor(abs1$sede)
abs1$cleanfee<-as.factor(abs1$cleanfee)
abs1$medim<-as.factor(abs1$medim)
abs1$thn<-as.factor(abs1$thn)
abs1$bathrooms[is.na(abs1$bathrooms)]<-0
abs1$bedrooms[is.na(abs1$bedrooms)]<-0
abs1$beds[is.na(abs1$beds)]<-0
#delete the listing that are not available in a whole year and hosts accept no request
abs1<-abs1[abs1$availability_365!=0,]
abs1<-abs1[abs1$host_acceptance_rate!=0,]
abs1<-abs1[,-29]
abs2<-abs1[!(is.na(abs1$review_scores_checkin)),]#no NA set
abs3<-abs1[,-c(22:28)]
abs3<-abs3[,-c(31:32)]

#to study the price use abs3
#to study the reviews use abs2
abs3$extra_people <- as.numeric(gsub('[$]', '', abs3$extra_people))

##regression
Set.seed(1234)
dt = sort(sample(nrow(abs3), nrow(abs3)*.7))
train<-abs3[dt,]
test<-abs3[-dt,]
fitlm<-glm(log(price)~neighbourhood_cleansed+property_type+room_type+accommodates+bathrooms
           +bedrooms+beds+bed_type+guests_included+minimum_nights+extra_people+availability_365+
             number_of_reviews+instant_bookable+cancellation_policy+reviews_per_month,data=train)
summary(fitlm)
predictlml<-predict(fitlm,test)
plot(exp(predictlml),test$price)

y<-test$price
SST <- sum((y - mean(y))^2)
SSR <- sum((predictlml - mean(y))^2)
SSE <- SST - SSR # 7022540
MSE=SSE/889#7899.37

#use ABS3, decision tree
library(rpart)
fit <- rpart(price ~ .,
             data=train,
             method="anova")
print(fit)
printcp(fit)
summary(fit)
Prediction <- predict(fit, test, type = "vector")
plot(Prediction,test$price)
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results 
#plotting the decision tree
plot(fit, uniform=TRUE, 
     main="Regression Tree ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

##how well it work
y<-test$price
SST <- sum((y - mean(y))^2)
SSR <- sum((Prediction - mean(y))^2)
SSE <- SST - SSR 
MSE=SSE/889
#random forest
library(randomForest)
abs3$reviews_per_month[is.na(abs3$reviews_per_month)]<-0
abs3<-na.omit(abs3)
train<-abs3[dt,]
test<-abs3[-dt,]
fitrf <- randomForest(price ~.,   data=train, ntree=500)
print(fitrf) # view results 
importance(fitrf) # importance of each predictor
varImpPlot(fitrf)
test$pred_price<-predict(fitrf,test)
plot(test$pred_price,test$price)
#test the SSE
y<-test$price
SST <- sum((y - mean(y))^2)
SSR <- sum((test$pred_price - mean(y))^2)
SSE <- SST - SSR # SSE <- sum(resid(softdrink.lm)^2)
MSE=SSE/889

#work with abs2, review/attractness
summary(abs2)
abs2$extra_people <- as.numeric(gsub('[$]', '', abs2$extra_people))
summary(abs2$number_of_reviews)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    3.00    9.00   21.63   25.00  362.00 
boxplot(abs2$number_of_reviews)
abs2$attract <- 'super' #the listing is super popular
abs2$attract[abs2$number_of_reviews < 25 & abs2$number_of_reviews >= 9] <- '9-25'# the list is popular than half of the listing
abs2$attract[abs2$number_of_reviews < 9] <- '<9'# the list is not that attractive
abs2$attract<-as.factor(abs2$attract)

dt = sort(sample(nrow(abs2), nrow(abs2)*.7))
train<-abs2[dt,]
test<-abs2[-dt,]

#decision tree
library(rpart)
set.seed(1234)
#prevent the availability affects the measure and delete the review amounts related columns
abs2<-abs2[abs2$availability_365>=200,]
abs4<-abs2[,-6]
abs4<-abs4[,-20]
abs4<-abs4[,-30]
train<-abs4[dt,]
test<-abs4[-dt,]

fit2 <- rpart(attract ~.,
              data=train,
              method="class")
print(fit2)
printcp(fit2)
summary(fit2)
plot(fit2, uniform=TRUE, 
     main="Classification Tree ")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)

test$Prediction <- predict(fit2, test, type = "class")
#check how well the model works
confusionMatrix(test$Prediction, test$attract)

# try to prune the tree but the plot remained the same
rt.pruned <- prune(fit2,cp = min.xerror) 
plot(rt.pruned, uniform=TRUE, 
     main="Classification Tree ")
text(rt.pruned, use.n=TRUE, all=TRUE, cex=.8)

par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit2) # visualize cross-validation results 

#try random forest
library(randomForest)
#since random forest can only take 32 variables, delete some columns that might not be that important #manully
train1<-train[,-c(35,36,39,43,42,5,6,7,41,40,38)]
fitrf2 <- randomForest(as.factor(attract) ~.,   data=train1, ntree=800)
print(fitrf2) # view results 
importance(fitrf2) # importance of each predictor
varImpPlot(fitrf2)
test$pred_attrac<-predict(fitrf2,test)

confusionMatrix(test$pred_attrac, test$attract)
