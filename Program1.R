members <- read.csv("Members.csv")
members$Sex <- as.numeric(members$Sex)
members$AgeAtFirstClaim <- as.numeric(members$AgeAtFirstClaim)


hospital.y2 <- read.csv("DaysInHospital_Y2.csv")

hospital.y3 <- read.csv("DaysInHospital_Y3.csv")

err <- function(obs, pred){
sqrt( 1/length(obs) * sum((log(pred+1) - log(obs+1))^2))
}

submission <- read.csv("Target.csv")

# members is all members
# hospital.(2,3,4) includes fewer members


#making training set
training <- merge(members,hospital.y2, by="MemberID", all.y=TRUE,sort=FALSE)

#modeling
linear.time <- lm(log1p(DaysInHospital) ~ Sex + AgeAtFirstClaim + ClaimsTruncated, data = training)


#making validation set
validation <- merge(members,hospital.y3, by="MemberID", all.y=TRUE,sort=FALSE)

#predicting
#using training model on Y3 data, should have 71435 obs
my.prediction <- predict(linear.time,validation)

#error
err(my.prediction,hospital.y3$DaysInHospital)

# linear model, age/sex/trunc
# .5479141
# linear model, logDays age/sex/trunc
# .4819708
# linear model, logDays age/age2/sex/trunc

#making submission set
submission.dataset <- merge(submission,members , by="MemberID", all.x=TRUE,sort=FALSE)

#filling in DaysInHospital
submission.prediction <- predict(linear.time,submission.dataset)

submission$DaysInHospital <- submission.prediction