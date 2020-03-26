Boostrapping


#Loading necessary libraries
library(haven)
library(foreign)
library(dummy)
mazedata1 <- read_dta("C:/Users/isabe/Downloads/mazedata1.dta")

#Analazing the data
str(mazedata1)
summary(mazedata1)
names(mazedata1)


#Separating into treatment and control
Rev <- which(mazedata1$treatment ==1)    #Treatment
noRev <- which(mazedata1$treatment ==2)   #Control 

#Creating a storage vector for the boostrap
storage <- c(1:10000)


#Creating a foor loop that will boostratp the samples with replacement
for (i in 1:10000){
  Rev_sample <- mazedata1$round1[sample(Rev,length(Rev), replace = TRUE)]
  noRev_sample <- mazedata1$round1[sample(noRev,length(noRev), replace = TRUE)]
  
  storage[i] <- mean(Rev_sample)  -  mean(noRev_sample)
}


# 95% Confindence intervals
quantile(storage, probs=c(0.025,0.975))

#Confidence Interval
model <- lm(round1~treatment, data=mazedata1)
confint(model)[2,]

# a) A table with the relevant results (bounds on the 2 confidence intervals).
data.frame("simulation" = quantile(storage, c(0.025, 0.975)), "analytical" = confint(model)[2,])


#b Histogram
#histogram (properly labeled) showing your bootstrap-sample results. How you do this one is up to you.
hist(storage, main = "Bootstrapped Values for the Treatment Effect", 
     xlab = "Treatment Effect", ylab = "Frequent", col= 'chocolate')
     
     
#Question 5   Cross Validation
library(boot)

foo <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
#Structure of the data
str(foo)  #14117 observations of 12 variables.

foo$treat
set.seed(12345)
test_set_rows <- sample(1:length(foo$education), 2000, replace =FALSE)
#Separting into train and test set
test_set <- foo[test_set_rows,]
train_set <- foo[-test_set_rows,]

#Structure of the train_set
str(train_set)

#Surname : Vitorio. Hence, using the last column(T-Z)


##Constructing the simple model
smodel <- lm(treat~education, data = train)

#Cross Validation using K-fold for the simple model
set.seed(1632) 
cv.error.10=rep(0,10) 

for (i in 1:10){ 
  glm.fit=glm(treat~education,data=train_set, family = binomial) 
  cv.error.10[i]<- cv.glm(train,glm.fit ,K=10)$delta[1]
} 
sk<- cv.error.10[1] #cross-validation estimate for the test error is approximately 0.0798

#Cross Validation via LOOCV for the simple model
glm.fit=glm(treat~education,data=train_set) 
cv.error <- cv.glm(train,glm.fit)
sv <- cv.error$delta[1] #cross-validation estimate for the test error is approximately 0.0798578
sv   #0.07985758

#Test Error rate for the simple model
MSE1 = mean((test_set$treat - predict.glm(glm.fit, test_set))**2)
MSE1  ## 0.01037194

##Constructing the complex model
cmodel <- glm(treat~age +education + black + hispanic + married + nodegree + 
                re75 +u74 + u75 + re74*re75,data= train_set, family = binomial)

summary(cmodel)

#Cross Validation using K-fold for the complex model
set.seed(1632) 
cv.error.10=rep(0,10) 

for (i in 1:10){ 
  glm.cfit=glm(treat~age +education + black + hispanic + married + nodegree + 
                 re75 +u74 + u75 + age*education,data=train_set, family = binomial) 
  cv.error.10[i]<- cv.glm(train_set,glm.cfit ,K=10)$delta[1]
} 
ck2<- cv.error.10[1] #cross-validation estimate for the test error is approximately 
ck2  #0.00802981

#Test error set for the complex model

MSE2 = mean((test_set$treat - predict.glm(glm.cfit, test_set))^2)
MSE2 ## 70.08081
#Cross Validation via LOOCV for the complex model
glm.fitc <-glm(treat~age +education + black + hispanic + married + nodegree + 
                 re75 +u74 + u75 + re74*re75,data= train_set, family = binomial) 
cv.errorp <- cv.glm(train_set,glm.fitc)
ck<- cv.errorp$delta[1]#cross-validation estimate for the test error is approximately 
ck    # 0.079



#Combining the results in a dataframe.
data.frame("K-folds" = c(sk,ck2), "Test set error"= c(MSE1,MSE2),"LOOCV"= c(sv,ck))
