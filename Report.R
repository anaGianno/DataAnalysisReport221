                                #Part 1
carSeat = read.csv(file.choose(), header = TRUE)
View(carSeat)

                  ###Detailed initial analysis of data set###

#Plot correlations
plot(carSeat, panel = panel.smooth, lwd = 2)

#Find correlations
round(cor(carSeat), 3)



    ###An initial analysis of the full model, statistical significance###

# Define the full model 
#(can use ~. to tell R to use all other variables (not response))
carSeatFullLM = lm(Sales ~., data = carSeat)
summary(carSeatFullLM)



    ###1:Details of how the predictors were chosen for the selected model###

# Stepwise regression using AIC
carSeatStepAICLM= step(carSeatFullLM, direction = "both", trace = TRUE)

# Stepwise regression using BIC
carSeatStepBICLM = step(carSeatFullLM, direction = "both", trace = TRUE, k = log(nrow(carSeat)))

### Our function to assess best models with different criteria ###
myallpossregs.func=function(model,n) {
  require(leaps)
  X=model.matrix(model)[,-1]
  y=model.response(model.frame(model))
  temp.regsubsets=regsubsets(X,y,nbest=n,names=names(X))
  
  which.mat=summary(temp.regsubsets)$which
  
  p.vec=apply(which.mat,1,sum)-1
  rsq.vec=summary(temp.regsubsets)$rsq
  adjr2.vec=summary(temp.regsubsets)$adjr2
  cp.vec=summary(temp.regsubsets)$cp
  press.vec=aic.vec=bic.vec=s.vec=c()
  
  for (i in 1:nrow(which.mat)) {
    temp.lm=lm(y~.,data=as.data.frame(X[,which.mat[i,-1]]))
    press.vec[i]=sum((residuals(temp.lm)/(1-hatvalues(temp.lm)))^2)
    aic.vec[i]=AIC(temp.lm)
    bic.vec[i]=AIC(temp.lm,k=log(nrow(X)))
    s.vec[i]=summary(temp.lm)$sigma
  }
  result.mat=cbind(p.vec,round(rsq.vec,3),round(adjr2.vec,3),round(press.vec,1),round(aic.vec,1),round(bic.vec,1),round(cp.vec,1),round(s.vec,4),which.mat[,-1]*1)
  colnames(result.mat)=c("Vars","Rsq","Rsq(adj)","PRESS","AIC","BIC","Cp","s",colnames(X))
  rownames(result.mat)=NULL
  as.data.frame(result.mat)
}
#########################################################################

# Assess model 
myallpossregs.func(carSeatFullLM,1)

# Find 'best' model with respect to single criteria
which.max(myallpossregs.func(carSeatFullLM,1)$"Rsq(adj)")
which.min(myallpossregs.func(carSeatFullLM,1)$PRESS)
which.min(myallpossregs.func(carSeatFullLM,1)$AIC)
which.min(myallpossregs.func(carSeatFullLM,1)$BIC)
which.min(myallpossregs.func(carSeatFullLM,1)$s)

# Plot Mallow's Cp with p
plot(myallpossregs.func(carSeatFullLM,1)$Vars + 1, 
     myallpossregs.func(carSeatFullLM,1)$Cp, 
     pch = 16, ylab = "Cp", xlab = "p", cex = 0.5)
abline(0, 1)



           ###2:Criteria/rationale used to determine the best model###

                 ###3:Error analysis of the chosen model###

#LM of best model
carSeatBestLM = lm(Sales ~ CompPrice + Advertising + Price + Age + Income, data = carSeat)


### Residual Plots
old.par = par(mfrow = c(2, 2))
plot(carSeatBestLM)
library(car)
old.par = par(mfrow = c(1,1))

qqPlot(carSeatBestLM[[2]],xlab = "Theoretical Quantities", ylab = "Standardized residuals", main = "Q-Q residuals")

shapiro.test(residuals(carSeatBestLM))
hist(residuals(carSeatBestLM),xlab="Residuals",main = "Histogram of Unit sales (in thousands) at each location")


    ###A conclusion of the results within the context of the problem###

summary(carSeatBestLM)

                                #Part 2
co2Uptake1 = read.csv(file.choose(), header = TRUE)
View(co2Uptake1)

co2Uptake = read.csv(file.choose(), header = TRUE)
View(co2Uptake)

nonChilled = co2Uptake1[,2] 
chilled = co2Uptake1[,1] 
both = co2Uptake1[,3] 

uptake = co2Uptake[,2]

                   ###Initial analysis of the data###


hist(both,xlab="CO2 Uptake",main = "Histogram of CO2 uptake of chilled and non chilled plants")
boxplot(both,ylab="CO2 Uptake",main = "Boxplot of CO2 uptake of chilled and non chilled plants")
summary(both)
sd(both)

###Choice of any and all tests used with justification, statement of the hypotheses,
                    ###and printout of the results###

## normality checks
library(car)
old.par = par(mfrow = c(2, 2))
hist(chilled,xlab="CO2 Uptake",main = "Histogram of CO2 uptake of chilled plants")
hist(nonChilled,xlab="CO2 Uptake",main = "Histogram of CO2 uptake of non chilled plants")
qqPlot(chilled,xlab = "Theoretical Quantities", ylab = "Standardized residuals", main = "Q-Q residuals (chilled)")
qqPlot(nonChilled,xlab = "Theoretical Quantities", ylab = "Standardized residuals", main = "Q-Q residuals (nonChilled)")

old.par = par(mfrow = c(1,1))
shapiro.test(chilled)
shapiro.test(nonChilled)

#one way to run paired t-test
help(t.test)
t.test(nonChilled, chilled, paired = TRUE,na.rm = TRUE, alternative = "two.sided")
t.test(chilled,nonChilled, paired = TRUE,na.rm = TRUE, alternative = "two.sided")



      ###A summary of the results as well as the resulting decision and conclusion###







