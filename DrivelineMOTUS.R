Motus <- read.csv("MotusData.csv") #read file

require(leaps)
require(MASS)

#var_sel <- regsubsets(MPH ~ ArmSlot + ArmSpd + sRotation + Stress, data = Motus, method = "backward") #variable selectio
#summary(var_sel) #ArmSpd removed

model <- lm(MPH ~ ArmSlot + Stress, data = Motus) 
#summary(model) #sRot removed
Motus$xMPH <- predict(model,Motus,interval="confidence")

#athlete subsets

athlete_one <- subset(Motus, pID == 1 & throwID <= 20)
athlete_two <- subset(Motus, pID == 2 & throwID <= 40)
athlete_three <- subset(Motus, pID == 3 & throwID <= 60)
athlete_four <- subset(Motus, pID == 4 & throwID <= 80)
athlete_five <- subset(Motus, pID == 5 & throwID <= 100)

#boxplot with min/max

boxplot(xMPH ~ pID, data=Motus, las=3, horizontal=TRUE, xLAB = "xMPH")

#confidence interval graph

require(plotrix)
with(Motus, plotCI(throwID, fit, ui=upr, li=lwr, xlab = "Throw", ylab = "xMPH"))

#mph vs xmph 

with(athlete_one, plot(throwID, fit, type="l", lty=1, lwd=1,
     xlab="throw", ylab = "xMPH",
     xlim=c(0,100), ylim=c(65,90)))
with(athlete_two, lines(throwID, fit, lty=2,lwd=1))
with(athlete_three, lines(throwID, fit, lty=3, lwd=1))
with(athlete_four, lines(throwID, fit, lty=4,lwd=1))
with(athlete_five, lines(throwID, fit, lty=5, lwd=1))