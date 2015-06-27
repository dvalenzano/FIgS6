library(survival)
grz <- read.csv(file='~/Desktop/Stanford_GRZ.csv', header=TRUE)
grz
fit <- survfit(formula=Surv(days,status)~group, data = grz)

##########################################################
m<- subset(grz,group=="m")
m
mfit <- survfit(formula=Surv(days,status)~group, data = m)
plot(mfit)

f<- subset(grz,group=="f")
f
ffit <- survfit(formula=Surv(days,status)~group, data = f)
plot(ffit)
##########################################################

plot(fit, col=c(1,2,3), lwd=c(3,3,3), xlab="days",ylab="Fraction Survived", main="Survival GRZ at Stanford")
legend(200,.7, legend=c("females","males","na"), lwd=c(3,3,3), col=c(1,2,3))

days2 <- rep(grz$days,2)
status2 <- rep(grz$status, 2)
pop2<-rep(grz$population,2)
group2<-c(grz$group,rep(4, length(grz$group)))
grz2 <- data.frame(pop2, status2, days2, group2)

fit2 <- survfit(formula=Surv(days2,status2)~group2, data = grz2)

plot(fit2, col=c(1,2,3,4), lwd=c(3,3,3,3), xlab="days",ylab="Fraction Survived", main="Survival GRZ at Stanford")
legend(200,.7, legend=c("females (N= 47)","males (N= 42)","na (N= 24)", "whole (N= 113)"), lwd=c(3,3,3,3), col=c(1,2,3,4))
table(grz$group)

write.csv(grz2, file='/Volumes/group_dv/personal/DValenzano/Jan2014/Stanford_paper/grz2_survivial.csv')
