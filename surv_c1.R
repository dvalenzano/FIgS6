library(survival)

### SURVIVAL DIFFERENCES AMONG F1 MALES AND FEMALES IN CROSS 1 #######
c1ph <- read.table(file="/Volumes/group_dv/personal/DValenzano/Feb2015/c1_f1surv.txt", header=T)
c1ph[1:10,]
c1phf1 <- subset(c1ph, generation=="F1")
c1phf1$status <- c(rep(1, length(c1phf1$days)))
c1phf1[1:10,]

a <- survdiff(Surv(days, status) ~ gender, data=c1phf1) 
plot(survfit(Surv(days, status) ~ gender, data = c1phf1), xlab = "days", ylab = "fraction alive", lwd = c(2.5,2.5), col=c("red", "blue"), main="cross 1 F1 survival, p= 0.00183")
legend(50,.4, legend=c("females","males"), lwd=c(2.5,2.5), col=c("red", "blue"))
a
# survdiff(formula = Surv(days, status) ~ gender, data = c1phf1)

# N Observed Expected (O-E)^2/E (O-E)^2/V
# gender=f 8        8      3.5      5.79      9.71
# gender=m 8        8     12.5      1.62      9.71

# Chisq= 9.7  on 1 degrees of freedom, p= 0.00183

###### GRZ SURVIVAL, MALES AND FEMALES #######

grz <- read.csv(file='/Volumes/group_dv/personal/DValenzano/Feb2015/Surv_Stanford/grz2_survivial.csv', header=TRUE)
grz <- subset(grz, group2 != '3')
grz <- subset(grz, group2 != '4')
names(grz) <- c("X", "pop", "status", "days", "gender")

b <- survdiff(Surv(days, status) ~ gender, data=grz) 
plot(survfit(Surv(days, status) ~ gender, data = grz), xlab = "days", ylab = "fraction alive", lwd = c(2.5,2.5), col=c("red", "blue"), main="GRZ F1 survival, p= 0.824")
legend(200,.8, legend=c("females","males"), lwd=c(2.5,2.5), col=c("red", "blue"))
b

###### MZM0703 SURVIVAL, MALES AND FEMALES #######

mzm0703 <- read.table(file='/Volumes/group_dv/personal/DValenzano/Feb2015/mzm-0703.txt', header=TRUE)
mzm0703 <- subset(mzm0703, gender != "na")
c <- survdiff(Surv(days, status) ~ gender, data=mzm0703) 
plot(survfit(Surv(days, status) ~ gender, data = mzm0703), xlab = "days", ylab = "fraction alive", lwd = c(2.5,2.5), col=c("red", "blue"), main="MZM-0703 F1 survival, p= 0.924")
legend(200,.8, legend=c("females","males"), lwd=c(2.5,2.5), col=c("red", "blue"))
c

###### SAVE WORKSPACE ######
save.image("/Volumes/group_dv/personal/DValenzano/Feb2015/surv_01-Feb-2015.RData")
