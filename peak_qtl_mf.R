gpeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Oct2014/m_peakG.csv', sep=',', header=TRUE)
gpeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Oct2014/f_peakG.csv', sep=',', header=TRUE)

gpeak_m$status <- rep(1, length(gpeak_m$X))
gpeak_f$status <- rep(1, length(gpeak_f$X))

library(survival)

#46347 is the peak marker according to the random forest analysis.

gms_46347 <- subset(gpeak_m, X46347=='2')
gfs_46347 <- subset(gpeak_f, X46347=='2')
gmf_46347 <- rbind(gms_46347,gfs_46347)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X46347, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker (46347)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X46347, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker (46347)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_46347)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker (46347)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
