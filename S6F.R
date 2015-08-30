# This code generates figure S6F and calculates basic statistics on the lifespan differences between male and females 
# at the peak marker.

library(survival)

gpeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Oct2014/m_peakG.csv', sep=',', header=TRUE)
gpeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Oct2014/f_peakG.csv', sep=',', header=TRUE)

gpeak_m$status <- rep(1, length(gpeak_m$X))
gpeak_f$status <- rep(1, length(gpeak_f$X))

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

dev.off()

# Statistics for male and female survival at the peak marker
gpeak_m_peak <- subset(gpeak_m[1:3])
gpeak_m_peak$X46347 <- gpeak_m$X46347
gpeak_f_peak <- subset(gpeak_f[1:3]) 
gpeak_f_peak$X46347 <- gpeak_f$X46347

# Subsetting by genotype both male and female data
gpm0 <- subset(gpeak_m_peak,X46347!=0) 
gpm1 <- subset(gpeak_m_peak,X46347!=1)
gpm2 <- subset(gpeak_m_peak,X46347!=2)

gpf0 <- subset(gpeak_f_peak,X46347!=0) 
gpf1 <- subset(gpeak_f_peak,X46347!=1)
gpf2 <- subset(gpeak_f_peak,X46347!=2)

survdiff(formula=Surv(X.2,X.1)~X46347, data=gpm0)
survdiff(formula=Surv(X.2,X.1)~X46347, data=gpm1)
survdiff(formula=Surv(X.2,X.1)~X46347, data=gpm2)

survdiff(formula=Surv(X.2,X.1)~X46347, data=gpf0)
survdiff(formula=Surv(X.2,X.1)~X46347, data=gpf1)
survdiff(formula=Surv(X.2,X.1)~X46347, data=gpf2)


# Counting how many individuals for each genotype
# males
m_ss <- subset(gpeak_m_peak, gpeak_m_peak$X46347==0)
median(m_ss$X.2)
length(m_ss$X.2)
m_sl <- subset(gpeak_m_peak, gpeak_m_peak$X46347==1)
median(m_sl$X.2)
length(m_sl$X.2)
m_ll <- subset(gpeak_m_peak, gpeak_m_peak$X46347==2)
median(m_ll$X.2)
length(m_ll$X.2)

# females
f_ss <- subset(gpeak_f_peak, gpeak_f_peak$X46347==0)
median(f_ss$X.2)
length(f_ss$X.2)
f_sl <- subset(gpeak_f_peak, gpeak_f_peak$X46347==1)
median(f_sl$X.2)
length(f_sl$X.2)
f_ll <- subset(gpeak_f_peak, gpeak_f_peak$X46347==2)
median(f_ll$X.2)
length(f_ll$X.2)
