## Data Preparation
#install.packages("survival")
library(survival)
setwd("D:\\Code_Factory\\surv_anly\\1")
ACTG175 <- read.table("ACTG175(speff2trial).txt", header = TRUE, sep = ',')
# used to indicate that all subsequent variable names apply to the ACTG175 dataset

## 1. KP curves
km <- Surv(days, cens==1)
# summary(survfit(km~arms))
plot(survfit(km~arms),ylim = c(0.6,1), col=c("red","orange","blue","green"),
     ylab = "survival probabilities", xlab="survival time in days")

legend("topright",           #The legend is in the upper right corner
       legend=c("zidovudine","zidovudine and didanosine","zidovudine and zalcitabine",
                "didanosine"),        #content of legend
       col=c("red","orange","blue","green"),         #color of legend
       lty=1,lwd=2,                                  #size of legend
       cex = 0.7, text.width = 380)

## Q2:Log-rank test

#non-weighted log-rank test
survdiff(km~arms)

# use subset = group %in% c("group1", "group2") to do test only for a subset
survdiff(km~arms, subset = arms %in% c(1,2,3))

#weighted log-rank test
survdiff(km~arms, rho = 1)

#stratified log-rank test
survdiff(km~arms + strata(hemo) + strata(homo) +
           strata(cut(age, breaks = c(0,20, 50,max(age)))))
#note: 
# use the function \verb"cut" to convert numeric variable \verb"age" into factor