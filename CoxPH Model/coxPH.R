#install.packages("survival")
library(survival)
setwd("D:\\Code_Factory\\surv_anly\\2")
fmh <- read.table("framingham.csv",sep = ",", header = T)[,-1]
fmh$age2 <- (fmh$age^2)

Y = Surv(fmh$time_outcome, fmh$outcome==2)
model1 = coxph(Y ~ cursmoke + female + totchol + age +
               age^2 + bmi + BPVar + heartrte + glucose, data = fmh)
summary(model1)

Y1 = Surv(fmh$time_outcome, fmh$outcome==2)
model1 = coxph(Y1 ~ cursmoke + female + totchol + age +
                 age2 + bmi + BPVar + heartrte + glucose, data = fmh)

Y2 = Surv(fmh$time_outcome, fmh$outcome==2)
model2 = coxph(Y2 ~ cursmoke + female + totchol + age +
                 age2 + bmi + BPVar + heartrte + glucose + 
                 cursmoke * female, data = fmh)

Y3 = Surv(fmh$age,fmh$age+fmh$time_outcome,fmh$outcome==2)
model3 = coxph(Y3 ~ cursmoke + female + totchol + bmi + BPVar + 
                 heartrte + glucose, data = fmh)

summary(model1)
summary(model3)

# model1 99% CI
l1 = exp(model1[["coefficients"]][["cursmoke"]] -
           qnorm(1 - (1 - 0.99)/2) * sqrt(model1[["var"]][1,1]))
u1 = exp(model1[["coefficients"]][["cursmoke"]] +
           qnorm(1 - (1 - 0.99)/2) * sqrt(model1[["var"]][1,1]))
print(paste0("Model 1 99% CI: [",as.character(l1) ,
             ",", as.character(u1), "]"))

# model3 99% CI
l3 = exp(model3[["coefficients"]][["cursmoke"]] -
           qnorm(1 - (1 - 0.99)/2) * sqrt(model1[["var"]][1,1]))
u3 = exp(model3[["coefficients"]][["cursmoke"]] +
           qnorm(1 - (1 - 0.99)/2) * sqrt(model1[["var"]][1,1]))
print(paste0("Model 3 99% CI: [",as.character(l3) ,
             ",", as.character(u3), "]"))

summary(model2)

est1 = model2[["coefficients"]][["female"]] + 
  model2[["coefficients"]][["cursmoke:female"]]
exp(est1)
sd1 = sqrt(model2[["var"]][2,2] + 
             model2[["var"]][10,10]+2*model2[["var"]][2,10])
# model2 99% CI 1
l2.1 = exp(est1 - qnorm(1 - (1 - 0.99)/2) * sd1)
u2.1 = exp(est1 + qnorm(1 - (1 - 0.99)/2) * sd1)
print(paste0("Model 2 99% CI 1: [",as.character(l2.1) ,
             ",", as.character(u2.1), "]"))

est2 = model2[["coefficients"]][["female"]]
exp(est2)
sd2 = sqrt(model2[["var"]][2,2])
# model2 99% CI 1
l2.2 = exp(est2 - qnorm(1 - (1 - 0.99)/2) * sd2)
u2.2 = exp(est2 + qnorm(1 - (1 - 0.99)/2) * sd2)
print(paste0("Model 2 99% CI 1: [",as.character(l2.2)
             , ",", as.character(u2.2), "]"))

# fit model 1 stratified by cursmoke to get the adjusted survival curves  
model1s = coxph(Y1 ~ strata(cursmoke) + female + totchol + age +
                  age2 + bmi + BPVar + heartrte + glucose, data = fmh)

pattern = data.frame(female = mean(fmh$female), totchol = mean(fmh$totchol),
                     age = mean(fmh$age), age2 = mean(fmh$age2), bmi = mean(fmh$bmi),
                     BPVar = mean(fmh$BPVar), heartrte = mean(fmh$heartrte),
                     glucose = mean(fmh$glucose))


summary(survfit(model1s, newdata = pattern))

plot(survfit(model1s, newdata = pattern), conf.int=F,
     lty = c("solid", "dashed"), col=c("black","grey"),
     main = "Survival curves for cursmoke, adjusted for other's overall means",
     ylim = c(0.75,1))
legend("topright", c("nonsmoker", "smoker"), lty=c("solid","dashed"),
       col=c("black","grey"))