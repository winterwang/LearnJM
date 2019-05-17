
# Page 57 -----------------------------------------------------------------

library(JM)

head(aids)

td.Cox <- coxph(Surv(start, stop, event) ~ drug + CD4, data = aids)

td.Cox


# Page 58 -----------------------------------------------------------------

aids.id

lmeFit.aids <- lme(CD4 ~ obstime + obstime:drug, 
                   random = ~ obstime | patient, data = aids)
coxFit.aids <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)

jointFit.aids <- jointModel(lmeFit.aids, coxFit.aids, timeVar = "obstime", 
                            method = "piecewise-PH-aGH")
summary(jointFit.aids)

# dynamic prediction GIF


# We are interested in producing predictions of survival probabilities for Patient 155
dataP110 <- aids[aids$patient == 3, ]
len_id <- nrow(dataP110)


# We can plot the data
sfit3 <- survfitJM(jointFit.aids, idVar = "patient",  newdata = dataP110[1:2, ]) 
sfit4 <- survfitJM(jointFit.aids, idVar = "patient",  newdata = dataP110[1:3, ]) 



par(mfrow=c(1,2))
plotfit3 <- plot(sfit3, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 10")
plotfit4 <- plot(sfit4, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 10")


library(animation)
saveGIF({
  for(i in c(1:len_id)){
    sfit <- survfitJM(jointFit.aids, idVar = "patient", newdata = dataP110[1:i, ]) 
    plot(sfit, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 3")
    
  }
},ani.width = 400, ani.height=400)

# try to use the JMbayes package 
library(JMbayes)

MixedModelFit1 <- mvglmer(list(CD4 ~  obstime + obstime:drug + (obstime | patient)), data = aids,
                          families = list(gaussian))

coxFit.aids <- coxph(Surv(Time, death) ~ drug, data = aids.id, model = TRUE)

JMFit1 <- mvJointModelBayes(MixedModelFit1, coxFit.aids, timeVar = "obstime")
