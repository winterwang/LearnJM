
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

jointFit.aids <- jointModel(lmeFit.aids, coxFit.aids, timeVar = "obstime")