
# Page 43 -----------------------------------------------------------------


library(JM)

data("pbc2.id")

pbc2.id$status2 <- as.numeric(pbc2.id$status != "alive")

coxFit <- coxph(Surv(years, status2) ~ drug + age + sex, data = pbc2.id)


summary(coxFit)


# Page 47 -----------------------------------------------------------------


head(prothro[c("id", "pro", "start", "stop", "event")], n = 3)

tdCox.pro <- coxph(Surv(start, stop, event) ~ pro + treat, 
                   data = prothro)

summary(tdCox.pro)
