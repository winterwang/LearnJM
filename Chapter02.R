
# Page 22 -----------------------------------------------------------------

library(JM)

data("aids")

aids


# Page 23 -----------------------------------------------------------------

lmeFit.int <- lme(CD4 ~ obstime, random = ~ 1 | patient, data = aids)

summary(lmeFit.int)


# Page 24 -----------------------------------------------------------------

margCov.int <- getVarCov(lmeFit.int, individuals = 12, type = "marginal")


margCov.int



# Page 25 -----------------------------------------------------------------

cov2cor(margCov.int[[1]])

lmeFit.slp <- lme(CD4 ~ obstime, random = ~ obstime | patient, data = aids)

summary(lmeFit.slp)
