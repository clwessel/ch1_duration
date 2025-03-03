# PSE experiment (N=60)

# initialize ---------
graphics.off()
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )

options(contrasts=c("contr.sum","contr.poly"))
library(nlme)
library(lme4)
library(lattice)

dat60 <- read.csv("heterogenous_regression_60.csv",stringsAsFactors = T)
dat60$repetition <- as.factor(dat60$repetition)
dat60$Participant <- factor(dat60$Participant,labels ="p")
summary(dat60)
nr <- dim(dat60)[1]
dat60$standard <- rep(0,nr)
dat60$pse <- rep(0,nr)
tmp <- dat60$envelope_1=="flat"
dat60$standard[tmp] <- dat60$Flat[tmp]
dat60$pse[tmp] <- dat60$Percussive[tmp]



tmp <- dat60$envelope_1=="perc"
dat60$standard[tmp] <- dat60$Flat[tmp]
dat60$pse[tmp] <- dat60$Percussive[tmp]

# create a "groupedData" data frame allows us to use augPred command
dat60 <- groupedData(formula=pse~standard|Participant,data=dat60)



# homogeneous variance models ---------
# create model assuming error variance is constant across all levels of standard duration
summary(dat60)

all.lme.01 <- lme(fixed=pse~standard*envelope_1,random=~1+standard|Participant,data=dat60)
anova(all.lme.01)

# dropping non-significant effects
all.lme.02 <- lme(fixed=pse~standard,random=~1+standard|Participant,data=dat60)
anova(all.lme.02)


# compare models using AIC & BIC
anova(update(all.lme.01,method="ML"),
      update(all.lme.02,method="ML")) # model 02 is better

# first check the full model:
plot(augPred(all.lme.01),main="all data full model 01")
coef(summary(all.lme.01)) # values of fixed effects
anova(summary(all.lme.01)) # anova fixed effects
VarCorr(all.lme.01) # variance of random effects
# SD(Intercept) = 121.8, SD(slope) = 0.349
# coef(all.lme.01) # coefficients (fixed+random) for individual subjects
# ranef(all.lme.01) # lists values of random effects for individual subjects

# evaluate statistical significance of random effects
all.lm.01 <- gls(pse~standard*envelope_1,data=dat60) # create a fixed-effects model with no random effects
anova(all.lm.01,all.lme.01) # mixed-effects model is better

# check residuals:
# these look normal to me:
qqnorm(residuals(all.lme.01,type="p"),main="all data (full model) N=60")
qqline(residuals(all.lme.01,type="p"))
boxplot(residuals(all.lme.01,type="p"),main="all data (full model) N=60")
plot(all.lme.01,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)


# now check "best-fitting" model 02:
plot(augPred(all.lme.02),main="all data (full model 02) N=60") # model 02 is best
coef(summary(all.lme.02)) # values of fixed effects
anova(summary(all.lme.02)) # values of fixed effects
VarCorr(all.lme.02) # variance of random effects
# SD(Intercept) = 120.8, SD(slope) = 0.336
# coef(all.lme.02) # coefficients for individual subjects
# ranef(all.lme.02) # lists values of random effects for individual subjects

# evaluate statistical significance of random effects
all.lm.02 <- gls(pse~standard,data=dat60) # create a fixed-effects model with no random effects
anova(all.lm.02,all.lme.02) # mixed-effects model is better

# check residuals:
# these look normal to me:
qqnorm(residuals(all.lme.02, type = "p"),main="all data (best-fit model 02) N=60")
qqline(residuals(all.lme.02, type = "p"))
boxplot(residuals(all.lme.02, type = "p"),main="all data (best-fit model 02) N=60")
plot(all.lme.02,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)


# heteroscedastic model --------
# re-fit data using models that allow variance of residuals to change with fitted values

all.lme.hetero.01 <- update(all.lme.01,weights=varPower(form = ~fitted(.)) )
anova(all.lme.01,all.lme.hetero.01) # hetero model fits better

anova(all.lme.hetero.01)

# dropping non-significant interaction:
all.lme.hetero.02 <- update(all.lme.01,fixed=pse~standard+envelope_1 )
anova(update(all.lme.hetero.02,method="ML"),update(all.lme.hetero.01,method="ML"))
# model 01 (full model) is better

# fit intercept-only model
all.lme.hetero.01b <- update(all.lme.hetero.01,random=~1|Participant)
anova(all.lme.hetero.01b,all.lme.hetero.01) # intercept+slope model is better


# evaluate statistical significance of random effects
all.lm.hetero.01 <- gls(pse~standard*envelope_1,weights=varPower(form = ~fitted(.)),data=dat60)
anova(all.lm.hetero.01,all.lme.hetero.01) # mixed-effects model is better





plot(augPred(all.lme.hetero.01),main="all data (hetero full model 01)")
coef(summary(all.lme.hetero.01)) # values of fixed effects
anova(all.lme.hetero.01) # anova fixed effects
VarCorr(all.lme.hetero.01) # variance of random effects
# SD(Intercept) = 166.05, SD(slope) = 0.345

# coef(all.lme.hetero.01) # coefficients (fixed+random) for individual subjects
# ranef(all.lme.hetero.01) # lists values of random effects for individual subjects

# check residuals:
# these look normal to me:
qqnorm(residuals(all.lme.hetero.01, type = "p"),main="all data (hetero full model 01) N=60")
qqline(residuals(all.lme.hetero.01, type = "p"))
boxplot(residuals(all.lme.hetero.01, type = "p"),main="all data (hetero full model 01) N=60")
plot(all.lme.hetero.01,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)

# SUMMARY OF BEST-FITTING MODEL ----
best.lme.model.n60  <- all.lme.hetero.01
lineColors <- trellis.par.get("superpose.line")$col # default colors 
plot(augPred(best.lme.model.n60,level=0:1),grid=T,
     key=list(columns = 2,text=list(lab=c("fixed","participant")),
              lines=list(cex=1,lwd=2,lty=c(1,3),col=lineColors[1:2])))# plot includes fixed line and lines for individual participants
anova(best.lme.model.n60) # anova fixed effects
VarCorr(best.lme.model.n60) # variance of random effects
coef(summary(best.lme.model.n60))
# plot pse vs fitted values:
# following commands identify "outliers" (p<.05)
# not a problem here because there are only a few
plot(best.lme.model.n60,pse~fitted(.),abline=c(0,1),id=.05,adj=-0.4)
qqnorm(best.lme.model.n60, ~resid(.,type="p"),id=.05,adj=-0.4)


