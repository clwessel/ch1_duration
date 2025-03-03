# PSE experiment (N=60)

# initialize ---------
graphics.off()
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )

options(contrasts=c("contr.sum","contr.poly"))
library(nlme)
library(lme4)
library(lattice)

combined <- read.csv("combined_exp1_2.csv", stringsAsFactors = T)
hom <- combined[combined$type == "Homogenous",]
hom$repetition <- as.factor(hom$repetition)
hom$Participant <- factor(hom$Participant, labels = "p")

# create a "groupedData" data frame allows us to use augPred command
hom <- groupedData(formula=y~x|Participant,data=hom)



# homogeneous variance models ---------
# create model assuming error variance is constant across all levels of standard duration
summary(hom)

all.lme.01 <- gls(y~x*envelope_1,data=hom)
anova(all.lme.01)

# dropping non-significant effects
all.lme.02 <- gls(y~x,data=hom)
anova(all.lme.02)


# compare models using AIC & BIC
anova(update(all.lme.01,method="ML"),
      update(all.lme.02,method="ML")) # model 02 is better

# first check the full model:
plot(augPred(all.lme.01),main="all data full model 01")
coef(summary(all.lme.01)) # values of fixed effects
anova(summary(all.lme.01)) # anova fixed effects

# check residuals:
# very strange
qqnorm(residuals(all.lme.01,type="p"),main="all data (full model) N=60")
qqline(residuals(all.lme.01,type="p"))
boxplot(residuals(all.lme.01,type="p"),main="all data (full model) N=60")
plot(all.lme.01,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)


# now check "best-fitting" model 02:
plot(augPred(all.lme.02),main="all data (full model 02) N=60") # model 02 is best
coef(summary(all.lme.02)) # values of fixed effects
anova(summary(all.lme.02)) # values of fixed effects

# check residuals:
# these look normal to me:
qqnorm(residuals(all.lme.02, type = "p"),main="all data (best-fit model 02) N=60")
qqline(residuals(all.lme.02, type = "p"))
boxplot(residuals(all.lme.02, type = "p"),main="all data (best-fit model 02) N=60")
plot(all.lme.02,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)


# heteroscedastic model --------
# re-fit data using models that allow variance of residuals to change with fitted values

all.lme.hetero.02 <- update(all.lme.02,weights=varPower(form = ~fitted(.)) )
anova(all.lme.02,all.lme.hetero.02) # hetero model fits better
anova(all.lme.hetero.02)


plot(augPred(all.lme.hetero.02),main="all data (hetero full model 01)")
coef(summary(all.lme.hetero.02)) # values of fixed effects
anova(all.lme.hetero.02) # anova fixed effects

# check residuals:
# these look normal to me:
qqnorm(residuals(all.lme.hetero.02, type = "p"),main="all data (hetero full model 01) N=60")
qqline(residuals(all.lme.hetero.02, type = "p"))
boxplot(residuals(all.lme.hetero.02, type = "p"),main="all data (hetero full model 01) N=60")
plot(all.lme.hetero.02,resid(., type = "p") ~ fitted(.)|Participant, abline = 0)

# SUMMARY OF BEST-FITTING MODEL ----
best_exp1  <- all.lme.hetero.02
lineColors <- trellis.par.get("superpose.line")$col # default colors 
plot(augPred(best_exp1,level=0:1),grid=T,
     key=list(columns = 2,text=list(lab=c("fixed","participant")),
              lines=list(cex=1,lwd=2,lty=c(1,3),col=lineColors[1:2])))# plot includes fixed line and lines for individual participants
anova(best_exp1) # anova fixed effects
coef(summary(best_exp1))
# plot pse vs fitted values:
# following commands identify "outliers" (p<.05)
# not a problem here because there are only a few
plot(best_exp1,y~fitted(.),abline=c(0,1),id=.05,adj=-0.4)
qqnorm(best_exp1, ~resid(.,type="p"),id=.05,adj=-0.4)



# MODEL COMPARISONS

# Full random effects
exp1_full_random <- lme(
      fixed = y ~ x * envelope_1,
      random = ~ 1 + x | Participant,
      data = hom,
      control = lmeControl(opt = "optim")
)
# Random intercepts only
exp1_int_only <- lme(
      fixed = y ~ x * envelope_1,
      random = ~ 1 | Participant,
      data = hom
)
# No random effects
exp1_none <- gls(
      y ~ x * envelope_1,
      data = hom
)

anova(exp1_full_random, exp1_int_only, exp1_none)

# Now compare to model with allow variance of residuals to change with fitted values

# Full random effects
exp1_full_random_hetres <- lme(
      fixed = y ~ x * envelope_1,
      random = ~ 1 + x | Participant,
      data = hom,
      control = lmeControl(opt = "optim"),
      weights=varPower(form = ~fitted(.))
)
# Random intercepts only
exp1_int_only_hetres <- lme(
      fixed = y ~ x * envelope_1,
      random = ~ 1 | Participant,
      data = hom,
      weights=varPower(form = ~fitted(.))
)
# No random effects
exp1_none_hetres <- gls(
      y ~ x * envelope_1,
      data = hom,
      weights=varPower(form = ~fitted(.))
)

anova(exp1_full_random, exp1_int_only, exp1_none,
exp1_full_random_hetres, exp1_int_only_hetres, exp1_none_hetres)


exp1_env_only <- gls(
      y ~ x + envelope_1,
      data = hom,
      weights=varPower(form = ~fitted(.))
)

exp1_envint_only <- gls(
      y ~ x + x:envelope_1,
      data = hom,
      weights=varPower(form = ~fitted(.))
)

exp1_x_only <- gls(
      y ~ x,
      data = hom,
      weights=varPower(form = ~fitted(.))
)

anova(
      update(exp1_none_hetres, method = "ML"),
      update(exp1_env_only, method = "ML"),
      update(exp1_envint_only, method = "ML"),
      update(exp1_x_only, method = "ML")
      )
