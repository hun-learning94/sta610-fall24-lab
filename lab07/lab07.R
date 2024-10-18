library(datasets)
library(lme4)
head(ChickWeight)

# Q1. Test the effect of "Diet" on "weight"
# 1) a model neglecting group structure
# 2) a model accounting for group, but how?
# 2-1) a random intercept
# 2-2) a randon int + slope

mod0 = lm(weight ~ Diet + Time, 
          data = ChickWeight)
summary(mod0)
mod1 = lmer(weight ~ Diet + Time + (1 | Chick), 
            data = ChickWeight, REML = F)
summary(mod1)
mod2 = lmer(weight ~ Diet + Time + (Time | Chick), 
            data = ChickWeight, REML = F)
summary(mod2)

# Q2. For each of the model in Q1, 
# test the effect of "Diet" 
# 1) calculate lambda, df yourself
# 2) use anova function
# 3) use drop1 function

# mod0
mod0 = lm(weight ~ Diet + Time, data = ChickWeight)
mod0_sub = lm(weight ~ Time, data = ChickWeight)
logLik(mod0)
logLik(mod0_sub)
anova(mod0_sub, mod0)

mod1 = lmer(weight ~ Diet + Time + (1 | Chick), 
            data = ChickWeight, REML = F)
mod1_sub = lmer(weight ~ Time + (1 | Chick), 
            data = ChickWeight, REML = F)
ll1 = logLik(mod1)
ll1_sub = logLik(mod1_sub)
(lambda = c(2*(ll1 - ll1_sub)))
(d = attr(ll1, "df") - attr(ll1_sub, "df"))
(1 - pchisq(lambda, d))
anova(mod1_sub, mod1)
drop1(mod1, test="Chisq")

mod2 = lmer(weight ~ Diet + Time + (Time | Chick), 
            data = ChickWeight, REML = F)
mod2_sub = lmer(weight ~ Time + (Time | Chick), 
                data = ChickWeight, REML = F)
ll2 = logLik(mod2)
ll2_sub = logLik(mod2_sub)
(lambda = c(2*(ll2 - ll2_sub)))
(d = attr(ll2, "df") - attr(ll2_sub, "df"))
(1 - pchisq(lambda, d))
anova(mod2_sub, mod2)
drop1(mod2, test="Chisq")


# Q3. Test of random effect:
(ll0 = logLik(mod0))
(ll1 = logLik(mod1))
(ll2 = logLik(mod2))

# 1) random intercept
lambda = c(2*(ll1 - ll0))
0.5*0 + 0.5*(1 - pchisq(lambda, 1))

# 2) random slope
lambda = c(2*(ll2 - ll1))
0.5*(1 - pchisq(lambda, 1)) + 0.5*(1 - pchisq(lambda, 2))










