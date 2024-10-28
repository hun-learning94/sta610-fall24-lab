library(lme4)
dat = dget("https://www2.stat.duke.edu/~pdh10/Teaching/610/Homework/Earthquake")
head(dat)

# response: accel
# group: Quake
# micro level: distance, soil -> fixed and random
table(dat$soil, dat$Quake)
boxplot(distance ~ Quake, dat=dat)
# macro level: Richter -> fixed only
plot(as.numeric(dat$Quake)+rnorm(nrow(dat),sd=0.1), dat$Richter)

## a
mod = lmer(accel ~ distance + soil +Richter +  (1+ soil + distance|Quake),
           dat = dat, REML=F)
summary(mod)
plot(mod)
qqnorm(residuals(mod)); qqline(residuals(mod))

## b
coplot(accel ~ distance | as.factor(Quake), data = dat, show.given=F)
coplot(accel ~ log(distance) | as.factor(Quake), data = dat, show.given=F)
coplot(log(accel) ~ distance | as.factor(Quake), data = dat, show.given=F)
coplot(log(accel) ~ log(distance) | as.factor(Quake), data = dat, show.given=F)

## c
dat$taccel = log(dat$accel)
dat$tdist = log(dat$distance)
mod1 = lmer(taccel ~ tdist + soil +Richter +  (1+ soil + tdist|Quake),
           dat = dat, REML=F)
summary(mod1)
plot(mod1)
qqnorm(residuals(mod1)); qqline(residuals(mod1))

## d
mod_full = lmer(taccel ~ tdist + soil +Richter +  (1+ soil + tdist|Quake), dat = dat, REML=F)
mod_nosoil = lmer(taccel ~ tdist + soil +Richter +  (1+ tdist|Quake), dat = dat, REML=F)
mod_notdist = lmer(taccel ~ tdist + soil +Richter +  (1+ soil|Quake), dat = dat, REML=F)

lambda = c(2*(logLik(mod_full) - logLik(mod_nosoil)))
0.5*(1 - pchisq(lambda, 3)) + 0.5*(1 - pchisq(lambda, 2))

lambda = c(2*(logLik(mod_full) - logLik(mod_notdist)))
0.5*(1 - pchisq(lambda, 3)) + 0.5*(1 - pchisq(lambda, 2))

BIC(mod_full)
BIC(mod_nosoil)
BIC(mod_notdist)

mod_notdist = lmer(taccel ~ soil +Richter +  (1+ soil + tdist|Quake), 
                   dat = dat, REML=F)
anova(mod_full, mod_notdist)
mod_nosoil = lmer(taccel ~ tdist +Richter +  (1+ soil + tdist|Quake), 
                   dat = dat, REML=F)
anova(mod_full, mod_nosoil)
mod_noRich = lmer(taccel ~ tdist + soil + (1+ soil + tdist|Quake), 
                  dat = dat, REML=F)
anova(mod_full, mod_noRich)

