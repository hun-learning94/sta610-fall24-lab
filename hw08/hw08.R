library(nlme)
library(tidyverse)
dat = readRDS("hw08/gdp.Rds")
dat = as.data.frame(dat)
dat$country = rownames(dat)
dat = dat %>% pivot_longer(!country, names_to="year", values_to="gdp")
dat$country = as.factor(dat$country)
dat$year = as.numeric(dat$year)
dat$gdp = log1p(dat$gdp) # not required to solve this problem, but usually economic time series are modeled in log term

## a.
fit0 = lm(gdp ~ year, data = dat) # no random effects
fit1 = lme(gdp ~ year, random = ~1|country, data = dat, method = "ML") # random intercept only
fit2 = lme(gdp ~ year, random = ~year|country, data = dat, method = "ML") # random intercept and random slope

# testing random intercept
lambda = 2*c(logLik(fit1) - logLik(fit0))
0.5 * (1 - pchisq(lambda,1))
# testing random slope
lambda = 2*c(logLik(fit2) - logLik(fit1))
0.5 * (1 - pchisq(lambda,2)) + 0.5 * (1 - pchisq(lambda,1))

## b.
boxplot(resid(fit1) ~ country, data = dat) # country effect explained, at least for the mean levels
coplot(resid(fit1) ~ year | country, data = dat, type = "p", show.given = F) # temporal pattern for some countries. add random time slope?

boxplot(resid(fit2) ~ country, data = dat) # country effect explained, at least for the mean levels
coplot(resid(fit2) ~ year | country, data = dat, type = "p", show.given = F) # temporal pattern still exists. Modelling time trend as random is not enough. what more to do?

## c.
dat$yearc = as.factor(dat$year)
fit3 = lmer(gdp ~ year + (year|country) + (1|yearc), data = dat, REML=F) # adding year as random intercept. somehow lme is not built for crossed random effects https://biostatmatt.com/archives/2718
summary(fit3)
BIC(fit2)
BIC(fit3) # improved fit

## d.
fit4 = lme(gdp ~ year, random = ~1|country, correlation = corAR1(form=~year|country), data = dat, method = "ML") # introducing autocorrelation, by specifying form=~year|country you are telling the model how far year is apart (time lag)
summary(fit4)
BIC(fit4) # improved fit

## e.
coplot(resid(fit4) ~ year | country, data = dat, type = "p", show.given = F) # temporal pattern mitigated
boxplot(resid(fit4) ~ country, data = dat) # by introducing phi, the random intercept sigma significantly reduced, leading to residuals varying per country.




