
library(tidyverse)
library(brms)
mudil <- read_csv("output/andmed.csv")
mudil

sd(mudil$TW, na.rm = TRUE)

ggplot(data = mudil) +
  geom_point(mapping = aes(x = age, y = TL))

prior1 <- prior(normal(160, 35), nlpar = "alpha") +
  prior(normal(160, 35), nlpar = "beta") +
  prior(normal(1, 2.5), nlpar = "k", lb = 0) +
  prior(normal(1, 2.5), nlpar = "m", lb = 0)
fit1 <- brm(bf(TW ~ (alpha^(1 - m) - beta * exp(-k * age)) ^ (1/(1 - m)), 
               alpha + m + beta + k ~ 1, nl = TRUE),
            data = mudil, 
            prior = prior1,
            chains = 1,
            iter = 2000, 
            control = list(adapt_delta = 0.999, max_treedepth = 15))
summary(fit1)
plot(marginal_effects(fit1), points = TRUE)
