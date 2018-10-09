
library(tidyverse)
library(brms)
mudil <- read_csv("output/andmed.csv")
mudil

max(mudil$TL, na.rm = TRUE)
sd(mudil$TL, na.rm = TRUE)

ggplot(data = mudil) +
  geom_point(mapping = aes(x = age, y = TL))

prior2 <- prior(normal(214, 42), nlpar = "Linf") +
  prior(student_t(3, 0, 2), nlpar = "K", lb = 0) +
  prior(student_t(3, 0, 2), nlpar = "t0", ub = 0)

if (FALSE) {
  fit2 <- brm(bf(TL ~ Linf * (1 - exp(-K * (age - t0))), 
                 Linf + K + t0 ~ 1, nl = TRUE),
              data = mudil, 
              prior = prior2,
              chains = 4,
              iter = 2400, 
              control = list(adapt_delta = 0.999, max_treedepth = 13))
  write_rds(fit2, "output/von_bertalanffy_student_2.rds")
}

fit2 <- read_rds("output/von_bertalanffy_student_2.rds")
summary(fit2)
plot(marginal_effects(fit2), points = TRUE)
