
library(tidyverse)
library(viridis)
library(brms)
mudil <- read_csv("output/andmed_otoliit.csv")
mudil

#' Fish id: location + nr
mudil %>% 
  group_by(nr, age, location) %>% 
  summarise(N = n()) %>% 
  filter(N > 1)

mudil_mod <- mudil %>% 
  mutate(location = str_replace_all(location, "\\s", "_"),
         id = str_c(location, nr, sep = "_"),
         sex = case_when(
           sex == 0 ~ "F",
           sex == 1 ~ "M",
           sex == 3 ~ "juv"
         )) %>% 
  select(id, everything())

ggplot(data = mudil_mod) +
  geom_histogram(mapping = aes(x = tl), bins = 30) +
  facet_wrap(~sex)

#' Only adult fish
mudil_ad <- filter(mudil_mod, sex != "juv")

mudil_ad %>% 
  group_by(age) %>% 
  summarise_at("tl", funs(mean, sd))

#' Individual growth curves
ggplot(data = mudil_ad) +
  geom_line(mapping = aes(x = age, y = tl, group = id, color = sex)) +
  facet_wrap(~location) +
  scale_color_viridis_d()

#' Average length at age in adults
ggplot(data = mudil_ad, mapping = aes(x = age, y = tl)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", alpha = 0.3) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", color = viridis(6)[6]) +
  facet_wrap(~location) +
  labs(x = "Age (year)", y = "Total length (mm)")

#' Starting values for van bertalaffny model coefficients
library(FSA)
svTypical <- vbStarts(tl ~ age, data = mudil_ad)
unlist(svTypical)

# Set up prior with suggested starting values using normal distribution
prior0 <- prior(normal(168, 30), nlpar = "Linf") +
  prior(normal(0.68, 0.2), nlpar = "K") +
  prior(normal(0.5, 0.2), nlpar = "t0")

get_prior(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
             Linf + K + t0 ~ 1, nl = TRUE),
          data = mudil_ad)

fit0 <- brm(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
               Linf + K + t0 ~ 1, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "log"),
            prior = prior0,
            chains = 4,
            iter = 4000)
write_rds(fit0, "output/von_bertalanffy_normal_otol.rds")
fit0 <- read_rds("output/von_bertalanffy_normal_otol.rds")
summary(fit0)
plot(marginal_effects(fit0, method = "fitted"), points = TRUE)


#' Model with individual variance

get_prior(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
             Linf + K + t0 ~ 1 + (1 | id), nl = TRUE),
          data = mudil_ad)

fit1 <- brm(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
               Linf + K + t0 ~ 1 + (1 | id), nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "log"),
            prior = prior0,
            chains = 1,
            iter = 4000)
fit_file <- "output/von_bertalanffy_normal_otol_1.rds"
write_rds(fit1, fit_file)
fit1 <- read_rds(fit_file)
summary(fit1)
plot(marginal_effects(fit1), points = TRUE)


get_prior(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
             Linf + K + t0 ~ 0 + location + (1 | gr(id, by = location)), 
             sigma ~ age, nl = TRUE),
          data = mudil_ad)


fit2 <- brm(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
               Linf + K + t0 ~ 0 + location + (1 | gr(id, by = location)), 
               sigma ~ age, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "identity"),
            prior = prior0,
            chains = 1,
            iter = 4000)
fit_file <- "output/von_bertalanffy_normal_otol_2.rds"
write_rds(fit2, fit_file)
fit2 <- read_rds(fit_file)
summary(fit2)
plot(marginal_effects(fit2), points = TRUE)

cond <- make_conditions(data.frame(location = unique(mudil_ad$location)), vars = "location")
plot(marginal_effects(fit2, conditions = cond), points = TRUE)

