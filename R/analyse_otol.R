#' ---
#' title: Mudil growth curves
#' author: rstats Tartu
#' date: 2018-10-30; (updated `r Sys.Date()`)
#' output: github_document
#' ---

#' ## Setup
#' Load libraries
#+
library(tidyverse)
library(viridis)
library(brms)
library(here)
library(skimr)

#' Set up stan-mc parameters
#+
chains <- 4
control_pars <- list(adapt_delta = 0.999)

#' Import data
#' Import and munge dataset.
#+
mudil <- read_csv(here("output", "andmed_otoliit.csv"))
skim(mudil)

#' Fish id: location + nr
#+
mudil %>% 
  group_by(nr, age, location) %>% 
  summarise(N = n())

#' Update sex categories.
#+
mudil_mod <- mudil %>% 
  mutate(location = str_replace_all(location, "\\s", "_"),
         id = str_c(location, nr, sep = "_"),
         sex = case_when(
           sex == 0 ~ "F",
           sex == 1 ~ "M",
           sex == 3 ~ "juv"
         )) %>% 
  select(id, everything())

#' Only adult fish
#+
mudil_ad <- filter(mudil_mod, sex != "juv")

#' Mean and sd of fish at different age
#+
mudil_ad %>% 
  group_by(age) %>% 
  summarise_at("tl", funs(mean, sd))

#' Individual growth curves
#+
ggplot(data = mudil_ad) +
  geom_line(mapping = aes(x = age, y = tl, group = id, color = sex), alpha = 2/3) +
  facet_wrap(~location) +
  scale_color_viridis_d() +
  labs(x = "Age (year)", y = "Total length (mm)")

#' Weird fish in Saarnaki:
#+
fish_id <- mudil_ad %>% 
  filter(location == "Saarnaki") %>% 
  mutate(ad = tl - tl[age == 1]) %>% 
  filter(ad < 0) %>% 
  pull(id)

#' Drop this weird fish
#+
mudil_ad <- filter(mudil_ad, id != fish_id)
ggplot(data = mudil_ad) +
  geom_line(mapping = aes(x = age, y = tl, group = id, color = sex), alpha = 2/3) +
  facet_wrap(~location) +
  scale_color_viridis_d() +
  labs(x = "Age (year)", y = "Total length (mm)")

#' Average length at age in adults
#+
ggplot(data = mudil_ad, mapping = aes(x = age, y = tl)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", alpha = 0.3) +
  geom_point(position = position_jitter(width = 1/3)) +
  stat_summary(fun.y = mean, geom = "line", color = viridis(6)[6]) +
  facet_wrap(~location) +
  labs(x = "Age (year)", y = "Total length (mm)")

#' Growth curves per introduction year
#+
ggplot(data = mudil_ad, mapping = aes(x = age, y = tl)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", alpha = 0.3) +
  geom_point(position = position_jitter(width = 1/3)) +
  stat_summary(fun.y = mean, geom = "line", color = viridis(6)[6]) +
  facet_wrap(~introduction) +
  labs(title = "Growth curves per introduction year", 
       x = "Age (year)", 
       y = "Total length (mm)")

#' ## Modeling
#' Get reasonable starting values for van bertalaffny model coefficients.
#+
library(FSA)
svTypical <- vbStarts(tl ~ age, data = mudil_ad)
unlist(svTypical)

#' Set up prior with suggested starting values using normal distribution

#' First model, individual variance and different sd per age
#' Total length modeled by van bertalanffy 
#+
vbgf_f <- tl ~ Linf * (1 - exp(-K * (age - t0)))

#' van bertalanffy model coefficients can vary by location and we take individual differences into account as random effect.s
#+
vbgf_coefs_f <- Linf + K + t0 ~ location + (1 | id)

#' This is how brms default priors look like:
#+
get_prior(bf(tl ~ Linf * (1 - exp(-K * (age - t0))), 
             vbgf_coefs_f, 
             sigma ~ age, nl = TRUE),
          data = mudil_ad)

#' We can set our own priors. Wiki says that adult gobis can be between 150 and 200 mm long, let's take 200 as a prior fot tl
#+
kihnu <- prior(normal(170, 35), nlpar = "Linf", lb = 0) +
  prior(normal(0.7, 0.2), nlpar = "K", lb = 0) +
  prior(normal(0.5, 0.2), nlpar = "t0")

#' Fit location model: 
#+ eval=FALSE
fit2 <- brm(bf(vbgf_f, 
               vbgf_coefs_f, 
               sigma ~ age, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "identity"),
            prior = kihnu,
            chains = chains,
            iter = 2000)
fit2 <- add_ic(fit2, ic = "waic")
write_rds(fit2, here("output", "von_bertalanffy_normal_otol_2.rds"))

#+ echo=FALSE
fit2 <- read_rds(here("output", "von_bertalanffy_normal_otol_2.rds"))

#+ 
summary(fit2)

#' Plot fits for different locations
cond <- make_conditions(data.frame(location = unique(mudil_ad$location)), vars = "location")
p <- plot(marginal_effects(fit2, conditions = cond), points = TRUE, ask = FALSE, plot = FALSE)
p[[1]] + labs(x = "Age (year)", y = "Total length (mm)")

#' Model with same t0 for all locations to test if we can use common t0 for all locations.
vbgf_coefs_f <- Linf + K ~ location + (1 | id)

#+ eval=FALSE
fit21 <- brm(bf(vbgf_f, 
               vbgf_coefs_f, 
               t0 ~ 1,
               sigma ~ age, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "identity"),
            prior = kihnu,
            chains = chains,
            iter = 2000)
fit21 <- add_ic(fit21, ic = "waic")
write_rds(fit21, here("output", "von_bertalanffy_normal_otol_21.rds"))

#+ echo=FALSE
fit21 <- read_rds(here("output", "von_bertalanffy_normal_otol_21.rds"))

#+ 
summary(fit21)

#' Plot fits for different locations
cond <- make_conditions(data.frame(location = unique(mudil_ad$location)), vars = "location")
p <- plot(marginal_effects(fit21, conditions = cond), points = TRUE, ask = FALSE, plot = FALSE)
p[[1]] + labs(x = "Age (year)", y = "Total length (mm)")

#' Compare models with location-specific t0 and common t0. Let's use waic as loo complains about pareto. 
waic(fit2, fit21)
#' Model with location-specific t0 (more parameters) seems to be better, so let's go on with it.

#' Introduction year
vbgf_coefs_f <- Linf + K + t0 ~ introduction + (1 | id)

#+ eval=FALSE
fit3 <- brm(bf(vbgf_f, 
               vbgf_coefs_f, 
               sigma ~ age, nl = TRUE),
            data = mutate_at(mudil_ad, "introduction", as.factor),
            family = gaussian(link = "identity"),
            prior = kihnu,
            chains = chains,
            iter = 4000)
write_rds(fit3, here("output", "von_bertalanffy_normal_otol_3.rds"))

#+ echo=FALSE
fit3 <- read_rds(here("output", "von_bertalanffy_normal_otol_3.rds"))

#+ 
summary(fit3)

#' Plot out fits for different locations
cond <- make_conditions(data.frame(introduction = as.factor(unique(mudil_ad$introduction))), vars = "introduction")
p <- plot(marginal_effects(fit3, conditions = cond), points = TRUE, ask = FALSE, plot = FALSE)
p[[1]] + labs(x = "Age (year)", y = "Total length (mm)")

#' Effect of location and sex
#' Simple effects
vbgf_coefs_f <- Linf + K + t0 ~ location + sex + (1 | id)

#+ eval=FALSE
fit4 <- brm(bf(vbgf_f, 
               vbgf_coefs_f, 
               sigma ~ age, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "identity"),
            prior = kihnu,
            chains = chains,
            iter = 4000)
write_rds(fit4, here("output", "von_bertalanffy_normal_otol_4.rds"))

#+ echo=FALSE
fit4 <- read_rds(here("output", "von_bertalanffy_normal_otol_4.rds"))

#+ 
summary(fit4)

#' Plot out fits for different locations
cond <- make_conditions(expand.grid(sex = unique(mudil_ad$sex), location = unique(mudil_ad$location)), vars = c("location", "sex"))
p <- plot(marginal_effects(fit4, conditions = cond), points = TRUE, ask = FALSE, plot = FALSE, ncol = 2)
p[[1]] + labs(x = "Age (year)", y = "Total length (mm)")

#' With interaction
vbgf_coefs_f <- Linf + K + t0 ~ location*sex + (1 | id)

#+ eval=FALSE
fit5 <- brm(bf(vbgf_f, 
               vbgf_coefs_f, 
               sigma ~ age, nl = TRUE),
            data = mudil_ad,
            family = gaussian(link = "identity"),
            prior = kihnu,
            chains = chains,
            iter = 4000)
write_rds(fit5, here("output", "von_bertalanffy_normal_otol_5.rds"))

#+ echo=FALSE
fit5 <- read_rds(here("output", "von_bertalanffy_normal_otol_5.rds"))

#+ 
summary(fit5)

#' Plot out fits for different locations
#+ fig.height=9
cond <- make_conditions(expand.grid(sex = unique(mudil_ad$sex), location = unique(mudil_ad$location)), vars = c("location", "sex"))
p <- plot(marginal_effects(fit5, conditions = cond), points = TRUE, ask = FALSE, plot = FALSE, ncol = 2)
p[[1]] + labs(x = "Age (year)", y = "Total length (mm)")

