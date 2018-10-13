library(tidyverse)
library(readxl)

(mudil <- read_excel("data/andmed.xlsx"))
mudil <- mutate_at(mudil, "TW", parse_number)

mudil_long <- select(mudil, nr, sex = Sex, location = Location, matches("\\d")) %>% 
  gather(age, tl, matches("\\d")) %>% 
  drop_na()

write_csv(mudil, "output/andmed.csv")
write_csv(mudil_long, "output/andmed_otoliit.csv")

