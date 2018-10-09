library(tidyverse)
library(readxl)

(mudil <- read_excel("data/andmed.xlsx"))
mudil <- mutate_at(mudil, "TW", parse_number)
write_csv(mudil, "output/andmed.csv")
