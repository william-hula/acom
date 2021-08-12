library(tidyverse)

ItemParameters <- read_excel("ItemParameters.xlsx") %>%
  rename(target = Item)
items <- read_csv("www/items.csv")

item_difficulty <- ItemParameters %>%
  left_join(items, by = "target")

sum(is.na(item_difficulty$slide_num))

# 0


#write.csv(item_difficulty, "www/item_difficulty.csv")
