# processing ability out

test_dat <- readRDS(here("tests", "test_dat.RDS"))

exports <- test_dat[[3]]

df <- tibble(
  discrimination = strsplit(exports$discrimination, "_", 1)[[1]],
  target = strsplit(exports$words, "_")[[1]],
  item_number = strsplit(exports$item_number, "_")[[1]],
  order = strsplit(exports$order, "_")[[1]],
  itemDifficulty = strsplit(exports$itemDifficulty, "_")[[1]],
  key_press = strsplit(exports$key_press, "_")[[1]],
  response = strsplit(exports$responses, "_")[[1]],
  ability = strsplit(exports$abil, "_", 1)[[1]],
  sem = strsplit(exports$sem, "_")[[1]]
)



