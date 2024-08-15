library(dplyr)

source("./funcs.R")

mouse_data_csv <- read.csv("./raw/mouse_data_e1.csv",stringsAsFactors = F)
complete_csv <- read.csv("./raw/complete.csv", stringsAsFactors = F)


completers_id <- complete_csv %>% 
  group_by(PIN, stage) %>% 
  count() %>% 
  filter(stage == "close_HIT_q") %>% 
  pull(PIN)

saveRDS(completers_id, "objects/completers_id")


mouse_data_completed <- mouse_data_csv %>%
  filter(PIN %in% completers_id) %>%
  group_by(PIN, stage) %>%
  mutate(click_count = cumsum(event_type == "Click")) %>% 
  select(PIN, timestamp, stage, event_type, x,y,coordinates_converted_details,scroll_position_x,scroll_position_y,click_count)


saveRDS(mouse_data_completed, "objects/mouse_data_completed")



