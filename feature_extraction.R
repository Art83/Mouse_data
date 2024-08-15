library(dplyr)


mouse_data_completed <- readRDS("objects/mouse_data_completed")


# Deviation from the ideal line
deviation_df <- mouse_data_completed %>%
  group_by(PIN, stage) %>% 
  mutate(
    order = cut(
      1:n(), 
      breaks = c(-Inf, which(event_type == "Click"), Inf),
      labels = 0:(length(which(event_type == "Click"))),
      include.lowest = TRUE  # Include the lowest value in the first interval
    )
  ) %>% 
  group_by(PIN, stage, order) %>% 
  mutate(
    start_x = first(x),
    end_x = last(x),
    start_y = first(y),
    end_y = last(y)
  ) %>% 
  rowwise() %>% 
  mutate(dist = perp_dist(c(x, y), c(start_x, start_y), c(end_x, end_y)))


deviation_df <- deviation_df %>% 
  group_by(PIN) %>% 
  summarise(avg_dist = median(dist, na.rm = T),
            iqr_dist = IQR(dist, na.rm = T),
            quant_25_dist = quantile(dist, probs = 0.25, na.rm = T),
            quant_75_dist = quantile(dist, probs = 0.75, na.rm = T),
            quant_95_dist = quantile(dist, probs = 0.95, na.rm = T),
            max_dist = max(dist, na.rm = T))

saveRDS(deviation_df, "objects/deviation_features")

# Acute angles
acute_angles_df <- mouse_data_completed %>%
  select(PIN, event_type, stage, x,y) %>% 
  group_by(PIN, stage) %>%
  mutate(
    lg_x1 = lag(x),
    lg_y1 = lag(y),
    lg_x2 = lag(x, n = 2),
    lg_y2 = lag(y, n = 2)
  ) %>% 
  filter(!is.na(lg_y2) | event_type == "Click") %>% 
  mutate(x.1 = x - lg_x1,
         y.1 = y - lg_y1,
         x.2 = lg_x2 - lg_x1,
         y.2 = lg_y2 - lg_y1,
         d.prod = x.1*x.2 + y.1*y.2,
         f.norm = sqrt(x.1^2 + y.1^2),
         s.norm = sqrt(x.2^2 + y.2^2),
         theta_rad = acos(d.prod / (f.norm*s.norm)),
         theta_degr = round(theta_rad*(180/pi),1)
  ) %>%
  mutate(acute_angle = if_else(theta_degr < 90 & theta_degr > 0, 1, 0)) %>%
  filter(acute_angle == 1 | event_type == "Click") %>%
  mutate(acute_angle = tidyr::replace_na(acute_angle, 0))



