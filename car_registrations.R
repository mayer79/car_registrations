#=============================================================================================
# BAR CHART RACE ILLUSTRATED
# Using publicly available data on Swiss car registrations from
# https://www.auto.swiss/statistiken/pw-zulassungen-nach-marken/
#=============================================================================================

library(openxlsx)
library(tidyverse)
library(lubridate)
library(gganimate) 

#=============================================================================================
# PART 1: Fetch data
#=============================================================================================

# Step 1: Function to fetch one single excel sheet from data source
get_sheet <- function(sheet, year) {
  url <- "https://www.auto.swiss/fileadmin/3_Statistiken/Autoverkaeufe_nach_Modellen/ModellePW%i.xlsx"
  read.xlsx(sprintf(url, year), sheet = sheet, startRow = 16, cols = 5:7)
}

# Step 2: Apply it to each year and month for 2017, 2018 and 2019
raw <- list(`2017` = lapply(1:12, get_sheet, 2017),
            `2018` = lapply(1:12, get_sheet, 2018),
            `2019` = lapply(1:6, get_sheet, 2019))

# saveRDS(raw, "raw.rds")

#=============================================================================================
# PART 2: Prepare data
#=============================================================================================

# raw <- readRDS("raw.rds")

# Step 1: turn nested list into data frame; clean up data
prep1 <- raw %>%                                            
  lapply(bind_rows, .id = "month") %>%                      # bind sheets within year
  bind_rows(.id = "year") %>%                               # bind years
  mutate(year_month = make_date(year, month, 1),            # combine year and month and format it
         ym_formatted = format(year_month, "%b %Y"),
         ym_formatted = fct_reorder(ym_formatted, year_month)) %>%              
  rename(brand = Marke, model = Modell, N = Anzahl) %>%     # English names
  filter(!is.na(brand), brand != "Total") %>%               # kick out "total" rows
  mutate(brand = gsub("Š", "S", brand),                     # fix data inconsistencies 
         brand = gsub("Yong", "yong", brand),
         model = gsub("'", "", model),
         brand_model = paste(brand, model))                 # combine brand and model

# Step 2: Counts are with respect to each January but we want total cumulative counts
prep2 <- prep1 %>% 
  arrange(year_month) %>%                                   # sort in year_month
  group_by(year, brand_model) %>%                           # differentiate wrt to each Jan
  mutate(N = ifelse(month == "1", N, N - lag(N))) %>% 
  group_by(brand_model) %>% 
  mutate(N_cum = cumsum(N)) %>% 
  ungroup()

# Step 3: Pick 20 best sellers per month, starting from Jan 2017
prep3 <- prep2 %>%
  arrange(year_month, desc(N_cum)) %>% 
  group_by(year_month) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <= 20) %>% 
  ungroup() %>% 
  mutate_at(c("brand", "model", "brand_model"), factor)     # After filter, turn to factor

# Step 3: Check
# subset(prep3, model == "Octavia") %>% View

#=============================================================================================
# PART 3: Visualize
# Credits
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
#=============================================================================================

p <- ggplot(prep3, aes(ranking, group = brand_model, fill = brand, color = brand)) +
  geom_tile(aes(y = N_cum / 2, height = N_cum, width = 0.9), 
            alpha = 0.8, color = NA, show.legend = FALSE) +
  geom_text(aes(y = 0, label = paste(brand, " ")), 
            vjust = 0.2, hjust = 1, color = "black", size = 5) +
  geom_label(aes(y = N_cum, label = model), color = "black", fill = "white", 
             hjust = 1.1, label.size = 0.2, label.r = unit(0.5, "lines"),
             label.padding = unit(0.25, "lines"), size = 5) +
  geom_text(aes(y = N_cum, label = paste0(" ", prettyNum(N_cum, big.mark = "'")), hjust = 0), 
            color = "black", size = 5) +
  coord_flip(clip = "off", expand = FALSE) +  # text in pre-flip x-axis requires clip = "off"
  scale_y_continuous(breaks = seq(5000, 30000, by = 5000)) +
  scale_x_reverse() +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.9) +
  labs(title = '{closest_state}',             # placeholder
       subtitle = "Car registrations in Switzerland since January 2017",
       caption = "Source: https://www.auto.swiss/statistiken/pw-zulassungen-nach-marken/") +
  theme_void(base_size = 16) +
  theme(panel.grid.major.x = element_line(size = .1, color = "grey"),
        plot.margin = margin(2, 3, 2, 6, "cm")) +
  
  # The next lines tune the animation
  transition_states(ym_formatted, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out') +
  view_follow(fixed_x = TRUE)


# Render the animation and save it
gif <- TRUE 

if (gif) {
  # gif animation needs "gifski" package (no admin rights required)
  require(gifski)
  animate(p, duration = 20, fps = 20, width = 800, height = 800)
  anim_save(filename = "car_registrations.gif")
} else {
  # MP4 needs av package (admin rights to install "FFmpeg" required)
  require(av)
  animate(p, duration = 20, fps = 20, width = 1200, height = 1200, res = 120,
          renderer = av_renderer("car_registrations.mp4"))
}