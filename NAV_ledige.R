# Load Libraries

library(tidyverse)
library(readxl)
library(httr)
library(lubridate)
library(gganimate)
library(scales)

# Get data and it in working directory. 

GET(url = "https://www.nav.no/no/nav-og-samfunn/statistikk/arbeidssokere-og-stillinger-statistikk/hovedtall-om-arbeidsmarkedet/_/attachment/download/514024b4-6eae-4676-99d0-7ed1794f2522:8d3d4333e3f2d32ebbb518700507e903e251715a/HL100_Antall_helt_ledige_historisk._%C3%85r_m%C3%A5ned.xlsx",
  httr::write_disk("Nav.xlsx"))

# The Excel is quite messy, as such load only the correct Sheet and range within the sheet. 

NAV <- read_xlsx("Nav.xlsx", sheet = "1. Antall Landet", range = "B6:N36")

# I find it easier to work with numbers, thus a small helper tibble:

mo_trans <- tibble(Month = names(NAV[-1]), 
                                    month_n = 1:12)

# Data wrangling. Change to tidy format, make date objects and keep only what is needed.

NAV <- NAV %>% 
  rename("Year" = 1) %>%
  pivot_longer(-Year, values_to = "Unemp", names_to = "Month") %>%
  filter(!is.na(Unemp)) %>%
  left_join(mo_trans, by = "Month") %>%
  mutate(Date = ceiling_date(ymd(paste0(Year,"-", month_n, "-", "1")), "month") - days(1)) %>%
  arrange(Date) %>%
  mutate(indx = row_number()) %>%
  select(Date, indx, Unemp)
  
# ADD DRAMA

NAV$indx <- ifelse(NAV$indx == max(NAV$indx), 
                   NAV$indx + 100, 
                   NAV$indx)

# Animation

p1 <- NAV %>% 
  ggplot(aes(x = Date, y = Unemp)) + 
  geom_line() +
  view_follow() +
  geom_point(color = "red") + 
  transition_reveal(indx) + 
  theme_minimal() +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Antall registerte arbeidsledige hos NAV", 
       x = NULL, 
       y = NULL, 
       caption = "Kilder: NAV.no, @FurusethThomas ")

animate(p1, end_pause = 25, nframes = 300, fps = 12)  

anim_save(filename = "arb.gif")
