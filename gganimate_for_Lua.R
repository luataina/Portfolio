library(tidyverse)
library(gganimate)
library(ggthemes)
library(gifski)

  #For now, let's just make desktop working directory -- nothing to import from folder
setwd("/Users/luareis/Documents/RWE/gg animate")
mydata <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"), na.strings = c("", "NA"))

View(mydata)

  # Let's filter location (and add Date as.Date)
filtered <- mydata %>% 
  filter(location == "United States" | location == "Brazil") %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) %>% 
  select(Date, location, total_cases_per_million)

  # Make base plot 
c <- ggplot(filtered,
     aes(Date, total_cases_per_million, color = location)) +
  geom_point() + 
  geom_line(lwd = 0.5) +
  ggthemes::theme_few() + 
  labs(x = "", y = "Cumulative case counts per million", title = "Total COVID case counts") + 
  transition_reveal(Date) 

# Save final animation as a .gif (takes ~ 2min)
final_animation <- animate(c, 150, fps = 30,
  duration = 20, width = 650, height = 650,
  renderer = gifski_renderer())

anim_save("Horserace.gif", animation = final_animation)

