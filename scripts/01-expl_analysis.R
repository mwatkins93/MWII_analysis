## 0. ORGANISATION ---------
## Name: Matt Watkins
## Date: 2024-08-09
## Project: MWII_analysis
## Objective: Explore the MWII throwing knife dataset
## Inputs: 2022 MWII csv sheet
## Outputs: various plots
## -------------------------

## 1. NOTES ----

### 1.01 Noteworthy analysis points ----

### 53% of kills came from 5 maps (Hydroelectric, Farm, Taraq, Raceway, and Dome)

### 44 kills came from the defensive side and 42 from the offensive side

## 2. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(RColorBrewer)

brewer.pal(n = 3, name = "Set1")

## 3. IMPORT ----

### 3.01 Read in MW 2022 data ----
tk_data <- read_xlsx(path = "./data/mw2022_tks.xlsx")

## 4. TIDY // PROCESS ----

tk_data$date <- as.Date(tk_data$date)

### 4.01 Make df for specific dates on annual count plot ----

vect1 <- c("2022-11-01", "2023-01-01")

dates <- data.frame(date = vect1)

dates$date <- as.Date(dates$date)

## 5. PLOTTING ----

### 5.01 Count the number of map occurrences and plot as a histogram ----

tk_data %>% 
  ggplot(aes(x = fct_rev(fct_infreq(map)), fill = side)) +
  
  # Bar plot
  geom_bar() +
  
  # Axes
  labs(x = "", y = "Kills", fill = "Spawn") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16), limits = c(0, 16)) +
  coord_flip(expand = FALSE) +
  
  # Theme
  # Theme
  theme_light() +
  scale_fill_manual(values = c("#377eb8", "#4DAF4A"))

### 5.02 Display a date histogram (view frequency over the year) ----
tk_data %>% 
  ggplot(aes(x = date)) +
  
  # Bar plot
  geom_bar() +

  # Axes
  scale_x_date(breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = c(0, 2, 4, 6, 8, 10), limits = c(0, 10)) +
  labs(x = "", y = "Kills") +
  
  # Theme
  theme_light() +

  # Annotations
  geom_vline(data = dates, aes(xintercept = date), colour = "lightblue", linetype = "dashed") +
  geom_text(data = dates, mapping = aes(x = date - 5, y = 9.5, label = format(date, format = "%Y")),
            inherit.aes = FALSE,
            size = 4,
            hjust = 1,
            angle = 90)
  




  
  
  
  
## 6. SAVING // EXPORTING ----

## 7. TRIAL // JUNK CODE ----



