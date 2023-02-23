library(readxl)
library(here)
library(tidyverse)
library(scales)

kwb_2022 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2022.xls"))
kwb_2022

kwb_2017 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2017.xls"))
kwb_2011 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2011.xls"))

ncol(kwb_2022)
ncol(kwb_2017)
ncol(kwb_2011)

identical(colnames(kwb_2022), colnames(kwb_2017))
identical(colnames(kwb_2022), colnames(kwb_2011))

intersect(colnames(kwb_2022), colnames(kwb_2017))
intersect(colnames(kwb_2022), colnames(kwb_2011))

colnames(kwb_2022)
colnames(kwb_2011)

colnames(kwb_2011) <- tolower(colnames(kwb_2011))

intersect(colnames(kwb_2022), colnames(kwb_2011))

colnames(kwb_2011)

colnames(kwb_2011)[14] <- "a_inw"
colnames(kwb_2011)[3] <- "gwb_code"

intersect(colnames(kwb_2022), colnames(kwb_2011))

kwb_2011 <- kwb_2011 |> 
  mutate(year = 2011) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

kwb_2017 <- kwb_2017 |> 
  mutate(year = 2017) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

kwb_2022 <- kwb_2022 |> 
  mutate(year = 2022) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

kwb_2011_22 <- rbind(kwb_2011, kwb_2017, kwb_2022)

kwb_2011_22 |> 
  filter(recs == "Gemeente", gm_naam == "Groningen") |> 
  ggplot(aes(x = year, y = a_inw, group = recs)) +
  geom_line()
  
kwb_2011_22 |>
  mutate(a_inw = as.numeric(a_inw)) |> 
  filter(recs == "Gemeente") |> 
  ggplot(aes(x = year, y = a_inw, color = gm_naam, group = gm_naam)) +
  geom_line() +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  xlab("Year") +
  ylab("Number of inhabitants") +
  labs(title = "Number of inhabitants in three Dutch cities") +
  scale_color_discrete(name = "Muncipality") +
  theme(legend.position = "none")

write.csv(kwb_2011_22, here("data", "processed_data", "kwb-2011-22.csv"))


