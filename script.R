# Load packages ----
## If you get an error, it means that the package is not installed. You can 
## use install.packages("packagename"), replacing "packagename" with "here", 
## for instance, if you want to install the "here" package
library(readxl)
library(here)
library(tidyverse)
library(scales)

# In this script we will look at population changes in Dutch cities with CBS data

# Read data ----

# Read data for 2022
kwb_2022 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2022.xls"))

# Examine data for 2022
kwb_2022

# Read data for the other two years as well
kwb_2017 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2017.xls"))
kwb_2011 <- read_excel(path = here("data", "raw_data", "kwb", "kwb-2011.xls"))

# Do all three tables have the same number of variables?
ncol(kwb_2022)
ncol(kwb_2017)
ncol(kwb_2011)

identical(colnames(kwb_2022), colnames(kwb_2017))
identical(colnames(kwb_2022), colnames(kwb_2011))

# Which column names do tables have in common?
intersect(colnames(kwb_2022), colnames(kwb_2017))
intersect(colnames(kwb_2022), colnames(kwb_2011))

# For 2022 we have lowercase column names
colnames(kwb_2022)

# For 2011 we have uppercase column names which do not match 2022 data
colnames(kwb_2011)

# Make column names in 2011 data lower case
colnames(kwb_2011) <- tolower(colnames(kwb_2011))

# Which column names do tables have in common now?
intersect(colnames(kwb_2022), colnames(kwb_2011))

# We still need to rename a couple of columns of interest
colnames(kwb_2011)

colnames(kwb_2011)[14] <- "a_inw"
colnames(kwb_2011)[3] <- "gwb_code"

# Now we have the columns we need to join the tables
intersect(colnames(kwb_2022), colnames(kwb_2011))

# Create a column with the year for each of the three tables before merging
# And keep only the five relevant columns gm_naam, recs, a_inw, gwb_code, and year
kwb_2011 <- kwb_2011 |> 
  mutate(year = 2011) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

kwb_2017 <- kwb_2017 |> 
  mutate(year = 2017) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

kwb_2022 <- kwb_2022 |> 
  mutate(year = 2022) |> 
  select(gm_naam, recs, a_inw, gwb_code, year)

# Bind the three tables into one
kwb_2011_22 <- rbind(kwb_2011, kwb_2017, kwb_2022)

# Show the population trend for Groningen
kwb_2011_22 |> 
  filter(recs == "Gemeente", gm_naam == "Groningen") |> 
  ggplot(aes(x = year, y = a_inw, group = recs)) +
  geom_line()

# Show the population trend for three cities
kwb_2011_22 |>
  mutate(a_inw = as.numeric(a_inw)) |> 
  filter(recs == "Gemeente", gm_naam %in% c("Groningen", "Haarlem", "Vlissingen")) |> 
  ggplot(aes(x = year, y = a_inw, color = gm_naam, group = gm_naam)) +
  geom_line() +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  xlab("Year") +
  ylab("Number of inhabitants") +
  labs(title = "Number of inhabitants in three Dutch cities") +
  scale_color_discrete(name = "Muncipality")

# Show the population trend for all cities in the Netherlands
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

# Write data to a csv file
write.csv(kwb_2011_22, here("data", "processed_data", "kwb-2011-22.csv"))


