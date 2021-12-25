# loading packages
library(tidyverse)
library(lubridate)

# press tab key to autocomplete file name
# loading data
well_data <- read_csv("well_information.csv")
raw_growth_data <- read_csv("Tuesday Plate A.csv", skip = 12)

# pivot data from wide to long format
growth_data <- pivot_longer(raw_growth_data,
                            !c(Time, Temp),
                            names_to = "well",
                            values_to = "OD600")

# remove data from other students
well_data <- filter(well_data, student == "Kimia Rostin")

# combine tables
growth_data <- inner_join(well_data, growth_data)

# need to group by both Time and blank variables
growth_data <- group_by(growth_data, Time, blank)
# summarize groups by taking their mean
growth_data <- summarise(growth_data, mean_OD600 = mean(OD600))
growth_data <- ungroup(growth_data)

blank_only <- filter(growth_data, blank == TRUE)
min_blank <- min(blank_only$mean_OD600)

# calibrate OD600 measurements
growth_data <- mutate(growth_data, net_OD600 = mean_OD600 - min_blank)

# visualize growth curve
ggplot(growth_data,
       aes(x = Time, y = net_OD600, colour = blank)) +
  geom_point() + 
  labs(x = "Time (h:m:s)", y = "OD600") + ggtitle('E. coli B23 Growth Curve @ 37 C in Aerobic Conditions')
