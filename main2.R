library(dplyr)
library(tidyverse)

victim_based_crimes = read.csv(file="datasets/BPD_Part_1_Victim_Based_Crime_Data.csv")

glimpse(victim_based_crimes)

# Randomly choose from this set, it's huge so let's chunk it down to 10k
SAMPLE_SIZE = 10000

victim_based_crimes.sample = victim_based_crimes[sample(nrow(victim_based_crimes), SAMPLE_SIZE),]

glimpse(victim_based_crimes.sample)



# dif_districts = unique(police_calls.sample$district)
# dif_districts
# length(dif_districts)
#
# dif_priority = unique(police_calls.sample$priority)
# dif_priority
