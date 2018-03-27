library(dplyr)
library(tidyverse)

police_calls_data = read.csv(file="datasets/911_Police_Calls_for_Service.csv")
police_calls_non_emerg = filter(police_calls_data, priority=="Non-Emergency")

length(unique(police_calls_non_emerg$callNumber))

# Randomly choose from this set, it's huge so let's chunk it down to 10k
SAMPLE_SIZE = 10000
police_calls.sample = police_calls_data[sample(nrow(police_calls_non_emerg), SAMPLE_SIZE),]


glimpse(police_calls.sample)

police_calls.non_emerg = filter(police_calls.sample, priority == "Non-Emergency")

glimpse(police_calls_sample_non_emerg)

dif_districts = unique(police_calls.sample$district)
dif_districts
length(dif_districts)

dif_priority = unique(police_calls.sample$priority)
dif_priority

unique(police_calls.sample$incidentLocation)

# anyDuplicated(police_calls.sample$callNumber)


