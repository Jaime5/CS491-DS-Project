library(dplyr)
library(tidyverse)
library(ggplot2)

SAMPLE_SIZE = 100000

pc_data = read.csv(file="datasets/911_Police_Calls_for_Service.csv", nrow=SAMPLE_SIZE)
non_emerg_data = filter(pc_data, priority=="Non-Emergency")

glimpse(non_emerg_data)

