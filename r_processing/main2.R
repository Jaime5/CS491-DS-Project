library(dplyr)
library(tidyverse)
library(ggplot2)

csr_data = read.csv(file="datasets/311_Customer_Service_Requests.csv")

# glimpse(csr_data)

# unique(csr_data$SRStatus)

csr_data$create = as.Date(csr_data$CreatedDate)
csr_data$doned = as.Date(csr_data$DueDate)


