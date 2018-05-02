# General Deps
library(dplyr)
library(tidyverse)
library(ggplot2)

# Apriori Deps
library(arules)

# KNN Deps
library(kknn)

# Clustering Deps
library("cluster")
library(factoextra)
library(cluster)
library(NbClust)

set.seed(12745)
SAMPLE_SIZE = 10000

pc_data = read.csv(file="datasets/911_Police_Calls_for_Service.csv", nrow=SAMPLE_SIZE)

# Helper function used for generating mode for categorical and numerical data
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ------------------- START KNN SECTION -------------------------

KNN_PoliceCalls = train.kknn(priority ~ district, data=pc_data, kmax=8)

plot(KNN_PoliceCalls)

summary(KNN_PoliceCalls) # best K is 5

KNNPcData = kknn(priority ~ district, train=pc_data, test=pc_data, k=5)

# Extremely over-fitted
summary(KNNPcData)

# ------------------- END KNN SECTION -----------------------------


# ------------------- START ASSOCIATION SECTION -------------------

# Set minimum confidence level
CONFIDENCE = .6

pc_data_categorical = pc_data

pc_data_categorical[,c(1:3)] = lapply(pc_data_categorical[,c(1:3)],
                                   function(Col) {factor(Col,
                                                         levels = unique(Col),
                                                         labels = unique(Col)) } )


Rules = apriori(pc_data_categorical, parameter=list(confidence=CONFIDENCE))

# Store results in a dataframe
Results = as(Rules,"data.frame")

# Let's see the rules that were found in our data
# All of these have at least 0.85 confidence
Results %>% select(rules, confidence) %>% arrange(desc(confidence))

# ------------------- END ASSOCIATION SECTION ----------------------


# ------------------- START CLUSTER SECTION ----------------------

pc_data.process = within(pc_data, rm(callDateTime, location, description, callNumber, recordId, incidentLocation))

pc_data.cprocess = pc_data.process

pc_data.cprocess$priority = factor(c(pc_data.process$priority))
pc_data.cprocess$district = factor(c(pc_data.process$district))

km.res = kmeans(pc_data.cprocess, 10, nstart=25)
# k-means group number of each observation
km.res$cluster

# Visualize k-means clusters
fviz_cluster(km.res, data=pc_data.cprocess, geom="point", stand=FALSE,
             ellipse.type="norm")

pam.res = pam(pc_data.cprocess, 3)
pam.res$cluster

# Visualize pam clusters
fviz_cluster(pam.res, stand=FALSE, geom="point", frame.type="norm")

# ------------------- END CLUSTER SECTION ----------------------
