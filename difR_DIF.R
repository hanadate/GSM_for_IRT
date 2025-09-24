library(tidyverse)
library(difR)

# Loading of the verbal data
data(verbal)
attach(verbal)

# Excluding the "Anger" variable
verbal <- verbal[colnames(verbal)!="Anger"]

# Keeping the first 5 items and the first 50 subjects
# (this is an artificial simplification to reduce the computational time)
verbal <- verbal[1:50, c(1:5, 25)]

# Three equivalent settings of the data matrix and the group membership
res.difLRT <- difLRT(verbal, group = "Gender", focal.name = 1, p.adjust.method="BH")
res.difLRT
