library(rpact)
library(difR)
library(tidyverse)
library(mirt)

# data creation
# Simulate dichotomous item responses for n respondents to all items
# - 'a' denotes the item discriminations
# - 'b' denotes the item difficulties
# - 'n' denotes the sample size
generate_dich_data <- function(a, b, n) {
  resp <-
    mirt::simdata(a = a, d = -a*b, N = n, itemtype = "dich") %>% 
    as.data.frame()
  
  return(resp)
}
# Item 1: nonDIF
# Item 2: DIF
set.seed(1)
b_1 <- generate_dich_data(a=c(1,1,1,1,1), b=c(0,0,0,0,0), n=100) %>% 
  dplyr::mutate(group="A")
set.seed(1)
b_2 <- generate_dich_data(a=c(1,1,1,1,1), b=c(-1,-.5,0,.5,1), n=100) %>% 
  dplyr::mutate(group="B")
data <- bind_rows(b_1, b_2)

# difLRT for fixed sample
res.difLRT <- difLRT(data, group = "group", focal.name = "A", p.adjust.method="BH")
res.difLRT
res.difLRT$p.value
res.difLRT$LRT

# randam grouping for each stage
set.seed(1)
stage_b_1 <- sample(rep(1:3, length=nrow(b_1)))
set.seed(1)
stage_b_2 <- sample(rep(1:3, length=nrow(b_2)))
b_1_stage <- b_1 %>% 
  dplyr::mutate(stage=stage_b_1)
b_2_stage <- b_2 %>% 
  dplyr::mutate(stage=stage_b_2)
data_stage <- bind_rows(b_1_stage, b_2_stage)
data1 <- data_stage %>% filter(stage==1) %>% select(-stage)
data2 <- data_stage %>% filter(stage %in% c(1,2)) %>% select(-stage)
data3 <- data_stage %>% filter(stage %in% c(1,2,3)) %>% select(-stage)

# difLRT for each stage
res.difLRT1 <- difLRT(data1, group = "group", focal.name = "A", p.adjust.method="BH")
res.difLRT1$LRT
res.difLRT2 <- difLRT(data2, group = "group", focal.name = "A", p.adjust.method="BH")
res.difLRT2$LRT
res.difLRT3 <- difLRT(data3, group = "group", focal.name = "A", p.adjust.method="BH")
res.difLRT3$LRT

# group sequential design
design <- getDesignGroupSequential(kMax=3,
                                 alpha=.025,
                                 sided=1,
                                 typeOfDesign="asOF")
design
(design$criticalValues)^2



