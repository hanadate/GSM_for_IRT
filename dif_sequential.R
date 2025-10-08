library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)
library(foreach)
library(doParallel)

#=== Create data
# params
a <- c(0.5, 0.5, 0.5, 0.5, 0.5) # discrimination or slope
b_ref <- c(0.0 ,0.0 ,0.0 ,0.0 ,0.0) # difficulty of reference group
b_focal <- c(-1.0,-0.5,0.0,0.5,1.0) # difficulty of focal group
d_ref <- -a * b_ref # intercept of reference group
d_focal <- -a * b_focal # intercept of focal group
N <- c(100,200,300,400)

# simdata
set.seed(1)
dat_ref <- simdata(a=a, d=d_ref, N=200, itemtype="dich") %>% 
  as.data.frame() %>% 
  dplyr::mutate(group="ref") %>% 
  mutate(stage = as.integer((row_number() - 1) %% 5 + 1))
set.seed(1)
dat_focal <- simdata(a=a, d=d_focal, N=200, itemtype="dich") %>% 
  as.data.frame() %>% 
  dplyr::mutate(group="focal") %>% 
  mutate(stage = as.integer((row_number() - 1) %% 5 + 1))

# bind
dat <- rbind(dat_ref, dat_focal) %>% 
  dplyr::mutate(id=row_number())

# long form
dat_longer <- tidyr::pivot_longer(dat, cols=starts_with("Item"), names_to="item", values_to="resp")

dif_item1 <- with(dat_longer, factor(0+(group=="focal" & item=="Item_1")))
dif_item2 <- with(dat_longer, factor(0+(group=="focal" & item=="Item_2")))
dif_item3 <- with(dat_longer, factor(0+(group=="focal" & item=="Item_3")))
dif_item4 <- with(dat_longer, factor(0+(group=="focal" & item=="Item_4")))
dif_item5 <- with(dat_longer, factor(0+(group=="focal" & item=="Item_5")))
dat_longer_all <- cbind(dat_longer, dif_item1, dif_item2, dif_item3, dif_item4, dif_item5)

#=== iteration for each stage
dif_coefs <- foreach(i=1:5, .combine="rbind") %do% {
  print(paste0("iter ",i))
  dat_longer <- dat_longer_all %>% 
    dplyr::filter(stage <= i) %>% 
    dplyr::select(-stage)
  
  #=== Item 1 (lower difficulty)
  res_item1 <- glmer(
    resp ~ -1 + item + dif_item1 + group + (1 | id),
    data=dat_longer, family=binomial
  )
  (coef_item1 <- summary(res_item1)$coefficients)
  
  #=== Item 2 (slightly lower difficulty)
  res_item2 <- glmer(
    resp ~ -1 + item + dif_item2 + group + (1 | id),
    data=dat_longer, family=binomial
  )
  (coef_item2 <- summary(res_item2)$coefficients)
  (z_item2 <- coef_item2["dif_item21", "z value"]) 
  
  #=== Item 3 (same difficulty)
  res_item3 <- glmer(
    resp ~ -1 + item + dif_item3 + group + (1 | id),
    data=dat_longer, family=binomial
  )
  (coef_item3 <- summary(res_item3)$coefficients)
  
  #=== Item 4 (slightly larger difficulty)
  res_item4 <- glmer(
    resp ~ -1 + item + dif_item4 + group + (1 | id),
    data=dat_longer, family=binomial
  )
  (coef_item4 <- summary(res_item4)$coefficients)

    #=== Item 5 (larger difficulty)
  res_item5 <- glmer(
    resp ~ -1 + item + dif_item5 + group + (1 | id),
    data=dat_longer, family=binomial
  )
  (coef_item5 <- summary(res_item5)$coefficients)
  
  #=== combine estimates of DIF covariate
  res <- rbind(coef_item1["dif_item11",],
        coef_item2["dif_item21",],
        coef_item3["dif_item31",],
        coef_item4["dif_item41",],
        coef_item5["dif_item51",]) %>% 
    as.data.frame() %>% 
    mutate(item=c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5"), .before=1) %>% 
    mutate(stage=i)
  return(res)
}
saveRDS(dif_coefs, "dif_coefs.rds")
dif_coefs <- readRDS("dif_coefs.rds")  

dif_coefs %>% 
  dplyr::select(item, `z value`, stage) %>% 
  dplyr::mutate(`z value`=round(`z value`,3)) %>% 
  tidyr::pivot_wider(names_from=stage, values_from=`z value`) %>% 
  as.matrix() %>% 
  stargazer()


