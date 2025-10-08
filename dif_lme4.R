library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)

# #=== Example for detecting DIF by glmer
# dif <- with(VerbAgg, factor(0 + (Gender=="F" & mode == "do" & btype!="shout")))
# dif_gender <- glmer(
#   r2 ~ -1 + item + dif + Gender + (1 | id), 
#   data=VerbAgg, family=binomial
# )
# summary(dif_gender)

#=== Create data
# params
a <- c(0.5, 0.5, 0.5, 0.5, 0.5) # discrimination or slope
# a <- c(1,1,1,1,1) # discrimination or slope
b_ref <- c(0.0 ,0.0 ,0.0 ,0.0 ,0.0) # difficulty of reference group
b_focal <- c(-1.0,-0.5,0.0,0.5,1.0) # difficulty of focal group
d_ref <- -a * b_ref # intercept of reference group
d_focal <- -a * b_focal # intercept of focal group
N <- c(100,200,300,400)

# simdata
set.seed(1)
dat_ref <- simdata(a=a, d=d_ref, N=200, itemtype="dich") %>% 
  as.data.frame() %>% 
  dplyr::mutate(group="ref")
set.seed(1)
dat_focal <- simdata(a=a, d=d_focal, N=200, itemtype="dich") %>% 
  as.data.frame() %>% 
  dplyr::mutate(group="focal")

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
dat_longer <- cbind(dat_longer, dif_item1, dif_item2, dif_item3, dif_item4, dif_item5)

#=== Item 1 (lower difficulty)
res_item1 <- glmer(
  resp ~ -1 + item + dif_item1 + group + (1 | id),
  data=dat_longer, family=binomial
)
(coef_item1 <- summary(res_item1)$coefficients)
(z_item1 <- coef_item1["dif_item11", "z value"])

power_item1 <- mixedpower(model=res_item1, data=dat_longer,
                          fixed_effects=c("dif_item1"),
                          simvar="id", steps=N, critical_value=2,
                          n_sim=1000, SESOI=FALSE, databased=TRUE)
saveRDS(power_item1, "power_item1.rds")
(power_item1 <- readRDS("power_item1.rds"))

#=== Item 2 (slightly lower difficulty)
res_item2 <- glmer(
  resp ~ -1 + item + dif_item2 + group + (1 | id),
  data=dat_longer, family=binomial
)
(coef_item2 <- summary(res_item2)$coefficients)
(z_item2 <- coef_item2["dif_item21", "z value"]) 

power_item2 <- mixedpower(model=res_item2, data=dat_longer,
                          fixed_effects=c("dif_item2"),
                          simvar="id", steps=N, critical_value=2,
                          n_sim=1000, SESOI=FALSE, databased=TRUE)
saveRDS(power_item2, "power_item2.rds")
(power_item2 <- readRDS("power_item2.rds"))

#=== Item 3 (same difficulty)
res_item3 <- glmer(
  resp ~ -1 + item + dif_item3 + group + (1 | id),
  data=dat_longer, family=binomial
)
(coef_item3 <- summary(res_item3)$coefficients)
(z_item3 <- coef_item3["dif_item31", "z value"])

power_item3 <- mixedpower(model=res_item3, data=dat_longer,
                          fixed_effects=c("dif_item3"),
                          simvar="id", steps=N, critical_value=2,
                          n_sim=1000, SESOI=FALSE, databased=TRUE)
saveRDS(power_item3, "power_item3.rds")
(power_item3 <- readRDS("power_item3.rds"))

#=== Item 4 (slightly larger difficulty)
res_item4 <- glmer(
  resp ~ -1 + item + dif_item4 + group + (1 | id),
  data=dat_longer, family=binomial
)
(coef_item4 <- summary(res_item4)$coefficients)
(z_item4 <- coef_item4["dif_item41", "z value"])

power_item4 <- mixedpower(model=res_item4, data=dat_longer,
                          fixed_effects=c("dif_item4"),
                          simvar="id", steps=N, critical_value=2,
                          n_sim=1000, SESOI=FALSE, databased=TRUE)
saveRDS(power_item4, "power_item4.rds")
(power_item4 <- readRDS("power_item4.rds"))


#=== Item 5 (larger difficulty)
res_item5 <- glmer(
  resp ~ -1 + item + dif_item5 + group + (1 | id),
  data=dat_longer, family=binomial
)
(coef_item5 <- summary(res_item5)$coefficients)
(z_item5 <- coef_item5["dif_item51", "z value"])

power_item5 <- mixedpower(model=res_item5, data=dat_longer,
                          fixed_effects=c("dif_item5"),
                          simvar="id", steps=N, critical_value=2,
                          n_sim=1000, SESOI=FALSE, databased=TRUE)
saveRDS(power_item5, "power_item5.rds")
(power_item5 <- readRDS("power_item5.rds"))

#=== combine results of power analysis 
rbind(power_item1["dif_item11",],
      power_item2["dif_item21",],
      power_item3["dif_item31",],
      power_item4["dif_item41",],
      power_item5["dif_item51",]) %>% 
  as.data.frame() %>% 
  dplyr::select(`100`, `200`, `300`, `400`) %>% 
  as.matrix %>% 
  stargazer()

#=== combine estimates of DIF covariate
rbind(coef_item1["dif_item11",],
      coef_item2["dif_item21",],
      coef_item3["dif_item31",],
      coef_item4["dif_item41",],
      coef_item5["dif_item51",]) %>% 
  as.data.frame() %>% 
  as.matrix() %>% 
  stargazer()


