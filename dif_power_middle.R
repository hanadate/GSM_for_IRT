library(tidyverse)
library(lme4)
library(mixedpower)
library(mirt)
library(foreach)
library(doParallel)


params <- expand.grid(nitem=c(10,30), # input even number
                      mdif=c(0,0.5,1.0),
                      pdif=c(0.1,0.3),
                      position="middle") %>% 
  mutate(ndif=nitem*pdif,
         params=row_number())
params
simN <- c(250,500,750,1000)

dif_power_all <-  foreach(k=1:nrow(params), .combine="rbind") %do% {
  param <- params[k,]
  print(now())
  print(param)
  #=== Create data
  # params
  a <- rep(1.0, param$nitem) # discrimination or slope
  b_ref <- seq(-3, 3, length.out=param$nitem) # difficulty of reference group
  b_focal <- c(b_ref[1:(length(b_ref)/2-round(param$ndif/2,0))],
               b_ref[(length(b_ref)/2-round(param$ndif/2,0)+1):(length(b_ref)/2-round(param$ndif/2,0)+param$ndif)]+param$mdif,
               b_ref[(length(b_ref)/2-round(param$ndif/2,0)+param$ndif+1):length(b_ref)])
  d_ref <- -a * b_ref # intercept of reference group
  d_focal <- -a * b_focal # intercept of focal group
 
  # simdata
  set.seed(1111)
  dat_ref <- simdata(a=a, d=d_ref, N=max(simN), itemtype="dich") %>% 
    as.data.frame() %>% 
    dplyr::mutate(group="ref")
  set.seed(2222)
  dat_focal <- simdata(a=a, d=d_focal, N=max(simN), itemtype="dich") %>% 
    as.data.frame() %>% 
    dplyr::mutate(group="focal")
  
  # bind
  dat <- rbind(dat_ref, dat_focal) %>% 
    dplyr::mutate(id=row_number())
  
  # long form
  dat_longer <- tidyr::pivot_longer(dat, cols=starts_with("Item"), names_to="item", values_to="resp")
  
  items <- unique(dat_longer$item)
  dif_items <- items[(length(items)/2-round(param$ndif/2,0)+1):(length(items)/2-round(param$ndif/2,0)+param$ndif)]
  
  dif <- with(dat_longer, factor(0+(group=="focal" & item %in% dif_items)))
  dat_longer_all <- cbind(dat_longer, dif)
  
  res <- glmer(resp ~ -1 + item + dif + group + (1 | id),
               data=dat_longer_all, family=binomial, nAGQ=0)
  
  power.res <- mixedpower(model=res, data=dat_longer_all, 
                          fixed_effects=c("item", "dif", "group"),
                          simvar="id", steps=simN,
                          critical_value=2, n_sim=1000,
                          SESOI=F, databased=T)
  dif_power <- power.res %>% 
    filter(effect == "dif1") %>% 
    mutate(param=k)
  print(dif_power)
  return(dif_power)
}
write_csv(dif_power_all, "dif_power_middle.csv")

dif_power_all <- read_csv("dif_power_middle.csv")
dif_power_all


