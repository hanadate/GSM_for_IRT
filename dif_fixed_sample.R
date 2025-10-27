library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)
library(foreach)
library(doParallel)
library(rpact)

#=== simulation settings
params <- expand.grid(nitem=c(10,30),
                      mdif=c(0.5,1.0),
                      pdif=c(0.1,0.3)) %>% 
  mutate(ndif=nitem*pdif)
params

power.dif.all <- foreach(k=1:nrow(params), .combine="rbind") %do% {
  param <- params[k,]
  print(param)
  print(now())
  #=== Create data
  # params
  a <- rep(1.0, param$nitem) # discrimination or slope
  b_ref <- seq(-3, 3, length.out=param$nitem) # difficulty of reference group
  b_focal <- c(b_ref[1:(length(b_ref)-length(b_ref)*param$pdif)],
               b_ref[(length(b_ref)-length(b_ref)*param$pdif+1):length(b_ref)]+param$mdif) # difficulty of focal group
  d_ref <- -a * b_ref # intercept of reference group
  d_focal <- -a * b_focal # intercept of focal group
  
  # simdata
  set.seed(1)
  dat_ref <- simdata(a=a, d=d_ref, N=500, itemtype="dich") %>% 
    as.data.frame() %>% 
    dplyr::mutate(group="ref")
  set.seed(1+1000)
  dat_focal <- simdata(a=a, d=d_focal, N=500, itemtype="dich") %>% 
    as.data.frame() %>% 
    dplyr::mutate(group="focal")
  
  # bind
  dat <- rbind(dat_ref, dat_focal) %>% 
    dplyr::mutate(id=row_number())
  
  # long form
  dat_longer <- tidyr::pivot_longer(dat, cols=starts_with("Item"), names_to="item", values_to="resp")
  
  items <- unique(dat_longer$item)
  dif_items <- items[(length(items)-param$ndif+1):length(items)]
  
  dif <- with(dat_longer, factor(0+(group=="focal" & item %in% dif_items)))
  dat_longer_all <- cbind(dat_longer, dif)
  
  res <- glmer(resp ~ -1 + item + dif + group + (1 | id),
               data=dat_longer_all, family=binomial)
  
  power.res <- mixedpower(model=res, data=dat_longer_all, 
                          fixed_effects=c("item", "dif", "group"),
                          simvar="id", steps=c(500,1000,1500,2000),
                          critical_value=2, n_sim=100)
  saveRDS(power.res, paste0("power_params_",k,".rds"))
  power.dif <- power.res %>% 
    dplyr::mutate(params=k) %>% 
    dplyr::filter(effect=="dif1")
  return(power.dif)
}
power.dif.all
saveRDS(power.dif.all, "power_dif_all.rds")
power.dif.all <- readRDS("power_dif_all.rds")
power.dif.all




