library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)
library(foreach)
library(doParallel)
library(rpact)

(t <- now())
#=== simulation settings
params <- expand.grid(nitem=c(10,30),
                      mdif=c(0.5,1.0),
                      pdif=c(0.1,0.3),
                      N=c(400,1000),
                      stages=c(2,5)) %>% 
  mutate(ndif=nitem*pdif)
params


res <- foreach(k=1:nrow(params), .combine="rbind") %do% {
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
  
  numCores <- parallel::detectCores()
  numCores
  cl <- makeCluster(numCores-1)
  registerDoParallel(cl)
  
  foreach(l=1:1000, .combine="rbind", 
          .packages=c("tidyverse","mirt","foreach","lme4")) %dopar% {  # simdata
            set.seed(l)
            dat_ref <- simdata(a=a, d=d_ref, N=param$N/2, itemtype="dich") %>% 
              as.data.frame() %>% 
              dplyr::mutate(group="ref") %>% 
              mutate(stage = as.integer((row_number() - 1) %% param$stages + 1))
            set.seed(l)
            dat_focal <- simdata(a=a, d=d_focal, N=param$N/2, itemtype="dich") %>% 
              as.data.frame() %>% 
              dplyr::mutate(group="focal") %>% 
              mutate(stage = as.integer((row_number() - 1) %% param$stages + 1))
            
            # bind
            dat <- rbind(dat_ref, dat_focal) %>% 
              dplyr::mutate(id=row_number())
            
            # long form
            dat_longer <- tidyr::pivot_longer(dat, cols=starts_with("Item"), names_to="item", values_to="resp")
            
            # TODO
            items <- unique(dat_longer$item)
            dif_items <- items[(length(items)-param$ndif+1):length(items)]
            
            dif <- with(dat_longer, factor(0+(group=="focal" & item %in% dif_items)))
            dat_longer_all <- cbind(dat_longer, dif)
            
            
            zvalues <- foreach(j=1:param$stages, .combine="rbind") %do% {
              print(paste0("stage: ",j))
              # calc z-value for each stage
              dat_longer <- dat_longer_all %>% 
                dplyr::filter(stage <= j) %>% 
                dplyr::select(-stage)
              res <- glmer(resp ~ -1 + item + dif + group + (1 | id),
                           data=dat_longer, family=binomial)
              (coef <- summary(res)$coefficients)
              data.frame(z=coef[startsWith(rownames(coef), "dif"),"z value"],
                         stage=j,
                         nsim=l,
                         params=k)
            }
          }
}
(now()-t)
res
stopCluster(cl)
saveRDS(res, "zvalues.rds")
