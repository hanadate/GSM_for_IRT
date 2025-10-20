library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)
library(foreach)
library(doParallel)
library(rpact)

t <- now()
#=== simulation settings
params <- expand.grid(nitem=c(10,30),
                      mdif=c(0.5,1.0),
                      pdif=c(0.1,0.3),
                      N=c(400,1000),
                      stages=c(2,5))
base_b <- 0.0

res <- foreach(k=1:nrow(params), .combine="rbind") %do% {
  param <- params[k,]
  print(param)
  #=== Create data
  # params
  a <- rep(0.5 , param$nitem) # discrimination or slope
  b_ref <- rep(base_b, param$nitem) # difficulty of reference group
  b_focal <- c(rep(base_b, param$nitem-(param$nitem*param$pdif)), 
               rep(base_b + param$mdif, param$nitem*param$pdif)) # difficulty of focal group
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
            
            items <- unique(dat_longer$item)
            difs <- foreach(i=items, .combine="cbind") %do% {
              new_col_name <- paste0("dif_", i)
              dif <- with(dat_longer, factor(0+(group=="focal" & item==i)))
              data.frame(dif) %>% 
                rename(!!new_col_name :=dif)
            }
            dat_longer_all <- cbind(dat_longer, difs)
            
            zvalues <- foreach(j=1:param$stages, .combine="rbind") %do% {
              print(paste0("stage: ",j))
              # calc z-value for each stage
              dat_longer <- dat_longer_all %>% 
                dplyr::filter(stage <= j) %>% 
                dplyr::select(-stage)
              
              coefs <- foreach(i=items, .combine="rbind") %do% { 
                print(i)
                dif <- paste0("dif_", i)
                form <- as.formula(paste0("resp ~ -1 + item + ", dif ," + group + (1 | id)"))
                res <- glmer(
                  form,
                  data=dat_longer, family=binomial
                )
                (coef <- summary(res)$coefficients)
                data.frame(z=coef[startsWith(rownames(coef), "dif_Item_"),"z value"],
                           item=i,
                           stage=j,
                           nsim=l,
                           params=k)
              }
            }
          }
}
now()-t
res
stopCluster(cl)
saveRDS(res, "zvalues.rds")
