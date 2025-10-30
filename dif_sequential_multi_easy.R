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
                      mdif=c(0.0,0.5,1.0),
                      pdif=c(0.1,0.3),
                      N=c(500,1000),
                      stages=c(2,5),
                      position="easy") %>% 
  mutate(ndif=nitem*pdif,
         params=row_number())
params
niter <- 1000

res <- foreach(k=1:nrow(params), .combine="rbind") %do% {
  param <- params[k,]
  print(param)
  print(now())
  #=== Create data
  # params
  a <- rep(1.0, param$nitem) # discrimination or slope
  b_ref <- seq(-3, 3, length.out=param$nitem) # difficulty of reference group
  b_focal <- c(b_ref[1:(length(b_ref)*param$pdif)]+param$mdif, 
               b_ref[(length(b_ref)*param$pdif+1):length(b_ref)])
  d_ref <- -a * b_ref # intercept of reference group
  d_focal <- -a * b_focal # intercept of focal group
  
  numCores <- parallel::detectCores()
  numCores
  cl <- makeCluster(numCores-5)
  registerDoParallel(cl)
  
  foreach(l=1:niter, .combine="rbind", 
          .packages=c("tidyverse","mirt","foreach","lme4")) %dopar% {  
            # simdata
            set.seed(l)
            dat_ref <- simdata(a=a, d=d_ref, N=param$N/2, itemtype="dich") %>% 
              as.data.frame() %>% 
              dplyr::mutate(group="ref") %>% 
              mutate(stage = as.integer((row_number() - 1) %% param$stages + 1))
            set.seed(l+niter)
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
write_csv(res, "zvalues_easy.csv")
res <- read_csv("zvalues_easy.csv")

#=== Critical values
pocock_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="P",
  sided=2,
  alpha=0.05,
  beta=0.2
)
pocock_5_df <- data.frame(stage=1:5, pocock_5=pocock_5$criticalValues)
pocock_2 <- getDesignGroupSequential(
  kMax=2,
  typeOfDesign="P",
  sided=2,
  alpha=0.05,
  beta=0.2
)
pocock_2_df <- data.frame(stage=1:2, pocock_2=pocock_2$criticalValues)

of_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="OF",
  sided=2,
  alpha=0.05,
  beta=0.2
)
of_5_df <- data.frame(stage=1:5, of_5=of_5$criticalValues)
of_2 <- getDesignGroupSequential(
  kMax=2,
  typeOfDesign="OF",
  sided=2,
  alpha=0.05,
  beta=0.2
)
of_2_df <- data.frame(stage=1:2, of_2=of_2$criticalValues)

#=== detect DIF
dif_detect <- res %>% 
  left_join(., params, by=c("params")) %>% 
  left_join(., pocock_2_df, by=c("stage")) %>% 
  left_join(., of_2_df, by=c("stage")) %>% 
  left_join(., pocock_5_df, by=c("stage")) %>% 
  left_join(., of_5_df, by=c("stage")) %>%
  mutate(pocock=ifelse(stages==2, pocock_2, pocock_5)) %>% 
  mutate(of=ifelse(stages==2, of_2, of_5)) %>% 
  select(-pocock_2, -pocock_5, -of_2, -of_5) %>% 
  mutate(pocock_dif=ifelse(abs(z)>pocock,1,0),
         of_dif=ifelse(abs(z)>of,1,0))
dif_detect

#=== results summary
dif_stage <- dif_detect %>% 
  dplyr::group_by(params, nsim) %>% 
  dplyr::mutate(pocock_dif_one=ifelse(row_number()==which(pocock_dif==1)[1],1,0),
                pocock_dif_one=replace_na(pocock_dif_one,0)) %>% 
  dplyr::mutate(of_dif_one=ifelse(row_number()==which(of_dif==1)[1],1,0),
                of_dif_one=replace_na(of_dif_one,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(params, stage) %>% 
  dplyr::summarise(pocock=sum(pocock_dif_one), of=sum(of_dif_one)) %>% 
  dplyr::left_join(., params, by=c("params"))
dif_stage
(pocock_stage <- dif_stage %>% 
    dplyr::select(nitem, mdif, pdif, N, stages, stage, pocock) %>% 
    tidyr::pivot_wider(names_from=stage, values_from=pocock) %>% 
    dplyr::mutate(expected_rate=ifelse(stages==2,
                                       (`1`*1/2+`2`*2/2)/(`1`+`2`),
                                       (`1`*1/5+`2`*2/5+`3`*3/5+`4`*4/5+`5`*5/5)/(`1`+`2`+`3`+`4`+`5`)),
                  expected_size=expected_rate*N,
                  detect_rate=ifelse(stages==2,
                                     (`1`+`2`)/niter,
                                     (`1`+`2`+`3`+`4`+`5`)/niter)))
pocock_stage %>% 
  ungroup %>% 
  select(nitem,mdif,pdif,N,stages,`1`,`2`,`3`,`4`,`5`,expected_size, detect_rate) %>% 
  as.matrix %>% 
  stargazer(., digits=2)
# pocock_stage %>% View

(of_stage <- dif_stage %>% 
    dplyr::select(nitem, mdif, pdif, N, stages, stage, of) %>% 
    tidyr::pivot_wider(names_from=stage, values_from=of) %>% 
    dplyr::mutate(expected_rate=ifelse(stages==2,
                                       (`1`*1/2+`2`*2/2)/(`1`+`2`),
                                       (`1`*1/5+`2`*2/5+`3`*3/5+`4`*4/5+`5`*5/5)/(`1`+`2`+`3`+`4`+`5`)),
                  expected_size=expected_rate*N,
                  detect_rate=ifelse(stages==2,
                                     (`1`+`2`)/niter,
                                     (`1`+`2`+`3`+`4`+`5`)/niter)))

of_stage %>% 
  ungroup %>% 
  select(nitem,mdif,pdif,N,stages,`1`,`2`,`3`,`4`,`5`,expected_size, detect_rate) %>% 
  as.matrix %>% 
  stargazer(., digits=2)
# of_stage %>% View
