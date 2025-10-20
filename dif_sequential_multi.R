library(lme4)
library(mirt)
library(mixedpower)
library(tidyverse)
library(stargazer)
library(foreach)
library(doParallel)
library(rpact)

#=== Create data
# params
a <- c(0.5, 0.5, 0.5, 0.5, 0.5) # discrimination or slope
b_ref <- c(0.0 ,0.0 ,0.0 ,0.0 ,0.0) # difficulty of reference group
b_focal <- c(-1.0,-0.5,0.0,0.5,1.0) # difficulty of focal group
d_ref <- -a * b_ref # intercept of reference group
d_focal <- -a * b_focal # intercept of focal group
N <- c(100,200,300,400)

numCores <- parallel::detectCores()
numCores
cl <- makeCluster(numCores-1)
registerDoParallel(cl)

t <- proc.time()
dif_coefs_multi <- foreach(j=1:1000, .combine="rbind", 
                           .packages=c("tidyverse","mirt","foreach","lme4")) %dopar% {
  # simdata
  set.seed(j)
  dat_ref <- simdata(a=a, d=d_ref, N=200, itemtype="dich") %>% 
    as.data.frame() %>% 
    dplyr::mutate(group="ref") %>% 
    mutate(stage = as.integer((row_number() - 1) %% 5 + 1))
  set.seed(j)
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
      mutate(stage=i) %>% 
      mutate(nsim=j)
    return(res)
  }
}
(proc.time()-t)
dif_coefs_multi
stopCluster(cl)
saveRDS(dif_coefs_multi, "dif_coefs_multi.rds")
dif_coefs_multi <- readRDS("dif_coefs_multi.rds")

#=== Critical values
pocock_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="P",
  sided=2,
  alpha=0.05,
  beta=0.2
)
pocock_cv <- data.frame(stage=1:5, pocock=pocock_5$criticalValues)
of_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="OF",
  sided=2,
  alpha=0.05,
  beta=0.2
)
of_cv <- data.frame(stage=1:5, of=of_5$criticalValues)

#=== detect DIF 
dif_coefs_multi_cv <- dif_coefs_multi %>% 
  dplyr::left_join(., pocock_cv, by=c("stage")) %>% 
  dplyr::left_join(., of_cv, by=c("stage")) %>% 
  dplyr::mutate(pocock_dif=ifelse(abs(`z value`)>pocock, 1, 0),
                of_dif=ifelse(abs(`z value`)>of, 1, 0)) %>% 
  dplyr::arrange(nsim,item,stage) %>% 
  dplyr::select(nsim, item, stage, `z value`, pocock, pocock_dif, of, of_dif)

#=== results summary
dif_stage <- dif_coefs_multi_cv %>% 
  dplyr::group_by(nsim, item) %>% 
  dplyr::mutate(pocock_dif_cumsum=cumsum(pocock_dif),
                pocock_dif_one=ifelse(pocock_dif_cumsum %in% c(1,0), pocock_dif_cumsum, 0)) %>% 
  dplyr::select(-pocock_dif, -pocock_dif_cumsum) %>% 
  dplyr::mutate(of_dif_cumsum=cumsum(of_dif),
                of_dif_one=ifelse(of_dif_cumsum %in% c(1,0), of_dif_cumsum, 0)) %>% 
  dplyr::select(-of_dif, -of_dif_cumsum) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(item, stage) %>% 
  dplyr::summarise(pocock=sum(pocock_dif_one), of=sum(of_dif_one)) 
dif_stage
(pocock_stage <- dif_stage %>% 
  dplyr::select(item, stage, pocock) %>% 
  tidyr::pivot_wider(names_from=stage, values_from=pocock))
(of_stage <- dif_stage %>% 
  dplyr::select(item, stage, of) %>% 
  tidyr::pivot_wider(names_from=stage, values_from=of))

#=== stargazer
pocock_stage %>% 
  as.matrix() %>% 
  stargazer()

of_stage %>% 
  as.matrix() %>% 
  stargazer()

#=== expected reduction
pocock_stage %>%
  ungroup() %>% 
  dplyr::select(-item) %>% 
  rowSums()
# pocock item1
((11*1+66*2+172*3+277*4+233*5)/759)*20 
# pocock item5
((13*1+57*2+173*3+297*4+240*5)/780)*20
of_stage %>%
  ungroup() %>% 
  dplyr::select(-item) %>% 
  rowSums()
# of item1
((1*1+2*3+3*129+4*487+5*318)/938)*20
# of item5
((1*0+2*3+3*106+4*513+5*320)/942)*20
