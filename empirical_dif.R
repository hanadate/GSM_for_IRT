library(tidyverse)
library(difR)
library(lme4)
library(foreach)
library(doParallel)
library(rpact)
library(stargazer)
library(psych)

dat_resp <- read_csv("resp_wide.csv")
dat_resp[is.na(dat_resp)] <- 0
dat_prof <- read_csv("dat_prof.csv")

# sc3: membership of school that study disabilities
dat_prof$sc3 %>% table # 192 / 1575
dat <- dat_resp %>% 
  mutate(sc3 = dat_prof$sc3) %>% 
  mutate(group=ifelse(sc3==1, "focal", "ref")) %>% 
  select(group, starts_with("q"))

# passing rate
passing_rate <- dat %>% 
  group_by(group) %>% 
  summarise(across(starts_with("q"), mean)) %>% 
  t 
passing_name <- passing_rate[1,]
passing_rate <- passing_rate[-1,]
colnames(passing_rate) <- passing_name
type.convert(passing_rate, as.is=TRUE) %>% 
  stargazer(., digits=2)

# Item-Total Correlation
it_cor_long <- dat %>% 
  group_split(group) %>% 
  map_dfr(function(sub_df){
    current_group <- sub_df$group[1]
    items_only <- sub_df %>% select(-group)
    res <- alpha(items_only, warnings=FALSE, check.keys=FALSE)
    data.frame(
      Item=rownames(res$item.stats),
      Group=current_group,
      IT_correlation=res$item.stats$r.drop
    )
  })
it_cor_wide <- it_cor_long %>% 
  pivot_wider(names_from=Group, values_from=IT_correlation) %>% 
  as.matrix()
it_cor_wide_name <- it_cor_wide[,1]
it_cor_wide <- it_cor_wide[,-1]
rownames(it_cor_wide) <- it_cor_wide_name
type.convert(it_cor_wide, as.is=TRUE) %>% 
  stargazer(., digits=2)

# Lord's DIF detection
lrt_res <- difLord(Data = dat, group = "group", 
                   focal.name = "focal", model = "1PL")
lrt_res
# q1, q2, q12, q15, q21, q22, q28, q36, q37, q41
# large effect: q15, q21
lrt_res

#============================

# define stage
numCores <- parallel::detectCores()
numCores
cl <- makeCluster(numCores-4)
registerDoParallel(cl)

niter <- 1000
res <- foreach(i=1:niter, .combine="rbind", 
        .packages=c("dplyr","tidyr","foreach","lme4")) %dopar% { 
  set.seed(i)
  dat <- dat_resp %>% 
    dplyr::mutate(sc3 = dat_prof$sc3) %>% 
    dplyr::mutate(group=ifelse(sc3==1, "focal", "ref")) %>% 
    dplyr::select(no, group, starts_with("q")) %>% 
    dplyr::mutate(stage = sample(rep(1:5, length.out = n())))
  
  # long format
  dat_longer <- tidyr::pivot_longer(dat, cols=starts_with("q"), names_to="item", values_to="resp")
  
  # define DIF item
  items <- unique(dat_longer$item)
  # dif_items <- items[c(1,2,12,15,21,22,28,36,37,41)]
  dif_items <- items[c(15,21)]
  
  dif <- with(dat_longer, factor(0+(group=="focal" & item %in% dif_items)))
  dat_longer_all <- cbind(dat_longer, dif)
  
  # 
  zvalues_tmp <- foreach(j=1:5, .combine="rbind") %do% {
    print(paste0("stage: ",j))
    # calc z-value for each stage
    dat_longer <- dat_longer_all %>% 
      dplyr::filter(stage <= j) %>% 
      dplyr::select(-stage)
    res <- glmer(resp ~ -1 + item + dif + group + (1 | no),
                 data=dat_longer, family=binomial, nAGQ=0)
    (coef <- summary(res)$coefficients)
    data.frame(z=coef[startsWith(rownames(coef), "dif"),"z value"],
               stage=j)
  }
  zvalues_tmp <- zvalues_tmp %>% 
    mutate(nsim=i)
  return(zvalues_tmp)
}
res
stopCluster(cl)
write_csv(res, "zvalues_empirical.csv")
res <- read_csv("zvalues_empirical.csv")


#=== Critical values
pocock_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="P",
  sided=2,
  alpha=0.05,
  beta=0.2
)
pocock_5_df <- data.frame(stage=1:5, pocock_5=pocock_5$criticalValues)

of_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="OF",
  sided=2,
  alpha=0.05,
  beta=0.2
)
of_5_df <- data.frame(stage=1:5, of_5=of_5$criticalValues)

#=== DIF detection
dif_detect <- res %>% 
  left_join(., pocock_5_df, by=c("stage")) %>% 
  left_join(., of_5_df, by=c("stage")) %>% 
  select(stage, pocock_5, of_5, z, nsim) %>% 
  mutate(pocock_dif=ifelse(abs(z)>pocock_5,1,0),
         of_dif=ifelse(abs(z)>of_5,1,0))
dif_detect
dif_stage <- dif_detect %>% 
  dplyr::group_by(nsim) %>% 
  dplyr::mutate(pocock_dif_one=ifelse(row_number()==which(pocock_dif==1)[1],1,0),
                pocock_dif_one=replace_na(pocock_dif_one,0)) %>% 
  dplyr::mutate(of_dif_one=ifelse(row_number()==which(of_dif==1)[1],1,0),
                of_dif_one=replace_na(of_dif_one,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(stage) %>% 
  dplyr::summarise(pocock=sum(pocock_dif_one), of=sum(of_dif_one))
dif_stage

(pocock_stage <- dif_stage %>% 
    dplyr::select(stage, pocock) %>% 
    tidyr::pivot_wider(names_from=stage, values_from=pocock) %>% 
    dplyr::mutate(expected_rate=(`1`*1/5+`2`*2/5+`3`*3/5+`4`*4/5+`5`*5/5)/(`1`+`2`+`3`+`4`+`5`), 
                  detect_rate=(`1`+`2`+`3`+`4`+`5`)/niter,
                  expected_size=(detect_rate*expected_rate+(1-detect_rate)*1)*nrow(dat)) %>% 
    dplyr::mutate(method="pocock"))

(of_stage <- dif_stage %>% 
    dplyr::select(stage, of) %>% 
    tidyr::pivot_wider(names_from=stage, values_from=of) %>% 
    dplyr::mutate(expected_rate=(`1`*1/5+`2`*2/5+`3`*3/5+`4`*4/5+`5`*5/5)/(`1`+`2`+`3`+`4`+`5`), 
                  detect_rate=(`1`+`2`+`3`+`4`+`5`)/niter,
                  expected_size=(detect_rate*expected_rate+(1-detect_rate)*1)*nrow(dat)) %>% 
    dplyr::mutate(method="of"))

rbind(pocock_stage, of_stage) %>%  
  select(method,`1`,`2`,`3`,`4`,`5`, detect_rate, expected_size) %>% 
  as.matrix() %>% 
  stargazer(., digits=2)

