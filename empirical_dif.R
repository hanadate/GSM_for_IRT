library(tidyverse)
library(difR)
library(lme4)
library(foreach)
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
dat <- dat_resp %>% 
  mutate(sc3 = dat_prof$sc3) %>% 
  mutate(group=ifelse(sc3==1, "focal", "ref")) %>% 
  select(no, group, starts_with("q"))
dat$stage <- rep(1:5, each = nrow(dat) / 5)

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
zvalues_tmp

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
dif_detect <- zvalues_tmp %>% 
  left_join(., pocock_5_df, by=c("stage")) %>% 
  left_join(., of_5_df, by=c("stage")) %>% 
  select(stage, pocock_5, of_5, z)
dif_detect %>% 
  as.matrix() %>% 
  stargazer()



