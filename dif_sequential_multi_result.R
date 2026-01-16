library(tidyverse)
res_easy_pocock <- read_csv("dif_sequential_multi_easy_pocock.csv")
res_easy_obf <- read_csv("dif_sequential_multi_easy_obf.csv")
res_difficult_pocock <- read_csv("dif_sequential_multi_difficult_pocock.csv")
res_difficult_obf <- read_csv("dif_sequential_multi_difficult_obf.csv")
res_middle_pocock <- read_csv("dif_sequential_multi_middle_pocock.csv")
res_middle_obf <- read_csv("dif_sequential_multi_middle_obf.csv")

res <- bind_rows(
  res_easy_pocock,
  res_easy_obf,
  res_difficult_pocock,
  res_difficult_obf,
  res_middle_pocock,
  res_middle_obf
)

res %>% glimpse

res_summary <- res %>% 
  select(position, boundary, stages, N, pdif, mdif, nitem, 
         detect_rate, expected_size)

#=== Type I error rate
res_summary %>% 
  filter(mdif==0, 
         stages==5,
         position=="middle",
         boundary=="pocock")

#=== substantial sample size reduction
res_summary %>% 
  filter(mdif %in% c(0.5,1.0),
         nitem==30,
         stages==5,
         position=="middle",
         boundary=="pocock")

#=== Boundary comparison
res_summary %>% 
  filter(mdif %in% c(0.5,1.0),
         nitem==30,
         stages==5,
         mdif==0.5,
         position=="middle")

#=== Impact of item difficulties
res_summary %>% 
  filter(mdif %in% c(0.5,1.0),
         nitem==30,
         pdif==0.1,
         mdif==0.5,
         stages==5,
         mdif==0.5,
         boundary=="pocock")






