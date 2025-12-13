library(tidyverse)
library(stargazer)

params_easy <- expand.grid(nitem=c(10,30), # input even number
                      mdif=c(0,0.5,1.0),
                      pdif=c(0.1,0.3),
                      position=c("easy")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())
params_middle <- expand.grid(nitem=c(10,30), # input even number
                           mdif=c(0,0.5,1.0),
                           pdif=c(0.1,0.3),
                           position=c("middle")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())
params_difficult <- expand.grid(nitem=c(10,30), # input even number
                           mdif=c(0,0.5,1.0),
                           pdif=c(0.1,0.3),
                           position=c("difficult")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())


dif_power_easy <- read_csv("dif_power_easy.csv") %>% 
  inner_join(., params_easy, by=c("param"))
dif_power_middle <- read_csv("dif_power_middle.csv") %>% 
  inner_join(., params_middle, by=c("param"))
dif_power_difficult <- read_csv("dif_power_difficult.csv") %>% 
  inner_join(., params_difficult, by=c("param"))

dif_power_all <- 
  bind_rows(dif_power_easy, dif_power_middle, dif_power_difficult) %>% 
  select(position, pdif, mdif, nitem, `250`, `500`, `750`, `1000`)
dif_power_all %>% 
  as.matrix() %>% 
  stargazer()
