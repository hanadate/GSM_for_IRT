library(tidyverse)
library(mirt)

# https://bookdown.org/sz_psyc490/r4psychometics/dif-detection.html
responses_dif <- read.table("https://raw.githubusercontent.com/sunbeomk/PSYC490/main/respnoses_dif.txt")
dim(responses_dif)
group <- read.table("https://raw.githubusercontent.com/sunbeomk/PSYC490/main/group.txt")$x
length(group)

dif_model <- multipleGroup(data = responses_dif, 
                           model = 1, 
                           itemtype = "3PL",
                           group = group, 
                           invariance = c(colnames(responses_dif)[-c(1:5)], 'free_means', 'free_var'),
                           verbose = FALSE
)

dif_test <- DIF(MGmodel = dif_model, 
                which.par = c("a1", "d", "g"), 
                items2test = c(1:5), 
                p.adjust = "BH")

dif_test


## 「Anxietyデータ」の読み込み
Anx<-read.csv("1/chapter05/PROMIS.csv")
head(Anx)
education <- as.factor(Anx$education)
Anx.e<-Anx[,-c(1:3)]
head(Anx.e)

dif_model <- multipleGroup(data = Anx.e, 
                           model = 1, 
                           itemtype = "Rasch",
                           group = education, 
                           verbose = FALSE
)

(dif_test <- DIF(MGmodel = dif_model, 
                which.par = c("d"),
                Wald=FALSE,
                p.adjust = "BH"))


## Verbal data
data("verbal")
# Excluding the "Anger" variable
gender <- as.character(verbal$Gender)[1:50]
verbal <- verbal[colnames(verbal)!=c("Anger","Gender")]
# Keeping the first 5 items and the first 50 subjects
# (this is an artificial simplification to reduce the computational time)
verbal <- verbal[1:50, c(1:5)]

dif_model <- multipleGroup(data = verbal, 
                           model = 1, 
                           itemtype = "Rasch",
                           group = gender,
                           verbose = FALSE
)

(dif_test <- DIF(MGmodel = dif_model, 
                 which.par = c("d"),
                 Wald=FALSE,
                 p.adjust = "BH"))
dif_test

