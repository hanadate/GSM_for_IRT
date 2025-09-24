library(rpact)
library(tidyverse)

pocock_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="P",
  sided=2,
  alpha=0.05,
  beta=0.2
)
summary(pocock_5)
plot(pocock_5)

of_5 <- getDesignGroupSequential(
  kMax=5,
  typeOfDesign="OF",
  sided=2,
  alpha=0.05,
  beta=0.2
)
summary(of_5)
plot(of_5)

# designSet <- getDesignSet(designs = c(pocock_5, of_5), variedParameters = "typeOfDesign")
# plot(designSet, type=1)
