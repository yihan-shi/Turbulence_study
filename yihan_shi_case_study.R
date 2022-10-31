library(tidyverse)
library(plyr)
# load data ---------------------------------------------------------------
train <- read.csv("data-train.csv")
test <- read.csv("data-test.csv")


# convert raw to central moments ------------------------------------------
train$C_moment_1 <- 0
train$C_moment_2 <- train$R_moment_2 - (train$R_moment_1)^2
train$C_moment_3 <- train$R_moment_3 - 3*train$R_moment_1*train$R_moment_2 + 2*(train$R_moment_1)^3
train$C_moment_4 <- train$R_moment_4 - 4*train$R_moment_1*train$R_moment_3 + 6*(train$R_moment_1)^2*train$R_moment_2 -3*(train$R_moment_1)^4


# ranges ------------------------------------------------------------------
plot(density(train$St))
plot(density(train$Re)) 
plot(density(train$Fr))
plot(density(train$R_moment_1))
plot(density(train$R_moment_2))
plot(density(train$R_moment_3))
plot(density(train$R_moment_4))

# transformation of Fr -------------------------------------------------------
# 6,600ft cloud: Fr = 0.052
# 40,000 ft cloud: Fr = 0.3
# highest possible cloud in the atmosphere is 280,000 ft: Fr = 2.08
grav_freq <- data.frame(height = c(6600,40000), fr = c(0.052, 0.3))
lm <- lm(fr ~ height, data = grav_freq)
new <- data.frame(height = c(280000))
pred_fr <- predict(lm, newdata = new)
train$Fr[which(train$Fr == Inf)] <- pred_fr

# fit linear regression ---------------------------------------------------
# what to predict?
lm1 <- lm(R_moment_1 ~ St + Re + Fr, data = train)
pred_r1 <- predict(lm1, test)
mean((pred_r1 - test$R_moment_1)^2)


## treat Fr as categorical
## 2 models for each raw moments (1 - prediction, 2 - inference)
## raw_moment_1