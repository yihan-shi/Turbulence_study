
# load data ---------------------------------------------------------------
train <- read.csv("data-train.csv")
test <- read.csv("data-test.csv")


# convert raw to central moments ------------------------------------------
train$C_moment_1 <- 0
train$C_moment_2 <- train$R_moment_2 - (train$R_moment_1)^2
train$C_moment_3 <- train$R_moment_3 - 3*train$R_moment_1*train$R_moment_2 + 2*(train$R_moment_1)^3
train$C_moment_4 <- train$R_moment_4 - 4*train$R_moment_1*train$R_moment_3 + 6*(train$R_moment_1)^2*train$R_moment_2 -3*(train$R_moment_1)^4

# 