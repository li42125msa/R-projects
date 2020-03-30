# initialize the data
data ("BostonHousing", package="mlbench")
original <- BostonHousing  # backup original data

# Introduce missing values
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"]