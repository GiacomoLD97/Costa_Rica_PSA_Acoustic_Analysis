### 1. LOAD PACKAGES ##############################
#Trying XGBoost Modelling
library(pacman)
p_load(xgboost) #the main algorithm
p_load(dplyr)
p_load(farff)
p_load(OpenML)
p_load(dplyr)
p_load(ggplot2)
p_load(SHAPforxgboost)

### 2. LOAD AND PREP DATA ##############################

ModellingData <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/ModellingDataForFigure2oct2023.csv") # Correct path to correct file


round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

agg_data <- ModellingData %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  dplyr::group_by(Type, nearest_10) %>%
  mutate(MeanPMN = mean(SummedPMN), MeanNoise = mean(SummedNoise)) %>%
  ungroup() 

averagedmodellingdata <- agg_data %>% subset(select = -c(Minute, TimeStamp, SummedPMN, SummedNoise, MicType, Latitude, Longitude)) %>% unique()
quartiles <- quantile(averagedmodellingdata$MeanPMN, probs = c(0.2, 0.4, 0.6, 0.8))

#In case we want to use categorical response variable
averagedmodellingdata <- averagedmodellingdata %>%
  mutate(Level = case_when(
    MeanPMN <= quartiles[1] ~ "Low",
    MeanPMN <= quartiles[2] ~ "Medium Low",
    MeanPMN <= quartiles[3] ~ "Medium",
    MeanPMN <= quartiles[4] ~ "Medium High", 
    TRUE ~ "High"
  ))

#In case we want to use binary type variable 
averagedmodellingdata$Plantationtype <- ifelse(grepl("Plantation", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$Pasturetype <- ifelse(grepl("Pasture", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$NatRegentype <- ifelse(grepl("Natural_Regeneration", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$Reftype <- ifelse(grepl("Reference_Forest", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$PSA <- ifelse(grepl("Reference_Forest", averagedmodellingdata$Type, ignore.case = TRUE), FALSE, ifelse(grepl("Pasture", averagedmodellingdata$Type), FALSE, TRUE))



### 3. XGBOOST FOR SHAP WITH 10-MIN DATA AND BINARY TYPE CATEGORIES ##############################

#Data
xgpreddata <- averagedmodellingdata[,-c(1, 2, 11, 16)]

#Define response and features
x <- colnames(xgpreddata)[-7]
y <- "MeanPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix <- sample(nrow(xgpreddata), (nrow(xgpreddata)*0.8)) #80% training dataset

xgpreddata <- as.data.frame(xgpreddata)
dtrain <- xgb.DMatrix(data.matrix(xgpreddata[ix, x]),
                      label = xgpreddata[ix, y])
dvalid <- xgb.DMatrix(data.matrix(xgpreddata[-ix, x]),
                      label = xgpreddata[-ix, y])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist = list(train=dtrain, test=dvalid)

fit_xgb <- xgb.train(
  params,
  data = dtrain,
  watchlist = watchlist,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)


X <- data.matrix(xgpreddata[sample(nrow(xgpreddata), 1000), x])

shap <- shap.prep(fit_xgb, X_train = X)

# SHAP importance
shap.plot.summary(shap)

# Loop over dependence plots in decreasing importance
for (v in shap.importance(shap, names_only = TRUE)) {
  p <- shap.plot.dependence(shap, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}


### 4. XGBOOST FOR SHAP WITH MINUTE BY MINUTE DATA AND BINARY TYPE CATEGORIES ##############################

#Perhaps we try with non-averaged data, using minute instead of ten-min timebin

ModellingData <- ModellingData %>% subset(select = -c(TimeStamp, Latitude, Longitude, Site))
ModellingData$Plantationtype <- ifelse(grepl("Plantation", ModellingData$Type, ignore.case = TRUE), TRUE, FALSE)
ModellingData$Pasturetype <- ifelse(grepl("Pasture", ModellingData$Type, ignore.case = TRUE), TRUE, FALSE)
ModellingData$NatRegentype <- ifelse(grepl("Natural_Regeneration", ModellingData$Type, ignore.case = TRUE), TRUE, FALSE)
ModellingData$Reftype <- ifelse(grepl("Reference_Forest", ModellingData$Type, ignore.case = TRUE), TRUE, FALSE)
ModellingData$MicType <- ifelse(grepl("Gen1.1", ModellingData$MicType), 0, 1)
ModellingData <- ModellingData %>% subset(select = -Type)

xg2 <- ModellingData

#Define response and features
x2 <- colnames(xg2)[-2]
y2 <- "SummedPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix2 <- sample(nrow(xg2), (nrow(xg2)*0.8)) #80% training dataset

xg2 <- as.data.frame(xg2)
dtrain2 <- xgb.DMatrix(data.matrix(xg2[ix2, x2]),
                      label = xg2[ix2, y2])
dvalid2 <- xgb.DMatrix(data.matrix(xg2[-ix2, x2]),
                      label = xg2[-ix2, y2])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist2 = list(train=dtrain2, test=dvalid2)

fit_xgb2 <- xgb.train(
  params,
  data = dtrain2,
  watchlist = watchlist2,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)


#Unaggregated data?


X2 <- data.matrix(xg2[sample(nrow(xg2), 1000), x2])

shap2 <- shap.prep(fit_xgb2, X_train = X2)

# SHAP plot
shap.plot.summary(shap2)

#  Loop over dependence plots in decreasing importance
for (v in shap.importance(shap2, names_only = TRUE)) {
  p <- shap.plot.dependence(shap2, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}

#CONCLUSION?
#Too noisy, keep using aggregated data


### 5. XGBOOST FOR SHAP WITH 10-MIN DATA AND CATEGORICAL TYPE ATTRIBUTE ##############################


#Data
xgpreddata3 <- averagedmodellingdata[,-c(1, 11:15)]

#Define response and features
x3 <- colnames(xgpreddata3)[-8]
y3 <- "MeanPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix3 <- sample(nrow(xgpreddata3), (nrow(xgpreddata3)*0.8)) #80% training dataset

xgpreddata3 <- as.data.frame(xgpreddata3)
dtrain3 <- xgb.DMatrix(data.matrix(xgpreddata3[ix3, x3]),
                      label = xgpreddata3[ix3, y3])
dvalid3 <- xgb.DMatrix(data.matrix(xgpreddata3[-ix3, x3]),
                      label = xgpreddata3[-ix3, y3])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist3 = list(train=dtrain3, test=dvalid3)

fit_xgb3 <- xgb.train(
  params,
  data = dtrain3,
  watchlist = watchlist3,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)


X3 <- data.matrix(xgpreddata3[sample(nrow(xgpreddata3), 1000), x3])

shap3 <- shap.prep(fit_xgb3, X_train = X3)

# SHAP importance plot
shap.plot.summary(shap3)

# Loop over dependence plots in decreasing importance
for (v in shap.importance(shap3, names_only = TRUE)) {
  p <- shap.plot.dependence(shap3, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}


### 6. XGBOOST FOR SHAP WITH 10-MIN DATA AND BINARY VALUES FOR ONLY PASTURE AND REFERENCE ##############################


#Data
xgpreddata4 <- averagedmodellingdata[,-c(1:2, 11:12, 14, 16)]
colnames(xgpreddata4) <- c("Mean Annual Precipitation", "Enhanced Vegetation Index", "Elevation", "Human Footprint", "Average Canopy Height", "10-minute Timebin", "MeanPMN", "Average Background Noise", "Reference Pasture Site", "Reference Forest Site")

#Define response and features
x4 <- colnames(xgpreddata4)[-7]
y4 <- "MeanPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix4 <- sample(nrow(xgpreddata4), (nrow(xgpreddata4)*0.8)) #80% training dataset

xgpreddata4 <- as.data.frame(xgpreddata4)
dtrain4 <- xgb.DMatrix(data.matrix(xgpreddata4[ix4, x4]),
                      label = xgpreddata4[ix4, y4])
dvalid4 <- xgb.DMatrix(data.matrix(xgpreddata4[-ix4, x4]),
                      label = xgpreddata4[-ix4, y4])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist4 = list(train=dtrain4, test=dvalid4)

fit_xgb4 <- xgb.train(
  params,
  data = dtrain4,
  watchlist = watchlist4,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)

# Step 1: Select some observations
X4 <- data.matrix(xgpreddata4[sample(nrow(xgpreddata4), 1000), x4])

# Step 2: Crunch SHAP values
shap4 <- shap.prep(fit_xgb4, X_train = X4)

# Step 3: SHAP importance
shap.plot.summary(shap4)

# Step 4: Loop over dependence plots in decreasing importance
for (v in shap.importance(shap4, names_only = TRUE)) {
  p <- shap.plot.dependence(shap4, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}


### 7. XGBOOST FOR SHAP WITH 10-MIN DATA AND BINARY VALUES FOR ONLY PASTURE, REFERENCE AND PSA ##############################


#Data
xgpreddata5 <- averagedmodellingdata[,-c(1:2, 11:12, 14)]

#Define response and features
x5 <- colnames(xgpreddata5)[-7]
y5 <- "MeanPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix5 <- sample(nrow(xgpreddata5), (nrow(xgpreddata5)*0.8)) #80% training dataset

xgpreddata5 <- as.data.frame(xgpreddata5)
dtrain5 <- xgb.DMatrix(data.matrix(xgpreddata5[ix5, x5]),
                       label = xgpreddata5[ix5, y5])
dvalid5 <- xgb.DMatrix(data.matrix(xgpreddata5[-ix5, x5]),
                       label = xgpreddata5[-ix5, y5])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist5 = list(train=dtrain5, test=dvalid5)

fit_xgb5 <- xgb.train(
  params,
  data = dtrain5,
  watchlist = watchlist5,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)

# Step 1: Select some observations
X5 <- data.matrix(xgpreddata5[sample(nrow(xgpreddata5), 1000), x5])

# Step 2: Crunch SHAP values
shap5 <- shap.prep(fit_xgb5, X_train = X5)

# Step 3: SHAP importance
shap.plot.summary(shap5)

# Step 4: Loop over dependence plots in decreasing importance
for (v in shap.importance(shap5, names_only = TRUE)) {
  p <- shap.plot.dependence(shap5, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}
