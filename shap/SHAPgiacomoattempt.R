
#Trying XGBoost Modelling
library(pacman)
p_load(xgboost) #the main algorithm
p_load(dplyr)

ModellingData <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/ModellingDataForFigure2oct2023.csv") # Correct path to correct file


round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

agg_data <- ModellingData %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  dplyr::group_by(Type, nearest_10) %>%
  mutate(MeanPMN = mean(SummedPMN), MeanNoise = mean(SummedNoise)) %>%
  ungroup() 

averagedmodellingdata <- agg_data %>% subset(select = -c(Minute, TimeStamp, SummedPMN, SummedNoise, MicType, Latitude, Longitude)) %>% unique()
quartiles <- quantile(averagedmodellingdata$MeanPMN, probs = c(0.2, 0.4, 0.6, 0.8))

averagedmodellingdata <- averagedmodellingdata %>%
  mutate(Level = case_when(
    MeanPMN <= quartiles[1] ~ "Low",
    MeanPMN <= quartiles[2] ~ "Medium Low",
    MeanPMN <= quartiles[3] ~ "Medium",
    MeanPMN <= quartiles[4] ~ "Medium High", 
    TRUE ~ "High"
  ))

averagedmodellingdata$Plantationtype <- ifelse(grepl("Plantation", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$Pasturetype <- ifelse(grepl("Pasture", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$NatRegentype <- ifelse(grepl("Natural_Regeneration", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)
averagedmodellingdata$Reftype <- ifelse(grepl("Reference_Forest", averagedmodellingdata$Type, ignore.case = TRUE), TRUE, FALSE)

#Data
xgpreddata <- averagedmodellingdata[,-c(1, 2, 11)]

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



#SHAP
p_load(farff)
p_load(OpenML)
p_load(dplyr)
p_load(ggplot2)
p_load(SHAPforxgboost)


# Step 1: Select some observations
X <- data.matrix(xgpreddata[sample(nrow(xgpreddata), 1000), x])

# Step 2: Crunch SHAP values
shap <- shap.prep(fit_xgb, X_train = X)

# Step 3: SHAP importance
shap.plot.summary(shap)

# Step 4: Loop over dependence plots in decreasing importance
for (v in shap.importance(shap, names_only = TRUE)) {
  p <- shap.plot.dependence(shap, v, color_feature = "auto", 
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}
