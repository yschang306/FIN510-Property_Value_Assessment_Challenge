### Full run time: 37.18188 mins

# Measuring run time
start.time <- Sys.time()

# Remove the scientific notation
options(scipen=999)

# 0. Load the packages
library(caret)
library(tidyverse)
library(glmnet)
library(randomForest)
library(rpart)
library(rpart.plot)

# 1. Data pre-processing

# 1.1 Load the data
# Load historic_property_data.csv
historic.df <- read.csv("/Users/liyuanrong/Desktop/FIN510_Final Project/data/historic_property_data.csv")
head(historic.df)
dim(historic.df) # 63 variables
str(historic.df)

# Load predict_property_data.csv
predict.df <- read.csv("/Users/liyuanrong/Desktop/FIN510_Final Project/data/predict_property_data.csv")
head(predict.df)
dim(predict.df)
str(predict.df)

# 1.2 Check the number of NA for each variable
# "char_apts","char_attic_fnsh","char_renovation", "char_porch" 
# These variables include NA more than half of the observation, 5,000
for (i in 1:ncol(historic.df)) {
  print(names(historic.df)[i])
  print(sum(is.na(historic.df[i])))
}

for (i in 1:ncol(predict.df)) {
  print(names(predict.df)[i])
  print(sum(is.na(predict.df[i])))
}

# 1.3 Drop variable char_tp_dsgn, char_cnst_qlty,char_site,char_repair_cnd,meta_cdu,char_apts,char_attic_fnsh,char_renovation,char_porch, meta_nbhd, geo_property_zip, geo_fips, geo_municipality

# "char_tp_dsgn" This field name comes from a variable that is no longer used, but the field name was not changed to reflect Cathedral Ceiling
# "char_cnst_qlty" In general, this field is not used consistently and is therefore not useful for analytical purposes
# "char_site" This field lacks sufficient variation to be useful for modeling.
# "char_repair_cnd" This field is subjective and contains little variation. As such, it is not used for modeling.
# "meta_cdu" Dozens of codes attached to face sheet that denote a number of seemingly unrelated characteristics associated with a PIN, ranging from condition to types of subsidies. This field does not match across the SQL server/AS-400.
# "meta_nbhd", "geo_property_zip", "geo_fips", "geo_municipality" represent less important information
historic.df <- subset( historic.df, select= -c(char_tp_dsgn, char_cnst_qlty,char_site,char_repair_cnd,meta_cdu,char_apts,char_attic_fnsh,char_renovation,char_porch, meta_nbhd, geo_property_zip, geo_fips, geo_municipality))
predict.df <- subset( predict.df, select= -c(char_tp_dsgn, char_cnst_qlty,char_site,char_repair_cnd,meta_cdu,char_apts,char_attic_fnsh,char_renovation,char_porch, meta_nbhd, geo_property_zip, geo_fips, geo_municipality))

# After dropping 13 variables, there are 50 variables left
dim(historic.df)
dim(predict.df)

# 1.4 Replace NA's in historic.df
# 1.4.1 Replace NA's with mode (Character variable)
# Create a function calc_mode to calculate the mode
calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

# 1.4.2 start the replacement
historic.df <- historic.df %>% 
  mutate(char_ext_wall = if_else(is.na(char_ext_wall), 
                                 calc_mode(char_ext_wall), 
                                 char_ext_wall))

historic.df <- historic.df %>% 
  mutate(char_roof_cnst = if_else(is.na(char_roof_cnst), 
                                  calc_mode(char_roof_cnst), 
                                  char_roof_cnst))

historic.df <- historic.df %>% 
  mutate(char_bsmt = if_else(is.na(char_bsmt), 
                             calc_mode(char_bsmt), 
                             char_bsmt))

historic.df <- historic.df %>% 
  mutate(char_bsmt_fin = if_else(is.na(char_bsmt_fin), 
                                 calc_mode(char_bsmt_fin), 
                                 char_bsmt_fin))

historic.df <- historic.df %>% 
  mutate(char_heat = if_else(is.na(char_heat), 
                             calc_mode(char_heat), 
                             char_heat))

historic.df <- historic.df %>% 
  mutate(char_oheat = if_else(is.na(char_oheat), 
                              calc_mode(char_oheat), 
                              char_oheat))

historic.df <- historic.df %>% 
  mutate(char_air = if_else(is.na(char_air), 
                            calc_mode(char_air), 
                            char_air))

historic.df <- historic.df %>% 
  mutate(char_attic_type = if_else(is.na(char_attic_type), 
                                   calc_mode(char_attic_type), 
                                   char_attic_type))

historic.df <- historic.df %>% 
  mutate(char_tp_plan = if_else(is.na(char_tp_plan), 
                                calc_mode(char_tp_plan), 
                                char_tp_plan))

historic.df <- historic.df %>% 
  mutate(char_gar1_size = if_else(is.na(char_gar1_size), 
                                  calc_mode(char_gar1_size), 
                                  char_gar1_size))

historic.df <- historic.df %>% 
  mutate(char_gar1_cnst = if_else(is.na(char_gar1_cnst), 
                                  calc_mode(char_gar1_cnst), 
                                  char_gar1_cnst))

historic.df <- historic.df %>% 
  mutate(char_gar1_att = if_else(is.na(char_gar1_att), 
                                 calc_mode(char_gar1_att), 
                                 char_gar1_att))

historic.df <- historic.df %>% 
  mutate(char_gar1_area = if_else(is.na(char_gar1_area), 
                                  calc_mode(char_gar1_area), 
                                  char_gar1_area))

historic.df <- historic.df %>% 
  mutate(char_use = if_else(is.na(char_use), 
                            calc_mode(char_use), 
                            char_use))

historic.df <- historic.df %>% 
  mutate(char_type_resd = if_else(is.na(char_type_resd), 
                                  calc_mode(char_type_resd), 
                                  char_type_resd))

historic.df <- historic.df %>% 
  mutate(geo_property_city = if_else(is.na(geo_property_city), 
                                     calc_mode(geo_property_city), 
                                     geo_property_city))


historic.df <- historic.df %>% 
  mutate(geo_ohare_noise = if_else(is.na(geo_ohare_noise), 
                                   calc_mode(geo_ohare_noise), 
                                   geo_ohare_noise))

historic.df <- historic.df %>% 
  mutate(geo_floodplain = if_else(is.na(geo_floodplain), 
                                  calc_mode(geo_floodplain), 
                                  geo_floodplain))

historic.df <- historic.df %>% 
  mutate(geo_withinmr100 = if_else(is.na(geo_withinmr100), 
                                   calc_mode(geo_withinmr100), 
                                   geo_withinmr100))

historic.df <- historic.df %>% 
  mutate(geo_withinmr101300 = if_else(is.na(geo_withinmr101300), 
                                      calc_mode(geo_withinmr101300), 
                                      geo_withinmr101300))


historic.df <- historic.df %>% 
  mutate(geo_school_elem_district = if_else(is.na(geo_school_elem_district), 
                                            calc_mode(geo_school_elem_district), 
                                            geo_school_elem_district))


historic.df <- historic.df %>% 
  mutate(geo_school_hs_district = if_else(is.na(geo_school_hs_district), 
                                          calc_mode(geo_school_hs_district), 
                                          geo_school_hs_district))

historic.df <- historic.df %>% 
  mutate(ind_garage = if_else(is.na(ind_garage), 
                              calc_mode(ind_garage), 
                              ind_garage))

# 1.4.3 Replace NA's with mean (Numeric variable)
historic.df <-historic.df  %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

# 1.4.4 Check the number of NA's
sum(is.na(historic.df)) 

# 1.5 Replace NA's in predict.df
# 1.5.1 Replace NA's with mode (Character variable)
# Create a function calc_mode to calculate the mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

# 1.5.2 start the replacement
predict.df <- predict.df %>% 
  mutate(char_ext_wall = if_else(is.na(char_ext_wall), 
                                 calc_mode(char_ext_wall), 
                                 char_ext_wall))

predict.df <- predict.df %>% 
  mutate(char_roof_cnst = if_else(is.na(char_roof_cnst), 
                                  calc_mode(char_roof_cnst), 
                                  char_roof_cnst))

predict.df <- predict.df %>% 
  mutate(char_bsmt = if_else(is.na(char_bsmt), 
                             calc_mode(char_bsmt), 
                             char_bsmt))

predict.df <- predict.df %>% 
  mutate(char_bsmt_fin = if_else(is.na(char_bsmt_fin), 
                                 calc_mode(char_bsmt_fin), 
                                 char_bsmt_fin))

predict.df <- predict.df %>% 
  mutate(char_heat = if_else(is.na(char_heat), 
                             calc_mode(char_heat), 
                             char_heat))

predict.df <- predict.df %>% 
  mutate(char_oheat = if_else(is.na(char_oheat), 
                              calc_mode(char_oheat), 
                              char_oheat))

predict.df <- predict.df %>% 
  mutate(char_air = if_else(is.na(char_air), 
                            calc_mode(char_air), 
                            char_air))

predict.df <- predict.df %>% 
  mutate(char_attic_type = if_else(is.na(char_attic_type), 
                                   calc_mode(char_attic_type), 
                                   char_attic_type))

predict.df <- predict.df %>% 
  mutate(char_tp_plan = if_else(is.na(char_tp_plan), 
                                calc_mode(char_tp_plan), 
                                char_tp_plan))

predict.df <- predict.df %>% 
  mutate(char_gar1_size = if_else(is.na(char_gar1_size), 
                                  calc_mode(char_gar1_size), 
                                  char_gar1_size))

predict.df <- predict.df %>% 
  mutate(char_gar1_cnst = if_else(is.na(char_gar1_cnst), 
                                  calc_mode(char_gar1_cnst), 
                                  char_gar1_cnst))

predict.df <- predict.df %>% 
  mutate(char_gar1_att = if_else(is.na(char_gar1_att), 
                                 calc_mode(char_gar1_att), 
                                 char_gar1_att))

predict.df <- predict.df %>% 
  mutate(char_gar1_area = if_else(is.na(char_gar1_area), 
                                  calc_mode(char_gar1_area), 
                                  char_gar1_area))

predict.df <- predict.df %>% 
  mutate(char_use = if_else(is.na(char_use), 
                            calc_mode(char_use), 
                            char_use))

predict.df <- predict.df %>% 
  mutate(char_type_resd = if_else(is.na(char_type_resd), 
                                  calc_mode(char_type_resd), 
                                  char_type_resd))

predict.df <- predict.df %>% 
  mutate(geo_property_city = if_else(is.na(geo_property_city), 
                                     calc_mode(geo_property_city), 
                                     geo_property_city))


predict.df <- predict.df %>% 
  mutate(geo_ohare_noise = if_else(is.na(geo_ohare_noise), 
                                   calc_mode(geo_ohare_noise), 
                                   geo_ohare_noise))

predict.df <- predict.df %>% 
  mutate(geo_floodplain = if_else(is.na(geo_floodplain), 
                                  calc_mode(geo_floodplain), 
                                  geo_floodplain))

predict.df <- predict.df %>% 
  mutate(geo_withinmr100 = if_else(is.na(geo_withinmr100), 
                                   calc_mode(geo_withinmr100), 
                                   geo_withinmr100))

predict.df <- predict.df %>% 
  mutate(geo_withinmr101300 = if_else(is.na(geo_withinmr101300), 
                                      calc_mode(geo_withinmr101300), 
                                      geo_withinmr101300))


predict.df <- predict.df %>% 
  mutate(geo_school_elem_district = if_else(is.na(geo_school_elem_district), 
                                            calc_mode(geo_school_elem_district), 
                                            geo_school_elem_district))


predict.df <- predict.df %>% 
  mutate(geo_school_hs_district = if_else(is.na(geo_school_hs_district), 
                                          calc_mode(geo_school_hs_district), 
                                          geo_school_hs_district))


predict.df <- predict.df %>% 
  mutate(ind_garage = if_else(is.na(ind_garage), 
                              calc_mode(ind_garage), 
                              ind_garage))

# 1.5.3 Replace NA's with mean (Numeric variable)
predict.df <-predict.df  %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

# 1.5.4 Check the number of NA's
sum(is.na(predict.df))


# 1.6 Extract data that should be covert to categorical variables or logical variables
# Numeric variables 
numeric.variables <- c("meta_certified_est_bldg", "meta_certified_est_land", "char_hd_sf", "char_age",
                       "char_rooms", "char_beds", "char_frpl", "char_fbath", "char_hbath", "char_ot_impr", 
                       "char_bldg_sf", "geo_tract_pop", "geo_white_perc", "geo_black_perc", "geo_asian_perc", 
                       "geo_his_perc", "geo_other_perc", "geo_fs_flood_factor", "geo_fs_flood_risk_direction", 
                       "econ_tax_rate", "econ_midincome", "sale_price")
# Logical variables
logical.variables <- c("geo_ohare_noise", "geo_floodplain", "geo_withinmr100", "geo_withinmr101300", 
                       "ind_large_home", "ind_garage", "ind_arms_length")
# Character and Categorical variables
index <- which(names(historic.df) %in% c(numeric.variables, logical.variables))
categorical.variables <- names(historic.df)[-index]

# 1.7 Convert the data
# 1.7.1 Historic data
# Redefined levels that exceed 50 levels to around 50 levels
# "geo_property_city ", "geo_school_elem_district", "geo_school_hs_district"
# Frequency lower than 250 -> Other
freq <- table(historic.df$geo_property_city)
levels <- names(freq)[freq < 250]
historic.df$geo_property_city <- replace(historic.df$geo_property_city, historic.df$geo_property_city %in% levels, "Other")

# Frequency lower than 250 -> Other
freq1 <- table(historic.df$geo_school_elem_district)
levels1 <- names(freq1)[freq1 < 250]
historic.df$geo_school_elem_district <- replace(historic.df$geo_school_elem_district, historic.df$geo_school_elem_district %in% levels1, "Other")

# Frequency lower than 350 -> Other
freq2 <- table(historic.df$geo_school_hs_district)
levels2 <- names(freq2)[freq2 < 350]
historic.df$geo_school_hs_district <- replace(historic.df$geo_school_hs_district, historic.df$geo_school_hs_district %in% levels2, "Other")

# Covert to categorical variables
# as.factor() coerces its argument to a factor
for (i in 1:length(categorical.variables)){
  v <- categorical.variables[i]
  j <- which(v == names(historic.df))
  factors <- as.factor(historic.df[, j])
  historic.df[, j] <- factors
}

# Convert to logical variables
for (i in 1:length(logical.variables)){
  v <- logical.variables[i]
  j <- which(v == names(historic.df))
  factors <- as.logical(historic.df[, j])
  historic.df[, j] <- factors
}

str(historic.df)

# 1.7.2 Predict data
# Redefined levels that exceed 50 levels with the same standard as historic.df
# "geo_property_city ", "geo_school_elem_district", "geo_school_hs_district"
levels3 <- names(table(historic.df$geo_property_city))
predict.df$geo_property_city <- replace(predict.df$geo_property_city, !predict.df$geo_property_city %in% levels3, "Other")
levels4 <- names(table(historic.df$geo_school_elem_district))
predict.df$geo_school_elem_district <- replace(predict.df$geo_school_elem_district, !predict.df$geo_school_elem_district %in% levels4, "Other")
levels5 <- names(table(historic.df$geo_school_hs_district))
predict.df$geo_school_hs_district <- replace(predict.df$geo_school_hs_district, !predict.df$geo_school_hs_district %in% levels5, "Other")

# Covert to categorical variables
# as.factor() coerces its argument to a factor
for (i in 1:length(categorical.variables)){
  v <- categorical.variables[i]
  j <- which(v == names(predict.df))
  factors <- as.factor(predict.df[, j])
  predict.df[, j] <- factors
}

# Convert to logical variables
for (i in 1:length(logical.variables)){
  v <- logical.variables[i]
  j <- which(v == names(predict.df))
  factors <- as.logical(predict.df[, j])
  predict.df[, j] <- factors
}

str(predict.df)

# 2. Data partition  
set.seed(1)
train.index <- sample(c(1:dim(historic.df)[1]), dim(historic.df)[1]*0.6)
train.df <- historic.df[train.index,]
test.df <- historic.df[-train.index,]

# 3. Linear regression
# Run a linear regression of Price with all predictors on the training set
lm.full <- lm(sale_price ~., data = train.df)
summary(lm.full)
# Run a linear regression of Price with no predictor on the training set
lm.null <- lm(sale_price~1, data = train.df)
# Use predict() to make predictions on the test set 
lm.pred <- predict(lm.full, test.df)
head(lm.pred)
# MSE in the test set 
mean((test.df$sale_price-lm.pred)^2) #15260334101


# 3.2 Step wise regression
lm.step.both <- step(lm.full, direction = "both")
summary(lm.step.both) 
# Use predict() to make predictions on the test set 
lm.step.pred.both <- predict(lm.step.both, test.df)
head(lm.step.pred.both)
# MSE in the test set
mean((test.df$sale_price - lm.step.pred.both)^2)  #15274230425


# 3.3 Lasso Regression
# Convert a data frame of predictors to a matrix and create dummy variables for character variables 
x <- model.matrix(sale_price ~ ., historic.df)[, -1]
y <- historic.df$sale_price
# Fit a lasso regression model to do 5-fold cross validation 
set.seed(1) # set seed
cv.fit <- cv.glmnet(x[train.index, ], y[train.index], alpha = 1, type.measure = "mse", nfold=5)
# Plot the cross-validated MSE for each lambda 
plot(cv.fit)
# Find the lambda that corresponds to the lowest cross-validated MSE 
lambda.best <- cv.fit$lambda.min
lambda.best
# Lasso regression coefficients  
coef.lambda.best <- predict(cv.fit, s = lambda.best, type = "coefficients")[1:20, ]
coef.lambda.best
# Non-zero coefficients 
coef.lambda.best[coef.lambda.best != 0]
# Make predictions for records in the test set 
pred.lambda.best <- predict(cv.fit, s = lambda.best, newx = x[-train.index, ])
head(pred.lambda.best)
# MSE in the test set
y.test <- y[-train.index]
mean((y.test - pred.lambda.best)^2) #15306767565


# 3.4 Random Forest
rf <- randomForest(sale_price ~ ., data = train.df, mtry = 8)
# Variable importance plot
varImpPlot(rf)
# Predicted prices for records in the test set 
rf.pred <- predict(rf, test.df, type = "response")
# MSE in the test set
y.test <- y[-train.index]
mean((y.test - rf.pred)^2) #13267269659

# 3.5 Regression Tree
cv.rt <- rpart(sale_price ~ ., data = train.df, method = "anova", cp = 0.001, xval = 5)
# Display the cp table
cv.rt$cptable
# Prune the tree
rt.pruned <- prune(cv.rt, cp = cv.rt$cptable[which.min(cv.rt$cptable[, "xerror"]), "CP"])
# Plot the tree
prp(rt.pruned, type = 1, extra = 1)
# Predicted prices for records in the test set 
rt.pruned.pred <- predict(rt.pruned, test.df, type = "vector")
head(rt.pruned.pred)
# MSE in the test set
mean((test.df$sale_price - rt.pruned.pred)^2) # 21482553307

# 4 Make predictions
# Choose the model with the lowest MSE, which is random forest
rf.pred.df <- predict(rf, predict.df, type = "response")
# Check NA's
sum(is.na(rf.pred.df))
# Covert named vectors to data frame
pred.df <- data.frame(pid=names(rf.pred.df), assessed_value=rf.pred.df, row.names=NULL)
head(pred.df)
names(pred.df)
# Export data frame to csv named Assessment File
write.csv(pred.df,"/Users/liyuanrong/Desktop/FIN510_Final Project/data/assessed_value.csv", row.names = FALSE)

# 5 Summary statistics of the prediction
summary(pred.df)

# Measuring run time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

