library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(vtreat)       # one-hot encoding
bmw_pricing_challenge <- read_csv("bmw_pricing_challenge.csv")
bmw_pricing <- tibble(bmw_pricing_challenge)
#data pre-processing
#check for na, nan, or missing
na_count <- sum(is.na(bmw_pricing_challenge))
nan_count <- sum(is.nan(unlist(bmw_pricing_challenge)))
levels(factor(bmw_pricing$car_type))

#since BMW's models begin with a number that indicates the
#series (the higher the number, usually the more expensive),
#we mutate the data as such

bmw_pricing <- bmw_pricing %>%
  mutate(Series = str_extract(model_key, "^[0-9]"))

#since some cars do not start with a number
#e.g. X5, M3, Z4
#we replace the NA's with those missing models

no_series_num_idx <- is.na(bmw_pricing$Series)
bmw_pricing$Series[no_series_num_idx] <- bmw_pricing$model_key[no_series_num_idx]

#since there is a column with registration date and sold date,
#it is logical to calculate the time in between

#convert to char

bmw_pricing$registration_date <- as.character(bmw_pricing$registration_date)
bmw_pricing$sold_at <- as.character(bmw_pricing$sold_at)
bmw_pricing <- bmw_pricing %>%
  mutate(`Time Elapsed` = round(difftime(sold_at, registration_date, units = "days"), 2), sold_at = NULL, registration_date = NULL)

#check for weird shit
summary(bmw_pricing)

#remove row with -64 mileage (doesn't make sense)
bmw_pricing <- bmw_pricing[-which(bmw_pricing$mileage == min(bmw_pricing$mileage)), ]

#remove row with 0 engine power (doesn't make sense)
bmw_pricing <- bmw_pricing[-which(bmw_pricing$engine_power == min(bmw_pricing$engine_power)), ]


# specify factors
# variables to keep as numeric: power, mileage, time elapsed, price
# variables to turn into factors: series, car_type, paint_color, fuel
# since BMW cars model key are based on the series and the engine power, 
# we can disregard the model key as well as make

bmw_pricing <- bmw_pricing %>%
  transform(Series = as.factor(Series),
            car_type = as.factor(car_type),
            paint_color = as.factor(paint_color),
            fuel = as.factor(fuel),
            `Time Elapsed` = as.double(`Time Elapsed`),
            feature_1 = as.numeric(feature_1), 
            feature_2 = as.numeric(feature_2), 
            feature_3 = as.numeric(feature_3), 
            feature_4 = as.numeric(feature_4), 
            feature_5 = as.numeric(feature_5), 
            feature_6 = as.numeric(feature_6), 
            feature_7 = as.numeric(feature_7), 
            feature_8 = as.numeric(feature_8) 
            )

#one-hot encoding for fuel, car_type, series
coded_df <- designTreatmentsZ(bmw_pricing, c("fuel", "car_type", "Series", "paint_color"))
bmw_pricing <- tibble(bmw_pricing, prepare(coded_df, bmw_pricing))

write.csv(bmw_pricing,"./bmw_pricing.csv")
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(bmw_pricing), 0.7 * nrow(bmw_pricing), replace = FALSE)
train_data <- bmw_pricing[train, ]
test_data <- bmw_pricing[-train, ]

model1 <- randomForest(price ~ Series +
                         engine_power +
                         Time.Elapsed +
                         mileage +
                         fuel_lev_x_diesel +
                         fuel_lev_x_electro +
                         fuel_lev_x_hybrid_petrol +
                         fuel_lev_x_petrol +
                         car_type_lev_x_convertible +
                         car_type_lev_x_coupe +
                         car_type_lev_x_estate +
                         car_type_lev_x_hatchback +
                         car_type_lev_x_sedan +
                         car_type_lev_x_subcompact +
                         car_type_lev_x_suv +
                         car_type_lev_x_van +
                         Series_lev_x_1 +
                         Series_lev_x_2 +
                         Series_lev_x_3 +
                         Series_lev_x_4 +
                         Series_lev_x_5 +
                         Series_lev_x_6 +
                         Series_lev_x_7 +
                         Series_lev_x_ActiveHybrid_5 +
                         Series_lev_x_i3 +
                         Series_lev_x_i8 +
                         Series_lev_x_M135 +
                         Series_lev_x_M235 +
                         Series_lev_x_M3 +
                         Series_lev_x_M4 +
                         Series_lev_x_M5 +
                         Series_lev_x_M550 +
                         Series_lev_x_X1 +
                         Series_lev_x_X3 +
                         Series_lev_x_X4 +
                         Series_lev_x_X5 +
                         Series_lev_x_X5_M +
                         Series_lev_x_X5_M50 +
                         Series_lev_x_X6 +
                         Series_lev_x_X6_M +
                         Series_lev_x_Z4 +
                         paint_color_lev_x_black +
                         paint_color_lev_x_beige +
                         paint_color_lev_x_blue +
                         paint_color_lev_x_brown +
                         paint_color_lev_x_green +
                         paint_color_lev_x_grey +
                         paint_color_lev_x_orange +
                         paint_color_lev_x_red +
                         paint_color_lev_x_silver +
                         paint_color_lev_x_white +
                         feature_1 +
                         feature_2 +
                         feature_3 +
                         feature_4 +
                         feature_5 +
                         feature_6 +
                         feature_7 +
                         feature_8 , data = train_data, importance = TRUE)
model1
plot(model1)
predval <- predict(model1, test_data, type = "class")
plot((predval - test_data$price)) #error
mean((predval - test_data$price)^2) #mse




