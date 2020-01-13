# Kaggle project 

# Linear regression example

## You are provided hourly rental data spanning two years. For this competition, 
## the training set is comprised of the first 19 days of each month, while the test set is 
## the 20th to the end of the month. 
## You must predict the total count of bikes rented during each hour covered by the test set, 
## using only information available prior to the rental period.

# Get data
bike <- read.csv("bikeshare.csv")
head(bike)

# Exploratory Data Analysis
ggplot(bike, aes(x = temp, y = count)) + geom_point(aes(color = temp), alpha = 0.5)

# convert datetime column into POISXct
bike$datetime <-  as.POSIXct(bike$datetime)
ggplot(bike, aes(x = datetime, y = count)) + geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_continuous(low="#55D8CE",high="#FF6E2E")

# Correction between temp and count
cor_col <- data.frame(bike$temp, bike$count)
cor_bike <- cor(cor_col)
cor_bike

# Create a boxplot with the y axis indicating count and the x axis begin a box for each season.
ggplot(bike, aes(x = factor(season), y = count)) + geom_boxplot(aes(color = factor(season)))

# Create an "hour" column that takes the hour from the datetime column. 
time_stamp <- bike$datetime
time_stamp <- format(time_stamp, "%H")
bike$hour <- time_stamp
head(bike)

# scatterplot of count versus hour, with color scale based on temp
bike_working <- subset(bike, bike$workingday == 1)
ggplot(bike_working, aes(x = hour, y = count)) + geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red", "purple"))

# for non working days
bike_notworking <- subset(bike, bike$workingday == 0)
ggplot(bike_notworking, aes(x = hour, y = count)) + geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red", "purple"))

# Build the model
temp_model <- lm(count ~ temp, data = bike)
summary(temp_model)

# predict the count when temperature is at 25 degree C is 
temp_test <- data.frame(temp = 25)
model_predict <- predict(temp_model, temp_test)

# change the hour column into numeric
bike$hour <- sapply(bike$hour, as.numeric)

head(bike)
final_bike <- select(bike, -datetime, -atemp, -casual, -registered)
head(final_bike)
final_bike$hour <-  as.factor(final_bike$hour)

# Final model build
final_model <- lm(count ~ ., data = final_bike)
summary(final_model)


