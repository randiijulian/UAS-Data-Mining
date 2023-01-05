```{r}
home_data = "D:/Uni Life's/Studies/Semester 7/Datmin/UAS/fire_archive_M-C61_318831.csv"
```

```{r}
# Load the data
fire_data <- read.csv(home_data, header = TRUE, sep = ",")
head(fire_data)
```

```{r}
max(fire_data$brightness)
```

```{r}
max(fire_data$bright_t31)
```

```{r}
max(fire_data$frp)
```

```{r}
max(fire_data$confidence)
```

```{r}
# print row with maximum value of brightness column
fire_data[fire_data$brightness == max(fire_data$brightness), ]
```

```{r}
# print row with maximum value of bright_t31 column
fire_data[fire_data$bright_t31 == max(fire_data$bright_t31), ]
```

```{r}
# print row with maximum value of frp column
fire_data[fire_data$frp == max(fire_data$frp), ]
```

```{r}
# print row with maximum value of confidence column
fire_data[fire_data$confidence == max(fire_data$confidence), ]
```

```{r}
# get maximum value of each column
apply(fire_data, 2, max)
```

```{r}
n <- 4
cols <- c('brightness', 'bright_t31', 'frp', 'confidence')
```

```{r}
data2 <- fire_data[cols]
head(data2)
```

```{r}
# PCA
pca <- prcomp(data2, scale = TRUE)
summary(pca)
```

```{r}
# correlation coefficient
cor(data2)
```

```{r}
install.packages('psych')
```

```{r}
library(psych)
```

```{r}
scree(data2)
```

```{r}
fa.parallel(data2)
```

```{r}
biplot(pca)
```

```{r}
# Split the data into training and test sets
set.seed(123)
split <- sample(1:nrow(data2), size = floor(0.7 * nrow(data2)))
train_data <- data2[split, ]
test_data <- data2[-split, ]

# Fit the model on the training data
model <- lm(frp ~ brightness, data = train_data)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Calculate the mean squared error
mse <- mean((predictions - test_data$frp)^2)
print(mse)
```

```{r}
install.packages('caret')
```

```{r}
library(caret)
```

```{r}
# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train the model using linear regression
model <- train(frp ~ brightness, data = data2, method = "lm", trControl = train_control)

# Print the results
print(model)
```

```{r}
# function to predict new data
predict_new_data <- function(brightness, frp, confidence) {
  new_data <- data.frame(brightness = brightness, frp = frp, confidence = confidence)
  predictions <- predict(model, newdata = new_data)
  print(predictions)
}
```

```{r}
# predict new data
predict_new_data(327.5, 39.2, 85)
predict_new_data(342.7, 40.1, 92)
predict_new_data(306.9, 6.2, 58)
predict_new_data(300, 40, 60)
```

```{r}

```
