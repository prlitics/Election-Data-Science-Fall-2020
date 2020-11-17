library(caret)
library(tidymodels)
library(anesr)

data("timeseries_2016")
anes16 <- timeseries_2016
rm(timeseries_2016)

clean <- function(x){ifelse(x < 0, NA, x)}

anes16_clean <- anes16 %>%
  mutate(across(everything(), clean)) %>%
  
  # What features do we want?
  ## Age, Income, PID, Ideology, Gender, Education, Race, Religion
  
  select(PID = V161158x , Gender = V161342, Income = V161361x, Religion = V161265x,Contact = V162007,
         Education = V161270, Age = V161267, Ideology = V161126, Race = V161310x, Vote = V162031x) %>%
  mutate(across(c(PID, Gender, Income, Religion, Education, Ideology, Race, Contact, Vote),as.factor)) %>%
  drop_na()

anes16_clean$Vote <- plyr::revalue(anes16_clean$Vote, c("1" = "Vote", "0"="Not_Vote"))

# Split the data
set.seed(131313)
split1 <- initial_split(anes16_clean, prop = .7)
Train_Data <- training(split1)
Test_Data<-testing(split1)

# Feature Engineering

anes_recipe <- recipe(Vote ~ ., data = Train_Data) %>% 
  step_dummy(PID, Gender, Income, Religion, Education, Ideology, Race, Contact) %>%
  step_center(Age) %>%
  step_scale(Age) %>%
  prep(training = Train_Data)

# Split the data

baked_train <- bake(anes_recipe, new_data = Train_Data)
baked_test <- bake(anes_recipe, new_data = Test_Data)



set.seed(131313)


cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  classProbs = T,
  summaryFunction = twoClassSummary
)

# Create a hyperparameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1,15, by = 2))
)

# Fit knn model and perform grid search
knn_grid <- train(
  Vote ~ .,
  data = baked_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "ROC"
)

ggplot(knn_grid)

knn_grid$results
