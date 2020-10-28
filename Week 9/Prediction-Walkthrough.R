install.packages("caret")
install.packages("tidymodels")

library(caret)
library(tidymodels)
library(anesr)

data("timeseries_2016")
anes16 <- timeseries_2016
rm(timeseries_2016)

clean <- function(x){ifelse(x < 1, NA, x)}

anes16_clean <- anes16 %>%
  mutate(across(everything(), clean)) %>%

# What features do we want?
## Age, Income, PID, Ideology, Gender, Education, Race, Religion

select(PID = V161158x , Gender = V161342, Income = V161361x, Religion = V161265x,
       Education = V161270, Age = V161267, Ideology = V161126, Race = V161310x, Trump_Fav = V161087) %>%
  mutate(across(c(PID, Gender, Income, Religion, Education, Ideology, Race),as.factor)) %>%
  drop_na()


# Split the data
set.seed(131313)
split1 <- initial_split(anes16_clean, prop = .7)
Train_Data <- training(split1)
Test_Data<-testing(split1)





# Feature Engineering

anes_recipe <- recipe(Trump_Fav ~ ., data = Train_Data) %>% 
  step_dummy(PID, Gender, Income, Religion, Education, Ideology, Race) %>%
  step_center(Age, Trump_Fav) %>%
  step_scale(Age, Trump_Fav) %>%
  prep(training = Train_Data)
  


# Split the data


baked_train <- bake(anes_recipe, new_data = Train_Data)
baked_test <- bake(anes_recipe, new_data = Test_Data)



set.seed(131313)
trump_lm <- train(
  Trump_Fav ~ ., 
  data = Train_Data, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)


trump_lm[["results"]]















