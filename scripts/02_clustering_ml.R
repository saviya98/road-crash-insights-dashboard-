library(readr)
library(dplyr)    
library(stringr)  
library(forcats)   
library(lubridate)
df <- read_csv("cleaned_accident_data.csv")
df1 <- df %>% 
  mutate(high_harm = severity %in% c("Fatal", "Severe"))

View(df1)

# Load necessary libraries
library(cvms)      # For confusion matrix plot
library(broom)     # For tidy()
library(tibble)    # For tibble()
library(caret)     # For confusionMatrix

df1 <- df1 %>% 
  mutate(
    precip     = weather_condition %in%
      c("Rain", "Freezing Rain/Drizzle",
        "Sleet/Hail", "Snow"),
  )

View(df1)
# Convert target variable to factor (high_harm)
df1$high_harm <- factor(df1$high_harm, levels = c(FALSE,TRUE), labels = c("No", "Yes"))

# Check distribution of the target variable
table(df1$high_harm)


harm <- df1[df1$high_harm == "Yes", ][1:10000, ]
no_harm <- df1[df1$high_harm == "No", ][1:10000, ]

# Combine the two groups into a balanced dataset
balanced_data <- rbind(harm, no_harm)
#balanced_data <- acc_data
# Check the distribution of the target variable in the balanced data
table(balanced_data$high_harm)

# Split the balanced data into training and testing sets (80-20 split)
set.seed(123)
train_idx <- sample(nrow(df1), 0.8 * nrow(df1))
train <- df1[train_idx, ]
test <- df1[-train_idx, ]

# Check the size of training and test sets
dim(train)
dim(test)

# Build logistic regression model on training data
logreg <- glm(high_harm ~ is_night + is_weekend + precip +
                peak_hour + num_units + intersection,data = train, family = "binomial")
summary(logreg)

# Predict probabilities on test set
pr <- predict(logreg, test, type = "response")

# Convert probabilities to classes with a cutoff of 0.5
pr.classes <- ifelse(pr > 0.5, "Yes", "No")
pr.classes <- factor(pr.classes, levels = c("No", "Yes"))

# Check the distribution of predicted classes
table(pr.classes)

# Confusion matrix between predicted and actual labels
cm <- confusionMatrix(pr.classes, test$high_harm)
print(cm)

# Plot confusion matrix
d_binomial <- tibble(target = test$high_harm, prediction = pr.classes)
basic_table <- table(d_binomial)
cfm <- tidy(basic_table)

plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")

#####Clustering#####
install.packages("forcats")
library(cluster)    
library(forcats)
library(tidyr)

harm <- df1[df1$high_harm == "Yes", ][1:9000, ]
no_harm <- df1[df1$high_harm == "No", ][1:9000, ]

balanced_data <- rbind(harm, no_harm)

#selecting variables to cluster
vars <- c("is_night", "precip", "is_weekend",          
          "hour",                      
          "intersection",                              
          "num_units",                                 
          "traffic_control_device", "crash_type") 

#getting complete cases
df_cl <- balanced_data %>% 
  select(all_of(vars)) %>% 
  drop_na() 

#converiting logical to factors
df_cl <- df_cl %>% 
  mutate(across(where(is.logical),
                ~ factor(.x, levels = c(FALSE, TRUE),
                         labels = c("No", "Yes"))))

df_cl <- df_cl %>% 
  mutate(across(where(is.character), as.factor))

df_cl <- df_cl %>% 
  mutate(across(where(is.numeric), scale))

df_cl <- df_cl %>%
  mutate(
    hour      = as.numeric(hour),      
    num_units = as.numeric(num_units)  
  )


sapply(df_cl, class)

gower_mat <- daisy(df_cl, metric = "gower") 
gower_mat
sil_width <- sapply(2:8, function(k){
  pam(gower_mat, k)$silinfo$avg.width
})

print(sil_width)

## Final clustering (PAM) 
pam4 <- pam(gower_mat, k = 4)
pam4$medoids                 
pam4$clustering[1:20]

## Silhouette object and plot 
sil <- silhouette(pam4)
plot(sil, col = 2:5, border = NA)

##Add cluster IDs back to the data 

df_clust <- df_cl %>% 
  mutate(cluster = factor(pam4$clustering))

df_clust
##Quick profile table 
df_clust %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.factor), ~round(mean(.x == "Yes")*100,1)),
            avg_hour = mean(hour),
            .groups = "drop")



#####Clasification models#####
library(dplyr)       
library(tidyr)       
library(readr)      

# modelling framework
library(caret)   
library(recipes)     

# model engines called by caret
library(ranger)      
library(kernlab)     
library(kknn)        
library(nnet)        
library(rpart) 
library(xgboost) 
library(pROC)       
library(doParallel)
library(caret)
library(naivebayes)

acc_data <- read_csv("cleaned_accident_data_1.csv")

acc_data <- acc_data %>% drop_na()

acc_data$high_harm <- factor(acc_data$high_harm, levels = c(FALSE,TRUE), labels = c("No", "Yes"))

acc_data <- acc_data %>%
  select(high_harm, is_night, precip, is_weekend,
         peak_hour, num_units, intersection,hour) %>%
  na.omit()

harm <- acc_data[acc_data$high_harm == "Yes", ][1:6000, ]
no_harm <- acc_data[acc_data$high_harm == "No", ][1:6000, ]

balanced_data <- rbind(harm, no_harm)

table(balanced_data$high_harm)

set.seed(123)

val_idx <- createDataPartition(balanced_data$high_harm, p = 0.8, list = FALSE)
train   <- balanced_data[val_idx, ]
test    <- balanced_data[-val_idx, ]

train <- train %>% drop_na()
test <- test %>% drop_na()

ctrl <- trainControl(
  method          = "repeatedcv",
  number          = 10, repeats = 3,
  summaryFunction = twoClassSummary,   
  classProbs      = TRUE,              
  sampling        = "smote",          
  savePredictions = TRUE
)
metric <- "ROC"

#Random Forest
fit.rf  <- train(high_harm ~ ., data = train,
                 method = "ranger", metric = metric,
                 trControl = ctrl, tuneLength = 10)
print(fit.rf)
# Support Vector Machine (SVM)
fit.svm <- train(high_harm ~ ., data=train, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=ctrl,tuneLength = 10)
print(fit.svm)
# Neural Networks
fit.nn <- train(high_harm ~ ., data=train, method="nnet", metric=metric, trace=FALSE, trControl=ctrl,tuneLength = 10)
print(fit.nn)
# K-Nearest Neighbors (KNN)
fit.knn <- train(high_harm ~ ., data=train, method="knn", metric=metric, preProc=c("center", "scale"), trControl=ctrl,tuneLength = 15)
print(fit.knn)
# Decision Trees (CART)
fit.cart <- train(high_harm ~ ., data=train, method="rpart", metric=metric, trControl=ctrl,tuneLength = 10)
print(fit.cart)

set.seed(42)

# XGBoost
grid_xgb <- expand.grid(
  nrounds            = c(150, 300),        
  max_depth          = c(3, 6, 9),
  eta                = c(0.05, 0.1),
  gamma              = 0,
  colsample_bytree   = 0.8,
  min_child_weight   = 1,
  subsample          = 0.8
)

fit.xgb <- train(high_harm ~ ., data=train, method = "xgbTree",
                 metric = metric, trControl = ctrl,tuneGrid    = grid_xgb,
                 verbose     = FALSE )
print(fit.xgb)
plot(fit.xgb)
#NaiveBayes
fit.nb <- train(high_harm ~ ., data=train, method = "naive_bayes",metric = metric, trControl = ctrl)
print(fit.nb)

#logistic
fit.lr <- train(high_harm ~ ., data = train, method = "glm", family = "binomial", metric = metric, trControl = ctrl)
print(fit.lr)

#glm
fit.glm = train(high_harm ~ ., data = train, method = "glmnet",
                metric = metric, trControl = ctrl,
                tuneLength = 10)
print(fit.glm)


models   <- resamples(list(RF  = fit.rf,
                           SVM = fit.svm, 
                           NNet = fit.nn,
                           KNN = fit.knn,
                           CART = fit.cart,
                           NB = fit.nb,
                           XGB = fit.xgb,
                           LR = fit.lr,
                           GLM = fit.glm))
summary(models, metric = "ROC")
dotplot(models, metric = "ROC")  

prob <- predict(fit.xgb, test, type = "prob")[, "Yes"]
roc_obj <- pROC::roc(test$high_harm, prob, levels = c("No", "Yes"))
auc(roc_obj)                          
plot(roc_obj, print.auc = TRUE)   

cut <- quantile(prob, 0.60)            
pred <- factor(ifelse(prob >= cut, "Yes", "No"), levels = c("No", "Yes"))
confusionMatrix(pred, test$high_harm, positive = "Yes")
