# -----------------------------------------------------------------------

# ------------- TEMPLATE FOR MODEL BUILDING AND EXPORTING ELEMENTS ------

# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)

# --- Read data sets ---

my_data <- fread("workshop_challenge_template/model/store_data/available_data_labeled.csv", sep = ";") 
current_response_var <- "yvar"
my_data_trainIndex <- createDataPartition(my_data[[current_response_var]], p = .8, 
                                         list = FALSE, 
                                         times = 1)
current_dataset_train <- my_data[my_data_trainIndex[,1]]
current_dataset_test <- my_data[-my_data_trainIndex[,1]]
rm(my_data, my_data_trainIndex)


# - if needed for binary response

current_dataset_train[[current_response_var]] <- as.factor(current_dataset_train[[current_response_var]])
current_dataset_test[[current_response_var]] <- as.factor(current_dataset_test[[current_response_var]])

yvar <- current_dataset_train[[current_response_var]]
yvar_test <- current_dataset_test[[current_response_var]]


# ------------------- CARET MODELING WITH ENSEMBLE  -----------------------------------

# - Model formula 

regressors <- c("xvar1", "xvar2", "xvar3")              
my_formula <- as.formula(paste0(current_response_var, " ~ ",paste(regressors, collapse = " + ")))

# - Train model. Caret template

my_model1 <- caret::train(my_formula, current_dataset_train, method = "",
                         trControl = trainControl(
                         method = "cv", number = 10, verboseIter = TRUE),
                         tuneGrid=expand.grid()
                         )


# - Validate model

# ... 

# ------------------  ELEMENTS OF THE MODEL FOR A USER FRIENDLY APP IN CLASSIFICATION -------------

my_model1_final <- my_model1$finalModel


# - Fitted and Predicted

my_model1_fitted <-predict(my_model1,current_dataset_train, type = "raw")
my_model1_pred <-predict(my_model1,current_dataset_test, type = "raw")

# - Confusion matrices
confusion_train <- confusionMatrix(my_model1_fitted,yvar, positive = "1")
confusion_test <- confusionMatrix(my_model1_pred,yvar_test, positive = "1")

my_model1_summary_performance <- rbind(data.table(Type = "Training", 
                                                 N = nrow(current_dataset_train),
                                                 Accuracy = confusion_train$overall[1],
                                                 Sensitivity = confusion_train$byClass[1],
                                                 Specificity = confusion_train$byClass[2]),
                                      data.table(Type = "Test", 
                                                 N = nrow(current_dataset_test),
                                                 Accuracy = confusion_test$overall[1],
                                                 Sensitivity = confusion_test$byClass[1],
                                                 Specificity = confusion_test$byClass[2]))
           

# - Variable importance. This might vary depending on the method used in caret::train. Check documentation of the method 
# -  This is an example in the case there are 3 trees in the random forest

my_varimp_tree1 <- sort(varImp(my_model1_model$trees[[1]])$Overall, decreasing = T)
my_model1_varimp_tree1 <- data.table(Tree = "Tree 1", Variable = rownames(varImp(my_model1_model$trees[[1]])), Importance = my_varimp_tree1/my_varimp_tree1[1])

my_varimp_tree2 <- sort(varImp(my_model1_model$trees[[2]])$Overall, decreasing = T)
my_model1_varimp_tree2 <- data.table(Tree = "Tree 2", Variable = rownames(varImp(my_model1_model$trees[[2]])), Importance = my_varimp_tree2/my_varimp_tree2[1])

my_varimp_tree3 <- sort(varImp(my_model1_model$trees[[3]])$Overall, decreasing = T)
my_model1_varimp_tree3 <- data.table(Tree = "Tree 3", Variable = rownames(varImp(my_model1_model$trees[[3]])), Importance = my_varimp_tree3/my_varimp_tree3[1])

varimp_summary <- rbind(my_model1_varimp_tree1,my_model1_varimp_tree2,my_model1_varimp_tree3)


# --- ELEMENTS IN THE FINAL LIST TO GET INTO THE SHINY APP: train, summary performance, extra info (e.g. variable importance)

model_output <- list(trained_model = my_model1,
                     summary_performance = my_model1_summary_performance,
                     variable_importance = varimp_summary)

# --- SAVE MODEL ELEMENTS

save(model_output, file = "model/model_file.RData")


