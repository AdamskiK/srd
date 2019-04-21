source("gini Function.R")
source("utils.R")


RpartModel <- function(full_dataset, train_data, cp) {
  
  # create a model
  tree_model <- rpart(DefFlag ~ ., 
                      data=train_data,
                      control=rpart.control(minsplit=2, 
                                            minbucket=1, 
                                            cp=cp),
                      parms = list(split="Gini"))
  
  # rpart.plot(tree_model)

  # create some predictions
  full_dataset$Score <- predict(tree_model, newdata=full_dataset)

  # check unique prediction values
  # print(unique(dataset$Score))

  # check importance of each variable
  # tree_model$variable.importance

  # check cross validation results
  # printcp(tree_model)

  df <- full_dataset %>% select(Application_ID, Score)
  print(head(df))
  write.csv(df, "output/output.csv", row.names=FALSE)
  
  result = Gini_value(full_dataset$Score,
                      full_dataset$DefFlag)
  print(paste(c("Gini result for model #1:", result), collapse = " "))
}


CaretModel <- function(data) {
  
  dataset <- data
  
  dataset$DefFlag <- as.factor(dataset$DefFlag)
  
  Grid <- expand.grid(cp=seq(0.1, 0.2, 0.1))
  
  rpart_model <- train(factor(DefFlag)~.,
                       data=dataset,
                       method='rpart',
                       metric = "Accuracy"
                       # tuneGrid = Grid,
                       # na.action = na.omit,
                       # parms=list(split='Gini')
                       )
  
  dataset$Score <- predict(rpart_model, dataset)
  
  print("Gini result for model 2")
  print(Gini_value(dataset$Score, dataset$DefFlag))
}