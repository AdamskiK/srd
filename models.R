source("gini Function.R")
source("utils.R")


RpartModel <- function(data, cp) {

  dataset <- data
  
  # create a model
  tree_model <- rpart(DefFlag ~ ., 
                      data=dataset,
                      control=rpart.control(minsplit=2, 
                                            minbucket=1, 
                                            cp=cp),
                      parms = list(split="Gini"))
  
  rpart.plot(tree_model)

  # create some predictions
  new <- data.frame(dataset)
  dataset$Score <- predict(tree_model, newdata=new)

  # check unique prediction values
  print(unique(dataset$Score))

  # check importance of each variable
  # tree_model$variable.importance

  # check cross validation results
  # printcp(tree_model)
  
  result = Gini_value(dataset$Score, dataset$DefFlag)
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