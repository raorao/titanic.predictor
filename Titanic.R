setwd('./')
seed = sample(1:1000, 1)
set.seed(seed)
require('caret')
require('mice')

prep.data <- function(data, hasSurvived = T) {
  #clean the data
  keeps = c(
    'PassengerId'
    ,'Survived'
    # ,'Ticket'
    # ,'Cabin'
    # ,'Name'
    # ,'Parch'
    # ,'SibSp'
    # ,'Sex'
    # ,'HasAge'
    ,'Fare'
    # ,'Embarked'
    # ,'FamilySize'
    ,'HasCabin'
    ,'Age'
    ,'Title'
    ,'Pclass'
  )

  # column transformations
  if (!hasSurvived) {
    data = data.frame(
      'Survived' = vector(mode="character", length=nrow(data)),
      data
    )
  }

  data$Survived <- as.factor(data$Survived)
  levels(data$Survived) <- c("Died", "Lived")

  data$Pclass <- as.ordered(data$Pclass)

  data[(data$Embarked == ''),'Embarked'] = NA

  # engineer features
  data$HasCabin <- (data$Cabin == "")
  data$FamilySize <- data$Parch + data$SibSp
  data$HasAge <- !is.na(data$Age)

  data$Title <- vector(mode="character", length=nrow(data))
  data[grepl('(Miss)', data$Name), 'Title'] <- 'Miss'
  data[grepl('(Mlle\\.)', data$Name), 'Title'] <- 'Miss'
  data[grepl('(Ms\\.)', data$Name), 'Title'] <- 'Miss'
  data[grepl('(Mrs\\.)', data$Name), 'Title'] <- 'Mrs.'
  data[grepl('(Mme\\.)', data$Name), 'Title'] <- 'Mrs.'
  data[grepl('(Mr\\.)', data$Name), 'Title'] <- 'Mr.'
  data[grepl('(Master)', data$Name), 'Title'] <- 'Master'
  # data[grepl('(Countess)', data$Name), 'Title'] <- 'Other (Female)'
  # data[grepl('(Lady\\.)', data$Name), 'Title'] <- 'Other (Female)'
  # data[grepl('(Dr\\.)', data$Name), 'Title'] <- 'Other (Male)'
  # data[grepl('(Rev\\.)', data$Name), 'Title'] <- 'Other (Male)'
  # data[grepl('(Major\\.)', data$Name), 'Title'] <- 'Other (Male)'
  # data[grepl('(Sir\\.)', data$Name), 'Title'] <- 'Other (Male)'
  # data[grepl('(Col\\.)', data$Name), 'Title'] <- 'Other (Male)'
  # data[grepl('(Capt\\.)', data$Name), 'Title'] <- 'Other (Male)'
  data[(data$Title == ''), 'Title'] <- 'Other'
  data$Title <- as.factor(data$Title)

  data$Embarked <- droplevels(data$Embarked)

  data <- data[,(colnames(data) %in% keeps)]

  # missing value munging
  data <- complete(mice(data,m=5,meth='pmm', seed = seed, printFlag = F),1)

  data
}

evaluate.model <- function(model, test.data) {
  predictions <- predict(model, prep.data(test.data, hasSurvived = F))
  print(confusionMatrix(model))
  print(model$results)
}

train.model <- function(train.data) {
  train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)

  train(
    Survived ~. -PassengerId,
    data = prep.data(train.data),
    trControl=train_control,
    method="ranger"
  )
}

sandbox <- function(data) {
  model <- train.model(data)
  evaluate.model(model, data)
}

production <- function(train.data, test.data) {
  data.test  <- prep.data(test.data, hasSurvived = F)
  model      <- train.model(train.data)

  evaluate.model(model, train.data)

  predictions <- predict(model, data.test)

  output.file <- as.data.frame(as.matrix(predictions))

  output.file$PassengerId <- test.data$PassengerId
  colnames(output.file) <- c("Survived", "PassengerId")
  levels(output.file$Survived) <- c(0, 1)

  writeLines('\n\noutput is ready at production.csv\n\n')
  write.csv(output.file, file = 'production.csv', row.names = F)
}

# run code
data <- read.csv('train.csv', header = T)
data.test <- read.csv('test.csv', header = T)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 1 && args[1] == "production") {
  production(data, data.test)
} else {
  sandbox(data)
}
