setwd('./')
seed = sample(1:1000, 1)
set.seed(seed)
require('randomForest')
require('caret')
require('mice')

prep.data <- function(data, hasSurvived = T) {
  #clean the data
  drops = c(
    'Ticket',
    'Cabin',
    'Parch',
    'SibSp',
    'Name',
    'Sex',
    # 'HasAge',
    'Embarked',
    'Fare',
    # 'FamilySize',
    # 'HasCabin',
    # 'Age',
    # 'Title'
    'Pclass'
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

  data <- data[,!(colnames(data) %in% drops)]

  # missing value munging
  data <- complete(mice(data,m=5,meth='pmm', seed = seed),1)

  data
}

evaluate.model <- function(model, test.data) {
  pdf("titanic.pdf")
  varImpPlot(model)
  dev.off()

  predictions <- predict(model, test.data, type="response")

  print(confusionMatrix(predictions, test.data$Survived, positive = "Died"))

  precision <- posPredValue(predictions, test.data$Survived)
  recall <- sensitivity(predictions, test.data$Survived)

  F1 <- (2 * precision * recall) / (precision + recall)

  writeLines("your F1 is...!\n")
  print(F1)
}

train.model <- function(train.data) {
  randomForest(
    Survived ~ . -PassengerId,
    data = train.data,
    importance=TRUE,
    mtry = 2,
    nodesize = 10,
    ntree = 4000
  )
}

sandbox <- function(data) {
  sample <- sample(1:nrow(data), 0.7*nrow(data))
  data.train <- prep.data(data[sample,])
  data.test  <- prep.data(data[-sample,])

  model <- train.model(data.train)
  evaluate.model(model, data.test)
}

production <- function(train.data, test.data) {
  data.train <- prep.data(train.data)
  data.test  <- prep.data(test.data, hasSurvived = F)

  model <- train.model(data.train)

  prediction <- predict(model, data.test, type="response")

  output.file <- as.data.frame(as.matrix(prediction))

  output.file$PassengerId <- test.data$PassengerId
  colnames(output.file) <- c("Survived", "PassengerId")
  levels(output.file$Survived) <- c(0, 1)

  write.csv(output.file, file = 'production.csv', row.names = F)
}

# run code
data <- read.csv('train.csv', header = T)
data.test <- read.csv('test.csv', header = T)

# sandbox(data)
production(data, data.test)
