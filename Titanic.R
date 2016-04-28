setwd('./')
seed = sample(1:1000, 1)
set.seed(seed)
require('randomForest')
require('caret')
require('mice')

prep.data <- function(data) {
  #clean the data
  drops = c(
    'PassengerId',
    'Ticket',
    'Cabin',
    'Parch',
    'SibSp',
    'Name'
  )

  # column transformations
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

  data <- data[,!(colnames(data) %in% drops)]

  # missing value munging
  data <- complete(mice(data,m=5,meth='pmm', seed = seed),1)

  # normalize
  data$Fare <- log10(data$Fare + 1)

  data
}

data.from.csv <- read.csv('train.csv', header = T)
data <- prep.data(data.from.csv)

sample <- sample(1:nrow(data), 0.7*nrow(data))
data.train <- data[sample,]
data.test <- data[-sample,]

model = randomForest(
  Survived ~ .,
  data = data.train,
  importance=TRUE
)

pdf("titanic.pdf")
varImpPlot(model)
dev.off()

predictions <- predict(model, data.test, type="response")

print(confusionMatrix(predictions, data.test$Survived, positive = "Died"))

precision <- posPredValue(predictions, data.test$Survived)
recall <- sensitivity(predictions, data.test$Survived)

F1 <- (2 * precision * recall) / (precision + recall)

writeLines("your F1 is...!\n")
print(F1)
