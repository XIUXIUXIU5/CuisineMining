library(jsonlite)
library(ggplot2)
library(tm)
library(caret)
library(rpart)
library(rpart.plot)

# import data
train_data <- fromJSON("train.json", flatten = TRUE)
#test_data <- fromJSON("train.json", flatten = TRUE)

# getting to know the data
ggplot(data = train_data, aes(x = cuisine)) + geom_histogram() + labs(title = "Cuisines", x = "Cuisine", y = "Number of Recipes")

# creating corpus
train_ingredients <- Corpus(VectorSource(train_data$ingredients))
#test_ingredients <- Corpus(VectorSource(test_data$ingredients))

# preprocess the words
## lower letter case
train_ingredients <- tm_map(train_ingredients, content_transformer(tolower))
#test_ingredients <- tm_map(test_ingredients, content_transformer(tolower))

## eliminating extra whitespace
train_ingredients <- tm_map(train_ingredients, stripWhitespace)
#test_ingredients <- tm_map(test_ingredients, stripWhitespace)

## remove unnecessary symbol
removeSymbol <- function(x) gsub("[^[:alpha:][:space:][:digit:]'%-]*","",x)
#removeSymbol <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
train_ingredients <- tm_map(train_ingredients, content_transformer(removeSymbol))
#test_ingredients <- tm_map(test_ingredients, content_transformer(removeSymbol))
## reduce ingredients to stem words
train_ingredients <- tm_map(train_ingredients, stemDocument)
#test_ingredients <- tm_map(test_ingredients, stemDocument)

# create bag of words
train_ingredientsDTM <- DocumentTermMatrix(train_ingredients)

# remove sparse terms
sparse <- removeSparseTerms(train_ingredientsDTM, 0.99)

# create bag of words
#test_ingredientsDTM <- DocumentTermMatrix(test_ingredients, control = list(weighting = weightTf,dictionary=Terms(sparse)))


# convert back to data frame
train_ingredientsDTM <- as.data.frame(as.matrix(sparse))
train_ingredientsDTM$cuisine <- as.factor(train_data$cuisine)

# Create Model
# inTrain <- createDataPartition(y = ingredientsDTM$cuisine, p = 0.7, list = FALSE)
training <- train_ingredientsDTM
#testing <- test_ingredientsDTM

# CART
set.seed(9347)
cartModelFit <- rpart(cuisine ~ ., data = training, method = "class")
# Plot
#prp(cartModelFit)

cartPredict <- predict(cartModelFit, newdata = testing, type = "class")
cartPredict
cartCM <- confusionMatrix(cartPredict, testing$cuisine)
cartCM

