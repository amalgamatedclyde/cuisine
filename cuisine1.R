---
title: "Cuisine"
author: "Clyde Tressler"
date: "November 24, 2015"
output: html_document
---

```{r create_matrix, echo=TRUE}
#create train/test set
setwd("~/cuisine")
  require("rjson")
  cookbook <- fromJSON(file = 'train.json')
  set.seed(314159)
  include <- sample(c(TRUE, FALSE), length(cookbook), prob = c(.8,.2), replace=T)
  test <- cookbook[!include] #test set
  cookbook <- cookbook[include] #training set
  
#create dataframe from json list
  first.row <- cookbook[[1]]
  id <- first.row[[1]]
  cuisine <- first.row[[2]]
  ingredients <- first.row[3]
  for(i in 2:length(cookbook)){cuisine <- c(cuisine, cookbook[[i]][2])}
  for(i in 2:length(cookbook)){id <- c(id, cookbook[[i]][1])}
  for(i in 2:length(cookbook)){ingredients <- c(ingredients, cookbook[[i]][3])}
  cookbook.df <- cbind(id, cuisine, ingredients)
  cookbook.df <- as.data.frame(cookbook.df)
  rownames(cookbook.df) <- NULL
  saveRDS(cookbook.df, "cookbook.RDS")

#tf,idf
all.ingredients <- unlist(cookbook.df$ingredients) #all ingredients in all recipes. this is the corpus
unique.cuisines <- unlist(unique(cuisine)) #20 unique cuisines
cuisine.recipes <- list() #list of dataframes. each is a collection ofr one cuisine
cuisine.ingredient.counts <- list() #count occurences of each ingredient in every cuisine
for(item in unique.cuisines){cuisine.recipes[[item]] <- subset(cookbook.df, cuisine==item)} #list of data frames of each #csuisine
for(item in unique.cuisines){cuisine.ingredient.counts[[item]] <- table(unlist(cuisine.recipes[[item]]$ingredients))}
unique.ingredients <- (unique(all.ingredients))
ingredient.counts <-  table(all.ingredients)

cuisines.with.ingredient <- vector()  

for(ingredient in unique.ingredients){
  doc.freq <- vector()
  for(cuisine in unique.cuisines){
    doc.freq <- c(doc.freq, ingredient %in% unlist(cuisine.recipes[[cuisine]]$ingredients))
  }
  cuisines.with.ingredient <- c(cuisines.with.ingredient, sum(doc.freq))
  }
names(cuisines.with.ingredient) <- unique.ingredients
idf <- log(21/(cuisines.with.ingredient))

#create matrix of all ingredients weigth by cuisine type
# cuisine.counts <- list(list())
# for(item in unique.cuisines){cuisine.counts <- c(cuisine.counts, list(unlist(cuisine.recipes[[item]]$ingredients)))}
# cuisine.counts[1] <- NULL
# ingredient.weight.matrix <- matrix(, nrow = 20, ncol = 6350, byrow=T)
# rownames(ingredient.weight.matrix) <- unique.cuisines
# colnames(ingredient.weight.matrix) <- unique.ingredients
# #for(num in 1:length(cuisine.counts)){ingredient.weight.matrix[num,] <- (sapply(unique.ingredients, function(x)                            sum(cuisine.counts[[num]]==x))) * idf}
# 
# for(num in 1:length(cuisine.counts)){ingredient.weight.matrix[num,] <- sapply(unique.ingredients, function(x) sum(cuisine.counts[[num]]==x))}
# # ingredient.weight.matrix <- t(ingredient.weight.matrix)
# ingredient.weight.matrix <- apply(ingredient.weight.matrix, 2, function(x) x/max(x)*idf)
# ingredient.weight.matrix <- t(ingredient.weight.matrix)
#create ingredient for each recipe in test case



is.ingredient.in.cuisine <- function(num){
  true <- c()
  for(ingredient in unique.ingredients){
    true <- c(true, ingredient %in% unlist(cookbook.df[num,3]))}
  true}

l <- sapply(1:nrow(cookbook.df), is.ingredient.in.cuisine)
interested in the subjectl <- t(l)
#both of the operations below multiply matrix v by vectore idf row-wise
m <- t(t(l) * idf)
```
 #test data


  first.row <- test[[1]]
  id <- first.row[[1]]
  cuisine <- first.row[[2]]
  ingredients <- first.row[3]
  for(i in 2:length(test)){cuisine <- c(cuisine, test[[i]][2])}
  for(i in 2:length(test)){id <- c(id, test[[i]][1])}
  for(i in 2:length(test)){ingredients <- c(ingredients, test[[i]][3])}
  test.df <- cbind(id, cuisine, ingredients)
  test.df <- as.data.frame(test.df)
  rownames(test.df) <- NULL

#create ingredient for each recipe in test case
test.weight.matrix <- matrix(, nrow = 7914, ncol = 6350, byrow=T)
rownames(test.weight.matrix) <- test.df$cuisine
colnames(test.weight.matrix) <- unique.ingredients
test.ingredients <- list(list())
is.ingredient.in.recipe <- function(num){
  true <- c()
  for(ingredient in unique.ingredients){
  true <- c(true, ingredient %in% unlist(test.df[num,]$ingredients))}
  true
  # test.weight.matrix[num,] <- true
  #  sapply(unique.ingredients, function(x, num) ingredient %in% unlist(test.df[num,]$ingredients)) #does not speed up creation of test.weight.matrix
  }

# for(num in 1:7914){test.weight.matrix[num,] <- is.ingredient.in.recipe(num)}#takes_forever

v <- sapply(1:7914, is.ingredient.in.recipe)
v <- t(v)
#both of the operations below multiply matrix v by vectore idf row-wise
w <- t(t(v) * idf)
#w2 <- sweep(v ,MARGIN=2,idf,`*`) the firon is faster
cookbook.df$cuisine <- as.character(cookbook.df$cuisine)
cookbook.df$cuisine <- as.factor(cookbook.df$cuisine)
cl <- cookbook.df$cuisine
test.df$cuisine <- as.character(test.df$cuisine)
rownames(w) <- test.df$cuisine
result <- apply(v, 1, function(row) t(apply(ingredient.weight.matrix, 1, function(row2) sum(row*row2))))
result <- t(result)
colnames(result) <- rownames(ingredient.weight.matrix)

by.chance <- sudo apt-get install google-chrome