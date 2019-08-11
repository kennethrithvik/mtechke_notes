
library(recommenderlab) 
library(ggplot2) # For plots
library(permute) # For Vegan
library(lattice) # For Vegan
library(vegan) # For distance
library(proxy) # For distance

# Load the data 
# (they are loaded into recommenderlab objects - a sparse ratings matrix (rows = users, cols = items))
data(Jester5k)  # 5000 users and 100 jokes
data(MovieLense) # 943 users and 1664 movies

######################################
# basic visualisation and exploration (on jester data)
#######################################

head(as(Jester5k,"data.frame"))  # converts to "transaction format" 
dim(Jester5k@data) # the raw data in the "dgCMatrix" object
head(Jester5k@data[1:20,1:20]) # @ is used to get a slot in any R object, data ~ a sparse Matrix of class "dgCMatrix"

## number of ratings
nratings(Jester5k)
# also can
length(getRatings(Jester5k))

## number of ratings per user
summary(rowCounts(Jester5k)) # about 72 on average

## rating distribution
hist(getRatings(Jester5k), main="Distribution of ratings")

## 'best' joke with highest average rating
best <- which.max(colMeans(Jester5k))
cat(JesterJokes[best])

# Keeping only jokes with more than 80 ratings and users with more than twenty rating
Jester5k_r <- Jester5k[rowCounts(Jester5k) > 80,  colCounts(Jester5k) > 20]
head(Jester5k_r@data[1:20,1:20])

######################################
# basic visuaisation and exploration (on movielens data)
#######################################
rowCounts(MovieLense) # count number of ratings per row (user)
colCounts(MovieLense) # count number of ratings per col (movie)

head(MovieLense@data[1:20,1:20]) # matrix with rows ~ movies and cols ~ users

# How many movies did people rate on average
# Seems people get tired of rating movies at a logarithmic pace. But most rate some.
qplot(rowCounts(MovieLense), binwidth = 10,
      main = "Movies Rated on average",
      xlab = "# of users",
      ylab = "# of movies rated")

# how many ratings for each movie plot
qplot(colCounts(MovieLense), binwidth = 10,
      main = "# ratings per movie",
      xlab = "# of ratings made (#users)",
      ylab = "# number of movies with this rating")

# What is the mean rating of each movie
qplot(colMeans(MovieLense), binwidth = .1,
      main = "Mean rating of Movies",
      xlab = "Rating",
      ylab = "# of movies")

summary(getRatings(MovieLense)) # Skewed to the right

# using recommenderLab
qplot(getRatings(MovieLense), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")

# Visualizing a sample of this (sample is a base fn, image is in arules and assumes s4 objects)
image(sample(MovieLense, 500), main = "Raw ratings")

############################################################
# predict ratings using UBCF with cosine similarity
############################################################

# set up the test protocol
# If given=x then foreach user the ratings for x random chosen items are used to build model, rest are used for test 
# If x is negative then model build uses all but x withheld ratings. 
scheme <- evaluationScheme(data = Jester5k,   method = "split", train = 0.8, given = 25, goodRating = 0.1)
scheme <- evaluationScheme(data = MovieLense, method = "split", train = 0.8, given = 10, goodRating = 4)

# Learns a recommender model from given data
recommender <- Recommender(getData(scheme, "train"), "UBCF", parameter = list(method = "Cosine"))

# Below creates recommendations using a recommender model and data about new users.
# The default type is "topNList" which creates a top-N recommendation list with recommendations.
# Some recommenders can also create other results 
# (e.g., type "ratings" returns only predicted ratings with known ratings represented by NA, 
#     or type "ratingMatrix" which returns a completed rating matrix).

# get actual predictions
preds <- predict(recommender, getData(scheme, "known"), type="ratings")
as(preds, "matrix")[1:5,1:5] # show a sample of the actual predictions
# compute prediction accuracy (as RMSE)
results <- rbind(calcPredictionAccuracy(preds, getData(scheme,"unknown")))
results

# compute prediction accuracy as confusion matrix
preds <- predict(recommender, getData(scheme, "known"), type="topNList")
results <- rbind(calcPredictionAccuracy(preds, getData(scheme,"unknown"), given=10, goodRating=4)) # use given value set above
results

# to get confusion matrices (use default topNlist, and do it for different values of N)
results <- evaluate(scheme,"UBCF", parameter=list(method="Cosine"), n=c(1, 3, 5, 10, 15, 20)) # does same thing
avg(results) # shows the confusion matrix (one for each value of n)
plot(results, annotate=TRUE)  # Draw ROC curve (the default)
plot(results, "prec/rec")
show(results)

# How about after normalization?
qplot(getRatings(normalize(MovieLense, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating")

summary(getRatings(normalize(MovieLense, method = "Z-score"))) # seems better?

results <- evaluate(scheme,"UBCF", parameter=list(method="Cosine",normalize = "Z-score"), n=c(1, 3, 5, 10, 15, 20))
avg(results) # shows the confusion matrix (one for each value of n)
plot(results, annotate=TRUE)  # Draw ROC curve (the default)
plot(results, "prec/rec")

###############################################################
## testing more than one algorithm at once
###############################################################

recommenderRegistry$get_entries(dataType = "realRatingMatrix") # view the various algs available

#scheme <- evaluationScheme(MovieLense, method = "split", train = .9, k = 1, given = 10, goodRating = 4)

algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF2", param=list(normalize = "Z-score")))

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

#results[[1]] # show first results
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
# See precision / recall
plot(results, "prec/rec", annotate=3)
