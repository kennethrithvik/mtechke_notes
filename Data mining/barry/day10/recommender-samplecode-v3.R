
users <- read.csv("simplemovies.csv")

#setwd("D:/datasets/movielens")
#users <- read.csv("u_data_tabular.csv")

# transpose the data (make columns ~ users), ignore record index
items <- as.data.frame(t(users[,2:ncol(users)]))
colnames(items) <- users[,1]

getrecommendations <- function(target) {
  
  # compute similarity between targetuser and all other users
  sims = suppressWarnings(cor(items[,target],items[,!names(items) %in% c(target)],use="pairwise.complete.obs"))
  sims = sims[1,!is.na(sims)]
  sims = sims[sims >= 0]

  # for each item compute weighted average of all the other user ratings
  wavrats = apply(items[,names(sims)],1,function(x) weighted.mean(x, sims, na.rm=TRUE))
  wavrats = wavrats[!is.na(wavrats[])]
  
  # remove items already rated by the user
  notseenitems = row.names(items[is.na(items[,target]),])
  t = wavrats[notseenitems]  
  sort(t[!is.na(t)] , decreasing = TRUE)[1:min(5,length(t))]  # get top 5 items
}

# for simplemovies data
getrecommendations("Toby")

# E.g. test for movielens data
getrecommendations("u10")

##################################################
# TESTING
##################################################

testusernames  = sample(names(items), min(10,length(names(items)))) # identify N users randomly for testing
trainusernames = setdiff(names(items),testusernames) # take remaining users for training

#test recommendations for all users
testall_UU <- function() {
  toterr = 0
  for (user in testusernames) {
    mae = testuser(user)
    cat("mae for ", user, "is ", mae, "\n");
    toterr = toterr + mae
  }
  avmae = toterr/length(testusernames)
  cat(sprintf("AVERAGE MAE=%0.4f\n", avmae))
  return(avmae)
}

#test recommendations for a single user using user-user collaborative filtering
testuser_UU <- function(target) {
  testitems = row.names(items[!is.na(items[,target]),]) # get all items that the target has rated
  testitems = sample(testitems,min(10,length(testitems))) # sample 10 items for testing
  targetdata  = items[testitems,target] 
  names(targetdata) = testitems
  traindata = items[testitems,trainusernames] 
  toterr = valid = 0
  for (item in testitems) { 
    truerating = targetdata[item]
    targetdata[item] = NA
    
    # pearson correlation
    sims = suppressWarnings(cor(targetdata,traindata,use="pairwise.complete.obs"))
    sims = sims[,!is.na(sims)] 
    
    # cosine distance measure
    #sims = apply(traindata, 2, function(x) cosinesim(x,targetdata)) # cosine distance
    #sims = sims[!is.na(sims)]
    
    sims = sims[sims >= 0] 
    prediction = weighted.mean(traindata[item,names(sims)], sims, na.rm=TRUE)
    if (!is.na(prediction)) {
      toterr = toterr + abs(prediction - unname(truerating))
      valid = valid + 1 
    }
    targetdata[item] = truerating
  }
  return(toterr/valid)
}

cosinesim <- function(x,y) {
  xy = x*y
  x = x[!is.na(xy)]
  y = y[!is.na(xy)]
  return( sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2))))
}

system.time(testall_UU())

# can do repeats if execution time is too small to register 
# (but increasing #testusers and #testmovies will slow execution greatly and will make this unnecessary)
system.time(replicate(10, testall_UU()))

#  Or.. to compute overall average MAE
multitest <- function(cnt) {
  gmae=0
  for (i in 1:cnt) {
    gmae = gmae + testall_UU()
  }
  cat("gmae = ", gmae/cnt, "\n")
}

system.time(multitest(10))

###########################
# for item-item CF
###########################

#precompute the similarity matrix - need do this once only!
itemsims = apply(items, 1, function(item)
     apply(items, 1, function(x) 1/(1+sqrt(sum((x - item)^2,na.rm=TRUE))))) # euclidean distance
     #apply(items, 1, function(x) 1/(1+(sum((x - item)^2,na.rm=TRUE))))) # euclidean distance used in the book code

# get recommendations for a given user
getrecommendations_II <- function(username) {
  myRats = items[,username]
  wavrats = apply(itemsims, 1, function(simrow) weighted.mean(myRats, simrow, na.rm=TRUE)) 
  notseenitems = row.names(items[is.na(items[,username]),])   # remove items already rated by the user
  t = wavrats[notseenitems]  
  sort(t[!is.na(t)] , decreasing = TRUE)[1:min(5,length(t))]  # get top 5 items
}

# for simplemovies
getrecommendations2("Toby")
#for movielens
getrecommendations2("u10")

#test recommendations for a single user using item-item collaborative filtering
testall_II <- function() {
  toterr = 0
  for (user in testusernames) {
    mae = testuser_II(user)
    toterr = toterr + mae
    cat("mae for ", user, "is ", mae, "\n");
  }
  avmae = toterr/length(testusernames)
  cat(sprintf("AVERAGE MAE=%0.4f\n", avmae))
  return(avmae)
}

testuser_II <- function(target, show=FALSE) {
  testitems = row.names(items[!is.na(items[,target]),]) # take all non-empty items for testing
  testitems = sample(testitems,min(10,length(testitems)))
  testratings  = items[testitems,target] 
  names(testratings) = testitems
  toterr = valid = 0
  for (item in testitems) { 
    truerating = testratings[item]
    testratings[item] = NA
    # multiply target's known movie ratings by the similarity of these movies to item
    prediction = weighted.mean(testratings, itemsims[item,names(testratings)], na.rm=TRUE)
    if (!is.na(prediction)) {
      toterr = toterr + abs(prediction - unname(truerating))
      valid = valid + 1 
      if (show) cat("pred=", prediction, "\tactual=",truerating, "\t", item, "\n")
    }
    testratings[item] = truerating
  }
  return(toterr/valid)
}

system.time(testall_II())

# can do repeats if execution time is too small to register
system.time(replicate(10, testall_II()))

#  Or.. to compute overall average MAE
multitest2 <- function(cnt) {
  gmae=0
  for (i in 1:cnt) {
    gmae = gmae + testall_II()
  }
  cat("gmae = ", gmae/cnt, "\n")
}

system.time(multitest2(10))

