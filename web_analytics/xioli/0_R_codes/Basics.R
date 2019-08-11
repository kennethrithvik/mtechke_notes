######################################################################################
# Basic functions
######################################################################################

# list installed packages
library()

# install a package
install.packages("ggplot2")

# load a library
library(ggplot2)

# search help files
??mean

# help for a function
?mean

######################################################################################
# Creating data
######################################################################################

# variable assignment
a = 1; b = 2; c = 3

# arrays
a = c(1,2,3)
b = c("x", "y", "z")
c = 1:5
seq(1,2,by=.2)
rep(1:5,3)

# matrices
matrix(1:10,2,5)

# data frames
codes = data.frame(id.x=1:4, code=c("B","B","A","D"))
colors = data.frame(id.y=1:4, color=c("red","red",NA,"white"))
d = merge( codes, colors, by.x="id.x", by.y="id.y")

######################################################################################
# Getting information about variable
######################################################################################

str(d)
summary(d)
is.na(d)
length(c)
dim(d)
nrow(d)
ncol(d)

######################################################################################
# Assessing data in objects
######################################################################################

# arrays
c
c[3]
c[1:3]
c[-2]
c[c(2,4)]

b
b[c(1,3)]

# matrix and dataframes
d
d[1:3,]
d[,1]

# dataframes by column names
d$code
d$color
d[c("code", "color")]

######################################################################################
# Simple statistics
######################################################################################

mean(1:9)
max(4:8)
range(c)
sum(c)
quantile(c,0.95)
rank(c)
var(c)
sd(c)
cor(c,rnorm(5))
table(d$color)
table(d$code,d$color)

######################################################################################
# Simulate data
######################################################################################

rnorm(1000)
rpois(10,4)

######################################################################################
# Sampling data
######################################################################################

sample(1:10)
sample(1:10, size = 5)
sample(1:10, size = 15, replace = TRUE)
set.seed(26011973)
sample(1:10, size = 15, replace = TRUE)


######################################################################################
#PLOT
######################################################################################
x=c(1,2,3, 4, 5)
y=c(1,4,9, 16, 25)
plot(x,y) 
plot(y,x)
plot(y)
x11() 
plot(x,y)
plot(x,y, main="My first Figure Using R",  sub="what is subtitle?")
plot(x,y, main="My first Figure Using R", col="red", sub="what is subtitle?", xlab="X-axis label", ylab="y-axix label", xlim=c(0, 6), ylim=c(0, 26))


######################################################################################
# Conditional statements
######################################################################################
#Avoid inserting newlines between '} else' 
if(1==0) {
  print(1) 
} else { 
  print(2) 
}


w=5
w
k=10
k
if (w>k)
{
  print("The bigger one is w");
  w
} else {
  print("The bigger one is k")
  k
}


w=10
w
k=5
k
if (w>k)
{
  print("The bigger one is w");
  w
} else {
  print("The bigger one is k")
  k
}


######################################################################################
# Loops
######################################################################################
total = 0  
for (x in 1:10) 
  total = total + x
total

######################################################################################
# List
######################################################################################
#Objects containing an ordered collection of objects, 
#Components do not have to be of same type
#Use list() to create a list:
a <- list("hello", c(4,2,1),"class")
#Components can be named:
a <- list(string1="hello",num=c(4,2,1),strng2="class")
#Use [[position]] to access list element
a[[1]]
a[[2]]
length(a)


######################################################################################
# Functions
######################################################################################

power<-function(x,y) x**y
power(2,3)

graphnormal<-function(n=1000) {
  d = rnorm(n)
  hist(d)
}
graphnormal(10000)


######################################################################################
# save
######################################################################################

save(d,file="d.RData")

######################################################################################
# Memory
######################################################################################

objects()
ls()
rm(list=ls())

######################################################################################
# load
######################################################################################

load("d.RData")
