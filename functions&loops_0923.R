

fahr_to_kelvin <- function(temp) {
  kelvin <- 273.15 + ((temp - 32) * (5/9))
  return(kelvin)
}

kelvin_to_celcius <- function(t) {
  celsius <- t - 273.15
  return(celsius)
}

fahr_to_celcius <- function(temp) {
  kelvin <- fahr_to_kelvin(temp)
  celcius <- kelvin_to_celcius(kelvin)
  return(celsius)
}

# Define a function "fence" - which takes 2 vectors and returns the second vector concatenated to the first
fence <- function(x, y) {
  return(c(y,x))
}

test_vec1 <- c("Shiyin", "is", "awesome")
test_vec2 <- c(1,2,3,4)

input_1 <- 20
mySum <- function(input_1, input_2 = 20) {
  print(input_1)
  print(input_2)
  output <- input_1 + input_2
  return(output)
}


unzip("data/r-novice-inflammation-data.zip")
dat <- read.csv("data/inflammation-01.csv", header=FALSE)
centered <- center(dat[,4],0)

# Function to center my data
center <- function(data, desired) {
  # return a new vector containing original data centered around the desired value
  # Example: center(c(1,2,3),0) => c(-1,0,1)
  new_data <- (data - mean(data)) + desired
  return(new_data)
}

# ---- Apply Functions -----
patient1 <- dat[1,]
avg_day_inflamation <- apply(dat, 2, mean)
min_day_inflamation <- apply(dat, 2, min)
max_day_inflamation <- apply(dat, 2, max)

# Exercise :
# create a function called "analyze" whose input is the name of a dataset 
# (e.g. "data/inflammation-01.csv") and output a plot of mean inflammation per day

analyze <- function(dataset) {
  dat <- read.csv(dataset, header=F)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
}

# Loops
analyze <- function(dataset) {
  dat <- read.csv(dataset, header=F)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, main = dataset)
}

dir("data/")

analyze("data/inflammation-01.csv")
analyze("data/inflammation-02.csv")
analyze("data/inflammation-03.csv")

# This is equivalent to c(1,3,5)+1:
for(i in c(1,3,5)) {
  print(i+1)
}

inflammation_files <- c(
  "data/inflammation-01.csv",
  "data/inflammation-02.csv",
  "data/inflammation-03.csv",
  "data/inflammation-04.csv",
  "data/inflammation-05.csv"
)

for(filename in inflammation_files) {
  analyze(filename)
}

all_information_files <- dir(
  path = "data", 
  full.names = T, 
  pattern = "^inflammation"
  )

x <- c(1:5)
sum_x <- 0
for(i in x) {
  cat("i:", i, "\n")  #a new line
  cat("sum_x:", sum_x, "\n")
  sum_x <- sum_x+i
}

exponentiate <- function(x,y) {
  result <- 1
  for(i in 1:y) {
    result <- result*x
  }
  return(result)
}
exponentiate(2,3)

dat <- read.csv("data/inflammation-01.csv", header = F)
patient_mean <- apply(dat, 2, mean)

patient_mean_function <- function(filename) {
  dat <- read.csv(filename, header = F)
  rows_cols <- dim(dat)
  dat_cols <- rows_cols[2]
  means_day <- numeric(dat_cols)  #an empty vector of dat_cols number of 0's
  for(i in 1:dat_cols) {
    means_day[i] <- mean(dat[,i])
  }
  plot(means_day, main=filename)  
}
patient_mean_function("data/inflammation-01.csv")

x <- 10:1
N <- length(x)
for(i in 1:N) {
  print(x[i])
  print(y[i])
}

for(i in x) {
  print(i)
}

# This function plots the mean inflammation of each patient across all days
analyze_rows <- function(dataset) {
  dat <- read.csv(dataset, header=F)
  avg_patient_inflammation <- apply(dat, 2, mean)
  plot(avg_patient_inflammation, main = dataset)
}
analyze_rows("data/inflammation-01.csv")

# This function plots the mean inflammation of each day across all patients
analyze_cols <- function(dataset) {
  dat <- read.csv(dataset, header=F)
  avg_day_inflammation <- apply(dat, 1, mean)
  plot(avg_day_inflammation, main = dataset)
}
analyze_cols("data/inflammation-01.csv")

analyze("data/inflammation-02.csv")
pdf("inflammation-02.pdf")
dev.off()

# Conditionals in R
num <- 3
if (num >5) {
  print("greater than 5!")
} else {
  print("try again!!!")
}

num <- 53
if (num > 100) {
  print("greater than 100!!")
}

# Exercise: write a function to return (0,-1,1) based on the sign of the input integer,
# call the function "sign"
sign <- function(input) {
  if (input > 0) {
    return("1")
  } else if (input == 0) {
    return("0")
  } else {
    return("-1")
  }
}
sign(-5)

if ((1 > 0) & (-1 > 0)) {
  print("both parts are true")
} else{
  print("at least one part is false")
}

if ((1 > 0) | (-1 > 0)) {
  print("at least one part is true")
} else{
  print("both parts are false")
}

# Grabbing odd numbers
n100 <- seq(1,100)
getOdd <- function(v) {
  return(v[v %% 2 == 1])
}
getOdd(n100)

# Exercise: write a function which takes 3 arguments:
# v - a vector of integers
# x - a number to check for even divisibility
# y - a second number to check for even divisibility
# output should be all numbers in "v" that are evenly divisible by both "x" and "y"
# Example: double_div(c(1,2,3,4), 2, 4) => [4]
double_div <- function(v, x, y){
  for(i in v) {
    return(v[(v %% x == 0) & (v %% y == 0)])
  }
}
double_div(c(1,2,3,4), 2, 4)
double_div(c(1,2,3,4,5,6,7,8,12,23,24), 2, 3)

