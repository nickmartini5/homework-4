## Homework 4
## STA 308
## Nick Martini






bday.problem <- function(num_people = 23){                 
  bdays <- 1:365
  sample1 <- sample(bdays,num_people, replace = TRUE)      ## Defining the function, sampling days from 1 to 365
  print(length(unique(sample1[duplicated(sample1)]) ))     ## Print the length aka number of matches
}


num_people <- 10      ## assign num_people to 10
value.list <- list()   ## creating an empty list
index <- 1       ## creating an index value of 1

while(num_people <= 100) {
  set.seed(314159)
  matches <- sapply(rep(num_people, 100,000), bday.problem)    ## while loop that repeats the function 100,000 times 
                                                               ## for 10 people, 20 people, 30 and so on
  value.list[[index]] <- c(matches)
  index <- index + 1
  num_people <- num_people + 10
}
  
  
for(i in 1:10) { 
    barplot(table(value.list[[i]]))                ## for loop that creates a bar plot and summary for all of our values
                                                   ## of matches in out list
    print(summary(value.list[[i]]))         
}
