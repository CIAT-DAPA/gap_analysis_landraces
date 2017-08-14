Loops optimization in R
# H. Achicanoy
# CIAT, 2017

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Loops optimization
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #


#r no es un leguaje d eprogramacion declarativo-no es eficiente por que gasta mas memoria
#limpiar la RAM gc(reset+TRUE)
#funciones compiladas 
options(warn = -1); options(scipen = 999); rm(list = ls())

install.packages("nycflights13")
suppressMessages(library(nycflights13))

options
data("flights")
View(flights)

##### Objective: CALCULATE TOTAL AIR TIME PER MONTH

### Common "for" loop

month_list <- 1:12 # Input
air_time_month <- c() # Output

system.time(expr = {
  
  for(i in 1:length(month_list)){ # Iterate by month
    
    flights_filtered <- flights[flights$month == month_list[i],] # Subset by month
    air_time_month[i] <- sum(flights_filtered$air_time, na.rm = T) # Sum air time per month
    
  }
  
})
air_time_month
# PLEASE LOOK THE NUMBER OF OBJECTS CREATED ON THE GLOBAL ENVIRONMENT



### Common "for" loop a little better

rm(list=setdiff(ls(), "flights"))

month_list <- 1:12 # Input
air_time_month <- rep(NA, length(month_list)) # Output

system.time(expr = {
  
  for(i in 1:length(month_list)){ # Iterate by month
    
    flights_filtered <- flights[flights$month == month_list[i],] # Subset by month
    air_time_month[i] <- sum(flights_filtered$air_time, na.rm = T) # Sum air time per month
    rm(flights_filtered)
    gc(reset = T)
    
  }
  rm(i, month_list)
  
})
air_time_month
# PLEASE LOOK THE NUMBER OF OBJECTS CREATED ON THE GLOBAL ENVIRONMENT



### lapply alternative

rm(list=setdiff(ls(), "flights"))

month_list <- 1:12 # Input

system.time(expr = {
  
  air_time_month <- unlist(lapply(1:length(month_list), function(i){
    
    flights_filtered <- flights[flights$month == month_list[i],] # Subset by month
    air_time_month <- sum(flights_filtered$air_time, na.rm = T) # Sum air time per month
    return(air_time_month)
    
  }))
  rm(month_list)
  
})
air_time_month <- data.frame(month = 1:12, air_time_month = air_time_month)
# PLEASE LOOK THE NUMBER OF OBJECTS CREATED ON THE GLOBAL ENVIRONMENT



### dplyr alternative
"%>% <----- se lee como entonces"
install.packages("dplyr")
rm(list=setdiff(ls(), "flights"))

suppressMessages(library(dplyr))

system.time(expr = {
  
  air_time_month <- flights %>% group_by(month) %>% summarize(air_time_month = sum(air_time, na.rm = T))
  
})
# PLEASE LOOK THE NUMBER OF OBJECTS CREATED ON THE GLOBAL ENVIRONMENT


##FUNCIONES COMPILABLES
install.packages("compliler")

library(compiler)


is.compile <- function(func)
{
  # this function lets us know if a function has been byte-coded or not
  #If you have a better idea for how to do this - please let me know...
  if(class(func) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(func),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}

# old R version of lapply
slow_func <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  if (!is.list(X))
    X <- as.list(X)
  rval <- vector("list", length(X))
  for(i in seq(along = X))
    rval[i] <- list(FUN(X[[i]], ...))
  names(rval) <- names(X)          # keep `names' !
  return(rval)
}


f1<-function(){month_list <- 1:12 # Input
air_time_month <- c()
for(i in 1:length(month_list)){ # Iterate by month
  
  flights_filtered <- flights[flights$month == month_list[i],] # Subset by month
  air_time_month[i] <- sum(flights_filtered$air_time, na.rm = T) # Sum air time per month
  
}
}
f2<-cmpfun(f1)
is.compile(f2)#tiene que dar TRUe

system.time(f1())
system.time(f2())

# Compiled versions
require(compiler)
slow_func_compiled <- cmpfun(slow_func)

fo <- function() for (i in 1:1000) slow_func(1:100, is.null)
fo_c <- function() for (i in 1:1000) slow_func_compiled(1:100, is.null)

system.time(fo())
system.time(fo_c())


