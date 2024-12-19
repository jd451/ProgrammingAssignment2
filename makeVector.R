## myVector <- makeVector(1:15)
## myVector contains the functions set(), get(), setmean(), 
## and getmean(), plus the objects with the data x and m
## the object x contains the vector 1:15 in this case

## Initialize the objects x (empty numeric vector) and 
## m (placeholder for the mean)

makeVector <- function(x = numeric()) {
  m <- NULL
  
  ## set takes the argument y and assigns y to the parent object x
  ## resets m to NULL in case there is anything already in the cache
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Anonymous function. Get is looking for x, so we are telling it to
  ## look in the parent env
  
  get <- function() x
  
  ## assign the value of mean to m in the parent env so that we can 
  ## access it later
  
  setmean <- function(mean) m <<- mean
  
  ## Anonymous functioin. Getmean is looking for mean, so we are 
  ## telling it to look in parent env
  
  getmean <- function() m
  
  ## creates a list with all of these named functions and sends to 
  ## parent env. we can use $ to extract by name
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##Retrieves the mean from the cache created by makeVector()

cachemean <- function(x, ...) {
  
  ## calls the getmean() fucntion from makeVector()
  m <- x$getmean()
  
  ## checks for NULL (i.e., nothing cached). If not NULL, get the mean
  ## function, calculate, and set the mean value
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

