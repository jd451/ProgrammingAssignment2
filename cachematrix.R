# Initialize the objects x (empty matrix) and 
# v (placeholder for the inverse)

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL

  
# set takes the argument y and assigns y to the parent object x
# resets v to null to clear cache
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
# Anonymous function. Get is looking for x, so we are telling it where
# to look
  
  get <- function() x
  
# assign value of inverse to v in parent env so we can access it later
  
  setinv <-function(solve) v <<- solve
  
# anonymous function. get inv is looking for inverse so we are 
# telling it where to look
  
  getinv <- function() v
  
# create a list with all of the funcitons and send to parent env
  
  list(set=set, get=get, setinv = setinv, getinv= getinv)
}


# Retrieves the inverse from the cache created by makeCacheMatrix

cacheSolve <- function(x, ...) {

# call getinv() function from makeVector()
  v <- x$getinv()
  
# check for NULL (nothing cached). If not NULL, then get the inv
# funciton, calculate it and set the value
  
  if(!is.null(v)){
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinv(v)
  v
}
