## create a function that accepts a matrix as argument and returns a list of functions on the matrix
makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
  	## delcare another function "set" which sets the value of the matrix to that passed
    ## Note use of <<- to assign a value to an object in a different environment (from the current)
  	set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  	## Fetch the original matrix
  	get <- function() x
  	## Set the inverse of the matrix; Note the absence of brackets for single-line function
    setinverse <- function(solve) matinv <<- solve 
    ## Fetch the inverse of the matrix     
    getinverse <- function() matinv      
    
    ## Return: a list containing functions to:
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ## This list can be used as inputs to other functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function to return the inverse of the matrix, either from cache or calculated afresh
cacheSolve<- function(x, ...) {                 
  matinv <- x$getinverse()
  
  #if the inverse is already calculated, fetch it
  if(!is.null(matinv)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matinv)
  }
  # otherwise, first calculate the inverse and then return it.
  data <- x$get()                               
  matinv <- solve(data, ...)
  # Set the value of the inverse in the cache via the setinverse function
  x$setinverse(matinv)
  matinv
}