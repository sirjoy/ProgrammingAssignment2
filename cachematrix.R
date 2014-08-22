## Functions for computing the inverse of matrix 
## The inverse is stored in a special matrix object for 
## fast retrieval
##
## Jonny Uribe
## sirjoy.work@gmail.com
##
## Ago 2014

## Special Matrix wich can contain its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Inverse matrix
  inversa <- NULL
  # Function for setting matrix values
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  # Function for getting matrix values
  get <- function() x
  # Function for setting inverse matrix, used by cacheSolve
  setInverse <- function(matrixInverse) inversa <<- matrixInverse
  # Function for getting stored matrix inverse 
  getInverse <- function() inversa
  # List for returning the special matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse matrix of a special matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  # If the inverse had been computed previously
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # If the inverse is absent
  M <- x$get() # Get matrix
  inverse <- solve(M, ...) # Computes inverse
  x$setInverse(inverse)
  inverse  
}

# Test code:
# Creating a new special matrix
m <- makeCacheMatrix(matrix(sample(9), nrow=3))
m$get() # matrix
m$getInverse() # no inverse yet
# Computing inverse of special matrix
mi <- cacheSolve(m)
mi%*%m$get() # Almost the identical matrix
mi2 <- cacheSolve(m) # The cached data
