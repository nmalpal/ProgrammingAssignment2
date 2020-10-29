## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix consists of set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)) { ##checking whether inverse is null
    message("getting cached data")
    return(inv)  ## Return a matrix that is the inverse of 'x'
  }
  
  mat <- x$get()
  inv <- solve(mat, ...) ##calculates inverse value
  x$setInverse(inv)
  inv
}
