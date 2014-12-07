## This function is to inverse a matrix only once. 


## The makeCacheMatrix function is to cache the matrix x. 
## Matrix x is first installed in inv (inv is set as NULL to start with)
makeCacheMatrix <- function(x = matrix())  {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function is to inverse matrix x. If x has been inversed already, 
## it returns the original inversed matrix. If x hasn't been inversed,
## it inverses and returns the inversed matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}