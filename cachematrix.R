## creates a special "matrix" caching its inverse.
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  get <- function() x
  getInverse <- function() {
    if(length(x)%%sqrt(length(x))==0) { ## make sure matrix is square
      inv <<- solve(x)
    }
  }
  list(get = get, getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. The cachesolve should retrieve 
## the inverse from the cache if already calculated
cacheSolve <- function(x, ...) {
  if(!is.atomic(x)){      # check if the given parameter is atomic
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }     
  } else {        # if not atomic then make it
    message("getting the inverse-- no cached data found")
    return(makeCacheMatrix(x)$getInverse())
  } 
  
}

## non-atomic example
f <- makeCacheMatrix(matrix(1:4, 2, 2))
print(cacheSolve(f))

## atomic examples
print(cacheSolve(matrix(1:4, 2, 2)))
