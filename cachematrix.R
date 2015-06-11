## These functions work together to calculate the inverse of a matrix
## but they do it by caching calculations so they do not have to be recalculated
## thereby increasing performance
## 
## Example Use:
## c=rbind(c(1, -1/4), c(-1/4, 1))
## c_cm <- makeCacheMatrix(c)
## cacheSolve(c_cm)
## -- RESULTS --
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## This function creates a special matrix that caches our inverse function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function uses our special matrix to calculate the inverse of our matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
