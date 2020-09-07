

## This function execute a matrix which is invertible

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(b){
          p <<- b
          inv <<- NULL
  }
  get <- function() z
  getInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## Finding Inverse from the cache matrix

cacheSolve <- function(p, ...) {
        inv <- p$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- p$get()
  inv <- solve(data)
  p$setInverse(inv)
  inv      
}
