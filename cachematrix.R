
## This function creates a special "matrix" object that can cache its inverse.

makeCasheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)){
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

casheSolve <- function(x, ...){
  s <- x$getSolve
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(s)){
    message("getting cashed data")
    return(x)
  }
  data <- x$get
  s <- solve(x, ...)
  x$setSolve(s)
  s
}
