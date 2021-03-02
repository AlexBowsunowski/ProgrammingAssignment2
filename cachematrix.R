makeCasheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)){
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() m
  list(set = set, get = get,
         setSolve = setSolve, getSolve = getSolve)
}

casheSolve <- function(x, ...){
  s <- x$getSolve
  if(!is.null(s)){
    message("getting inversed matrix")
    return(x)
  }
  data <- x$get
  s <- solve(x, ...)
  x$setSolve(s)
  s
}














