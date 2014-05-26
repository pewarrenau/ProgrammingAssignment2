## This R script contains two functions: makeCacheMatrix and cacheSolve
## IMPORTANT NOTE - These methods do not have error checking. You must pass
## valid data or things break

## The function 'makeCacheMatrix' creates an object which contains a
## martix (x), it's inverse (s) and four listed methods:
## 1. set - Used to set a matrix to the v value. E.g. a$set(matrix(1:4,2,2))
## 2. get - Used to return the value of x. E.g. a$get()
## 3. setsolve - Used by 'cacheSolve'to solve the inverse matrix of x. Should
##     not be directly called
## 4. getsolve - Used by 'cacheSolve'to return the the inverse matrix of x. Can
#      be called directly. E.g a$getsolution()


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) s <<- solve
  getsolution <- function() s
  list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
  
}

## The function 'cacheSolve' is used to solve the inverse matrix of x by calling
## the 'setsolution' method of the 'makeCacheMatrix'function. E.g cacheSolve(a)
## The 'cacheSolve' function first checks the value 's' in the 'makeCacheMatrix'
## function to determine if the inverse matrix has already been solved and stored.
## If the inverse matric has not been solve then the 'cacheSolve' function will
## run the 'solve' function from the 'setsolution' method of 'makeCacheMatrix' and
## store the result in 's'. If 'cacheSolve' determines 's' is not NULL then the
## fucntion directly reports the current value of 's' without trying to recalucate
## the inverse matrix
## USAGE: cacheSolve(a)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolution()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolution(s)
  s
  
}
