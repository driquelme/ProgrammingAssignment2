## This is the solution to the Programming R second programming assignment.

##
## The function, makeCacheMatrix creates a special matrix, which is really a list
## containing functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
## The function, cacheSolve calculates the inverse os the special matrix 
## created with makeCacheMatrix.
## It first checks if the inverse has already been calculated, and if so, it 
## gets the mean from the cache ans skips the computation. Otherwise, it 
## it calculates the inverse of the data using solve() and sets the value of
## the inverse in the cache using the setmean function.
## This code assumes that the data represents an square invertible matrix.
##
## This code is testable using the following snippet:
##
##> source("cachematrix.R")
##> c=rbind(c(1, -1/4), c(-1/4, 1))
##> mat = makeCacheMatrix(c)
##> cacheSolve(mat)
##
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
##> cacheSolve(mat)
##
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
