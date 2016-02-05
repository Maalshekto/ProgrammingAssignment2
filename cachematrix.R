## 
## cachematrix.R 
##
## This file contains 2 functions that can be used to handle 
## a specific matrix object and calculate its inverse storing the result in cache.
## 
## Author   - Thomas MARTIN
## Date     - 05/02/2016
## Email    - tmartin@live.fr
## Version  - 1.0
##

## 
## makeCacheMatrix
##
## This function return a specific matrix object in 
## the form of a list of functions to handle it.
## 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## 
## cacheSolve
##
## If present, ths function returns the cache value of the 
## inverse of the supplied specific matrix as argument.
## Otherwise, the inverse is computed then cached for future usage.
##

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Use examples
# > mat <- makeCacheMatrix()
# > mat$set(matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3))
# > cacheSolve(mat)
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(mat)
# getting cached data
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1