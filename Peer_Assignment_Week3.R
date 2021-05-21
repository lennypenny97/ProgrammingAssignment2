# R-Programming - Coursera
# Week 3


# Peer-graded Assignment - Leon Sohl
## descriptions will descripe what the functions does
## Two functions: makeCacheMatrix, cacheSolve
## To calculate the inverse of non- and squared matrices -> MASS is used

library(MASS)

## makeCacheMatrix function
### Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # starting with inv = NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
          }
  get <- function() x # function for getting matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <<- function() { # function to obtain inverse of the matrix
        inver <- ginv(x)
        inver%*%x
          }
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function
### Computes the inverse of the special "matrix" returned by makeCacheMatrix 
### If the inverse has already been calculated, cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) { # getting the cache data
  inv <- x$getinv()
  if(!is.null(inv)) { # here we check for inv = NULL
        message("getting cached data!")
        return(inv) # returning the inverse value
          }
  data <- x$get()
  inv <- solve(data, ...) # inverse values gets calculated
  x$setinv(inv)
  inv # finally returning the inverse matrix of x
} 


## Little example for testing the functions (remove the first # to test the code)

# t <- makeCacheMatrix(matrix(1:8, 2, 4))
# t$get() ## get the matrix
# t$getinv() ## get the inverse of the matrix
# cacheSolve(t) ## getting the message



