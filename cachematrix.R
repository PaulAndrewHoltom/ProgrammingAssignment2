## These two functions used in combination 
## (see "How to use these functions" below) will
##
## (i)  calculate the inverse of an inputted invertible square matrix, or
## (ii) return the cached inverse of an inputted invertible square matrix
##      if the inverse has already been calculated and the input matrix
##      has not changed

## The makeCacheMatrix function creates an object
## that caches the inverse of the inputted matrix x.
## The object is a list comprising (set, get, setinverse, getinverse),
## which becomes the input to the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  # define the functions set, get, setinverse and getinverse
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  # return list (set, get, setinverse, getinverse)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the "matrix"
## returned by the makeCacheMatrix function. 
## If the inverse has been calculated already, the function returns 
## the cached inverse, otherwise it calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  # if the inverse has already been calculated, return the cached inverse
  if(!is.null(s)) {
    message("getting cached inverse matrix")
    return(s)
  }
  # else calculate and return the inverse
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  message("calculating inverse matrix")
  s
}

## How to use these functions:
## - define matrix 'a' (it should be an invertible square matrix)
## - call makeCacheMatrix with argument 'a', assign result to 'b'
## - call cacheSolve with argument 'b'

## The expected behavior when cacheSolve is called:  
## - if the inverse of 'a' has not already been calculated,
##    the function returns the message "calculating inverse matrix" 
##    and prints the inverse matrix
## - if the inverse of 'a' has already been calculated (and 'a' has not changed),
##    the function returns the message "getting cached inverse matrix" 
##    and prints the inverse matrix

## To test result:
## n <- 5 (choose integer)
## a <- matrix(rnorm(n*n),nrow=n,ncol=n)
## b <- makeCacheMatrix(a)
## c <- cacheSolve(b)
## a %*% c 
## Result should be an (nxn) identity matrix (or very close!!!)
## Use "round(a %*% c,10)" to see a clearly (nxn) identity matrix