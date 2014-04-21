#### This functions stores the results of a inverse matrix calculation
#### to avoid processing same data over and over

#### makeCacheMatrix provides a list which includes four functions to
#### set and get the matrix and its inverse. Also includes a variable i
#### to store the inverse matrix when is calculated

makeCacheMatrix <- function(x = matrix()) {

   i <- NULL

   set <- function(y){
      x <<- y
      i <<- NULL
   }

   get <- function() x
   setinv <- function(inv) i <<- inv
   getinv <- function() i
   list(set = set, get = get, setinv = setinv, getinv = getinv)
} 

#### CacheSolve returns a matrix that is the inverse of 'x'.
#### Inverse matrix is stored in variable i in makeCacheMatrix 
#### if calculation was done previously.
#### Otherwise, calculates the inverse matrix, stores results in
#### variable i, and returns its value.

cacheSolve <- function(x, ...) {
   #### if inverse was calculated previously
   i <- x$getinv()
   if(!is.null(i)){
      message("getting cached data")
      return(i)
   }
   #### if inverse was not calculated previously
   message("calculating inverse matrix")
   data <- x$get()
   i <- solve(data,...)
   x$setinv(i)
   return(i)
}

