## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## It uses the assignment operator <<- to insulate internal variables from
## exposure to the outside environment. x is a square, non-singular matrix.

makeCacheMatrix <- function(x = matrix()) {

   inv = NULL #Store inverse
   
   set = function(y) {
      x <<- y
      inv <<- NULL
   }
   # set and get functions for matrix and inverse
   get = function() x
   setinv = function(inverse) inv <<- inverse
   getinv = function() inv
   
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve 
##the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv = x$getinv() #get the elements of the inverse
    if(!is.null(inv)) {
       message("Getting Cached Data")
       return(inv)
   }
   
   mat.data = x$get()
   inv = solve(mat.data, ...)
   x$setinv(inv)

   return(inv)

}
