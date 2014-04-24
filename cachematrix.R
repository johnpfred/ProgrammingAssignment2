## These functions return the inverse of a matrix.  They use
## a cache to store already calculated inverses to prevent
## duplicate calculations.



## This function creates a matrix object that can cache its inverse.
## The function returns a list of functions for use:
##    - set
##    - get
##    - setCachedMtrx
##    - getCachedMtrx

makeCacheMatrix <- function(x = matrix()) {
  #intially set cached matrix to null
  cachedMtrx <- NULL
  
  #four functions to operate on the matrix x:
  
  #set a new matrix 
  set <- function(y) {
    x <<- y
    cachedMtrx <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #set the cached matrix
  setCachedMtrx <- function(mtrx) {
    cachedMtrx <<- mtrx
  }
  
  #get the cached matrix
  getCachedMtrx <- function() cachedMtrx
    
  list(set = set, get = get,
       setCachedMtrx = setCachedMtrx,
       getCachedMtrx = getCachedMtrx)
  
}




## This function calculates the inverse matrix of what is passed in
## If a cached version exists, it uses the cached version to save
## time from potential duplicate calculation

cacheSolve <- function(x, ...) {
  
  #get the cached matrix if it exists, otherwise NULL
  cachedMtrx <-x$getCachedMtrx()
  
  #if there is a chached inverse matrix, return the cached version
  if(!is.null(cachedMtrx)) {
    message("Getting cached matrix")
    return(cachedMtrx)
  }

  #if no cached inverse matrix is found, calculate the inverse
  data<-x$get() 
  cachedMtrx<-solve(data,...)  #use solve() to get inverse
  x$setCachedMtrx(cachedMtrx)        #set the cached matrix for future use
  
  cachedMtrx  #return the inverse matrix
}
