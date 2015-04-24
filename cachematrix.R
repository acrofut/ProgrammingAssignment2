## functions calculate and cache the inverse of a matrix
## in order to speed up processes where the inverse of
## a matrix must be referenced repeatedly.

## makeCacheMatrix holds a matrix and a cached inverse matrix that is 
## calculated by cacheSolve function.  it also includes a getter and 
## setter for the initial matrix and a getter and setter for the 
## cache to hold the inverse matrix.  it also includes a list of functions.x
makeCacheMatrix <- function(x = matrix()) {
  
  ## cachedMatrix will hold the cached matrix
  cachedMatrix <- NULL
  
  ## function to set matrix to be cached
  set <- function(y) {
      x <<- y
      ## reset cachedMatrix to NULL to indicate not cached
      cachedMatrix <- NULL  
  }
  
  ## get the matrix to be cached
  get <- function() x
  
  ## cache the matrix
  setCache <- function(toCache) cachedMatrix <<- toCache 

  ## get the cached matrix
  getCache <- function() cachedMatrix
  
  ## list of functions in makeCacheMatrix
  list(set = set, get = get, setCache = setCache, getCache = getCache)
  
}


## cacheSolve checks to see if the inverse of matrix x has already been cached.  If so, 
## it returns the cached value.  If not, it solves the inversion, sets it to cache, and 
## returns the solution.
cacheSolve <- function(x, ...) {
        
  ## get the current cached matrix to see if null
  inversion <- x$getCache()
  
  ## if the inverse has already been cached return the cached inverse
  if(!is.null(inversion)) {
      message("getting cached data")
      return(inversion)
  }
  
  ## get the matrix for inversion
  inMatrix <- x$get()
  
  ## solve for the inverse
  inversion <- solve(inMatrix)
  
  ## set cache
  x$setCache(inversion)
  
  ## return the solution
  inversion
}
