## This function creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  getmatrixvalue <- function() x
  setcachedinverse <- function(inverse) cachedinverse <<- inverse
  getcachedinverse <- function() cachedinverse
  list(getmatrixvalue = getmatrixvalue, setcachedinverse = setcachedinverse, getcachedinverse = getcachedinverse)
}


## This function returns the cached inverse matrix from a
## makeCacheMatrix object, if a cached value exists; otherwise,
## an inverse matrix is created and stored in the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getcachedinverse()
  if(!is.null(cache)) {
    print("Cached data:")
    return(cache)
  }
  matrix <- x$getmatrixvalue()
  cache <- solve(matrix, ...)
  x$setcachedinverse(cache)
  cache
}
