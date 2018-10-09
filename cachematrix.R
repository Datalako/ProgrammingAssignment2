#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix){
  inv<-NULL # create empty inv
  set<-function(y){
    x <<- y
    m <<- NULL
   }
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, 
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve<-function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
