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
