## CacheMatrix is a data structure that allows for caching of the result of the inverse operation


#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
       
}

# testing function. The second call to cacheSolve returns a cached result and the
# 'getting cached data' message

test <- function (){
  y<- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
  yCache<-makeCacheMatrix(y)
  cacheSolve(yCache)
  cacheSolve(yCache)
  i<-yCache$getinverse()
  i 
}
