## Put comments here that give an overall description of what your
## functions do

#Creates a class, like a list
#Contains four functions
#set stores matrix in cache and get recalls
#setinverse/getinverse is the same but for original matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL 
  #z matrix value
  set <- function(y) { 
    x <<- y 
    z <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(inverse) z <<- inverse 
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}

#The inverse is only solved once; if it has already been calculated, 'cacheSolve' is used.
#cacheSolve takes the matrix and checks if it is solved, if so, it recalls from cache
#if not, it will calculate and store in cache.

cacheSolve <- function(x, ...) {
  #Matrix that is inverse to z 
  z <- x$getInverse() 
  if(!is.null(z)) { 
    message("getting cached data") 
    return(z) #checks z matrix cache's and if true, returns 
  } 
  data <- x$get() 
  z <- solve(data, ...) 
  x$setInverse(z) 
  z 
  #if not found, it is calculated
}