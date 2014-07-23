## Put comments here that give an overall description of what your
## functions do
##The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse,
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix


## Write a short comment describing this function 
##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
##from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data and sets the value of the inverse in the cache via the 
##setinverse function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver<<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
        ## Return a matrix that is the inverse of 'x'
}
