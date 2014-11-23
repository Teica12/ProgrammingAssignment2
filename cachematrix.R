##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
 mat<-NULL
  set<-function(y) {
    x <<- y
    mat <<- NULL
  }
  get<-function() x
  setinverse<-function(inverse) mat <<- inverse
  getinverse<-function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##The following function calculates the inverse matrix of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
##via the setinverse function.

cacheSolve <- function(x, ...) {
        mat<-x$getinverse()
  if(!is.null(mat)) {
    message ("getting cached data")
    return(mat)
  }
  data<-x$get()
  mat<-solve(data, ...)
  x$setinverse(mat)
  mat
}
