## This program cache the inverse of a matrix in order to save 
## the resource to recaluate it again if the cache is still 
## available and input matrix is not changed.



makeCacheMatrix <- function(x = matrix()) {
  	## makeCacheMatrix function creates a special "matrix" object 
  	## that can cache its inverse. It takes a numeric matrix.

  	## initial  variable inv
  	inv <- NULL  

  	set <- function(y) {
    		x <<- y
      		inv <<- NULL
  	}
  	get <- function(){
      		return(x)
  	} 
  	setinverse <- function(inputReverse){
      		inv <<- inputReverse
  	}
  	getinverse <- function() {
      		return(inv)
  	} 
  	list(set = set, get = get,
      	     setinverse = setinverse,
      	     getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already been calculated (and the 
        ## matrix has not changed),then the cachesolve 
        ## should retrieve the inverse from the cache.

   	inv <- x$getinverse()
  
  	# If the calculated inverse is available, get the cached value to avoid recomputing
        # version of of the mean
        if(!is.null(inv)) {
      		message("getting cached data")
      		return(inv)  ## function return cached inv  
        }
        data <- x$get()  ## call get() to get the underlying matrix
 
        inv <- solve(data, ...)  ## solve(X) returns its inverse and assign to inv
 
      
        x$setinverse(inv)  ## the inverse in x so we cache it and no need to recompute it
  
  
        inv  ## function returns calculated inv
}
