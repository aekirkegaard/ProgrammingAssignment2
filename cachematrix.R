## These functions will work together to cache the inverse
## matrix rather than haing to compute it repeatedly. 


## This function creates a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		m <<-- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse 
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the matrix object returned by the makeCacheMatrix.
## If the inverse has already been calculate and the matrix has not changed, then this 
## function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
