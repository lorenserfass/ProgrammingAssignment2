# These functions use R's lexical scoping to implement
# a matrix-like object capable of caching its inverse.

# makeCacheMatrix creates a "matrix" but really
# returns a list of functions.  The matrix and its inverse
# are stored in the environment of the function.
# Here are the functions that are returned:
# The set function sets the value of a matrix.
# The get function returns the value of the matrix.
# The setInv function sets the value of the inverse matrix.
# The getInv function returns the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function
# This function returns the inverse matrix of x.
# It first checks whether the inverse has already been cached.
# If not, it calculates that inverse, caches it, then returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached matrix inverse")
		return(inv)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
}
