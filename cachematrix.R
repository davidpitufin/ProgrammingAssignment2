## These two functions cache the inverse of a matrix
## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize inverse matrix variable
	m <- NULL
	## Set: this function sets the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## Get: this function returns the value of the matrix
	get <- function() x
	
	## Setinv: this function sets the value of the inverse matrix
	setinv <- function(inv) m <<- inv
	
	## Getinv: this function returns the value of the inverse matrix
	getinv <- function() m
	
	## Returns cached matrix as a list
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	## Get cached inverse matrix variable
	m <- x$getinv()
	## Check if inverse matrix has been previously computed
	if(!is.null(m)) {
		## If it has been, return it
		message("getting cached data")
		return(m)
	}
	## If inverse matrix has not been computed, compute it now
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}