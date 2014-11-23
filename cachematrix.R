## makeCacheMatrix
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. gets the value of the inverse matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# initialize
	m <- NULL
	
	# set value
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	# get value
	get <- function() x
	
	# set inverse
	setinverse <- function(solve) m <<- solve
	
	# get inverse
	getinverse <- function() m
	
	# return list
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the
## cache. 

cacheSolve <- function(x, ...) {
	# get inverse
	m <- x$getinverse()
	
	# check if cached. return cached
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# get matrix
	data <- x$get()
	
	# compute matrix
	m <- solve(data, ...)
	
	# cache inverse
	x$setinverse(m)
	
	# return inverse
	m
}
