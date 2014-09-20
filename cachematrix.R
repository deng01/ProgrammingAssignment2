## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve attempt to cache the potentially time consuming
## operation of inverting a matrix. If the contents of the matrix are not changing,
## it makes sense to cache the value of the inverse so that it can be looked up
## in the cache instead of recomputed.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setmean = setmean,
		getmean = getmean) 
}


## Write a short comment describing this function

## cacheSolve calculates the special "matrix" created with the above function
## It checks to see if the inverse has already been calculated. If it is,
## it gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
    	message("getting cached data")
    	return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
