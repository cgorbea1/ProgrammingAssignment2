+## Put comments here that give an overall description of what your
+## functions do

+## Write a short comment describing this function

+## This function creates a special "matrix" object that can cache its inverse.
+## It supports setting a matrix, getting a matrix, setting an inverse and getting an inverse.

makeCacheMatrix <- function(x = matrix()) {                             ## define the argument
 +	inv <- NULL                                                     ## initialize inv as NULL
 +	set <- function(y) {                                            ## define the set function
 +		x <<- y                                                 ## value of matrix in parent environment
 +		inv <<- NULL                                            ## reset inv to NULL if there is a new matrix
 +	}
        
 +	get <- function() x                                             ## define the fucntion: returns value of the argument
 +	setinv <- function(invers) inv <<- invers                       ## assigns value of inv in parent environment
 +	getinv <- function() inv                                        ## gets the value of inv
 +	list(set = set, get = get, setinv = setinv, getinv = getinv)    ## this is needed in order to refer to the $ operator
                                                                        ## functions
 +}

+## Write a short comment describing this function

+## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
+## If the inverse has already been calculated (and the matrix has not changed), then the
 +## cachesolve will retrieve the inverse from the cache.
 +
 +cacheSolve <- function(x, ...) {
 +    ## Return a matrix that is the inverse of 'x'
 +	inv <- x$getinv()
 +	if(!is.null(inv)) {
 +		message("getting cached data")
 +		return(inv)
 +	}
 +	data <- x$get()
 +	inv <- solve(data)
 +	x$setinv(inv)
 +	inv
 +}
