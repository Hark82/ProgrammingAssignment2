## The functions below are called makeCacheMatrix and cacheSolve.
## The first function, makeCacheMatrix(), will be used to make cache available. In this case to cache a calculated inverse matrix.
## The second function, cacheSolve(), is used to calculate the inverse matrix which is set in the makeCacheMatrix() function.


## testing
X <- makeCacheMatrix()
X$set(matrix(1:4, 2, 2)) ## set matrix
X$get() ## returns matrix 1:9, 3, 3
X$getinverse() ## returns null as no inverse is calculated yet
cacheSolve(X) ## returns inverse matrix
X$getinverse() ## returns inverse matrix as this is now cached
cacheSolve(X) ## returns "getting cached data" and inverse matrix (cached)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		## set inverse = NULL
		i <- NULL
		
		## set function
		set <- function(y) {
			## assign the set arguments to x
			x <<- y
			
			## when set is called, set inverse to null as this will be a new matrix
			i <<- NULL
		}
		
		## return the matrix that is set
		get <- function() x
		
		## set the calculated inverse (called via cacheSolve)
		setinverse <- function(solve) i <<- solve
		
		## return the inverse matrix
		getinverse <- function() i
		
		## create list of the functions
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get inverse matrix, will return null if no inverse is set
		i <- x$getinverse()
		
		## Check if i (already calculated inverse) is not null
		if(!is.null(i)) {
			## show message that cached data is available and will be used
			message("getting cached data")
			
			## return cached value
			return(i)
		}
		
		## get matrix
		data <- x$get()
		
		## calculate inverse
		i <- solve(data, ...)
		
		## set inverse that is calculated
		x$setinverse(i)
		
		## return inverse value
		i
}
