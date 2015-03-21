
## the function makeCacheMatrix() can return a list of some function
## about how to cache a matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function(y){
					x <<- y
					m <<- NULL
			}
			
			get <- function() x
			set <- function() x
			setinverse <- function(solve) m <<- solve
			getinverse <- function() m
			list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}

## cacheSolve() can return the cache of special matrix 'x'. if there
## is no cache, it can calculate the solve of matrix 'x', and return
## the result to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)){
				message("getting cacheDate")
				return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinverse(m)
		m
}
