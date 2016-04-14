## Two functions calculate and cashe inverse matrix.
## 

## 
# function makeCacheMatrix sets the list of functions able to: 
#	1. set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the solution (inverse matrix)
#	4. get the value of the solution
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolution <- function(solution) m <<- solution
        getsolution <- function() m
        list(set = set, get = get,
             setsolution = setsolution,
             getsolution = getsolution)
}


## Write a short comment describing this function
#	The cacheSolve calculates the solution of the list
#	created with the makeCacheMatrix. 
#	If the solution has already been calculated. If so, it gets 
#	the solution from the cache and skips the computation. 
#	Otherwise, it calculates the inverse matrix and sets the value of 
#	the solution in the cache via the setsolution function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getsolution()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolution(m)
        m
}

