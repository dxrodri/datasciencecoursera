## These functions calculates inverse of a matrix. Once inverse is calculated, the value is saved in the cache as part of the special matrix object and retrieved on subsubsequent invocation of the special matrix. 

## This function creates a special matrix (associated with functions) for a given matrix object

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This function calculates the inverse of the special matrix and saves the value in the cache for subsequent invocation of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}

##Unit test code

#seq1 <- seq(1:4)
#mat1 <- matrix(seq1, 2,2)
#mat2 = makeCacheMatrix(mat1)
#mat3 = cacheSolve(mat2)

##The following should return a unit/identify matrix
#mat1 %*% mat3
