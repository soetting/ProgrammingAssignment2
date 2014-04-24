## This code will allow the cached storage of an inverse of a given matrix.
## The main objective of doing this is to be able to access the inverse
## matrix quickly and efficiently multiple times by storing the inverse value
## as opposed to recalculating it each time (say, for example, in the
## running of a for or while loop.)

## The makeCacheMatrix function creates a special 'matrix' which is a list
## containing functions to (1) set the value of the matrix, (2) get the value
## of the matrix, (3) set the value of the inversed matrix, and (4) get the 
## value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinvMatrix <- function(invMatrix) m<<-invMatrix
        getinvMatrix <- function() m
        list(set = set, get = get, 
            setinvMatrix = setinvMatrix, getinvMatrix = getinvMatrix)
}


## The cacheSolve function will return the value of the inversed matrix by
## first determining if the inversed value has already been solved and, only
## if it has not, then go through the process of calculating it using the 
## solve(). 
## Note: This function presumes that the original matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x$getinvMatrix()

        ## Determine if the inverse matrix has already been solved and stored.
        ## If so, return it.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
        ## Otherwise, get the matrix, solve the inverse, store it for future
        ## use, and return the inverse matrix
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinvMatrix(m)
    m
}
