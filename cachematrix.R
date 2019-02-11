
##       Inverse of a Matrix:
##       Step 1. Calculating the Matrix of Minors
##       Step 2. Matrix of Cofactors
##       Step 3. Calculating the Adjugate and multiply by 1/ Dterminant
##       Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
##       than computing it repeatedly . My assignment is to write a pair of functions that cache the inverse of a matrix.

##       This function makeCacheMatrix helps to create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(w) {
                x <<- w
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse<<- inv
        getinv <- function() inverse
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

##       This function is used for compute the inverse of a matrix(a matrix that is created thanks to the function  makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
}

##      This is how these functions worked.
m <- makeCacheMatrix(matrix(c(12,121,312342,1231), 2, 2))
cacheSolve(m)
 
