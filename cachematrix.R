##########################################################################################################################
## The functions in the file are used to compute the inverse of a matrix and cache it.                                  ##
## Once computed, the inverted matrix can be retrieved from the cache to reduce the computation time.                   ##
## Example of usage:                                                                                                    ##
##      amatrix = makeCacheMatrix(matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3))                                         ##
##      cacheSolve(amatrix)                                                                                             ##
## Result:                                                                                                              ##
##           [,1] [,2] [,3]                                                                                             ##
##      [1,]    7   -3   -3                                                                                             ##
##      [2,]   -1    1    0                                                                                             ##
##      [3,]   -1    0    1                                                                                             ##
##                                                                                                                      ##
## To subsequently access the inverse of the matrix from the cache call:                                                ##
## amatrix$getinverse()                                                                                                 ##
##########################################################################################################################


## makeCacheMatrix creates an object that encapsulate the original matrix(x) and the cache of the matrix's inverse(m)
## There are 4 functions defined for the object
## get() : will return the original matrix(x)
## Set(y) : will override the values of matrix(x) with the new matrix (y) and reset the cache to NULL
## getinverse(): will return the inverse of the original matrix from the cache (m).
## note: you must first call cacheSolve() once inorder to update the cache with the inverse of the matrix
## setinverse(): will set the cache with a given value. The function assumes the value provided is the inverse of the original matrix(x)
## note: this function is called by cacheSolve(). It is not recommended to call it directly 

makeCacheMatrix <- function(x = matrix()) {
        # create the cache and set it to NULL
        m <- NULL
        # update the matrix with new values
        set <- function(y) {
                x <<- y
                # reset the cache defined in the parent makeCacheMatrix
                m <<- NULL
        }
        # return the original matrix x that was passed during the object creation or updated using the set()
        get <- function() x
        # update the cache (m) defined in the parent makeCacheMatrix with the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        ## get the inverse matrix from the cache
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       
  
}

## cacheSolve() computes the inverse of the matrix object created by makeCacheMatrix and updates the cache. 
## If the inverse has already been calculated and is found in the cache, it will retrieve the inverse from the cache.
## Returned values:
##      The inverse of the original matrix encapsulated in x
## important notes:
##      * The function expects a matrix object created by using makeCacheMatrix
##      * The function assumes the original matrix in x is invertible 
##      Calling the function on a non invertible matrix will return error of the form: 
##      Lapack routine dgesv: system is exactly singular: U[3,3] = 0 

cacheSolve <- function(x, ...) {
        ## first check if the inverse is cached
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                ## The cache is not empty therefore returning the cached inverse
                return(m)
        }
        # get the original matrix
        data <- x$get()
        ## inverse the matrix 
        m <- solve(data, ...)
        ## set the cache of x
        x$setinverse(m)
        ## Returns the matrix that is the inverse of the original matrix in 'x'
        m
}
