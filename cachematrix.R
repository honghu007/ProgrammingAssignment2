## makeCacheMatrix is a function creating a special "matrix" object that can 
## cache its inverse, and returns a list of four functions
   ## setMatrix        set the value of a matrix
   ## getMatrix        get the value of a matrix
   ## setInvermatrix   set the inverse of the matrix
   ## getInvermatrix   get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        
        ## initially set the inverse to NULL
        inverse <- NULL
        
        ## set a new matrix y to x
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## returns the matrix x
        getMatrix <- function() {
                x
        }
        
        ## set the inverse of the matrix 
        setInvermatrix <- function(invermatrix) {
                inverse <<- invermatrix
        }
        
        ## get the inverse of the matrix 
        getInvermatrix <- function() {
                inverse
        }
        
        ## return a list of the functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInvermatrix = setInvermatrix, 
             getInvermatrix = getInvermatrix)
}



## cacheSolve is a function calculating the inverse of a "special" matrix created 
## with makeCacheMatrix, and returns the inverse of the matrix.  
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache. If not, cacheSolve 
## will calculate the inverse ,and set the value to the cache via setInvermatrix.


cacheSolve <- function(x, ...) {
        
        ## get the incerse of matrix x 
        inverse <- x$getInvermatrix()
        
        ## if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## otherwise get the matrix
        data <- x$getMatrix()
        
        ## caclulate the inverse matrix
        inverse <- solve(data)
        
        ## set inverse matrix to the cache
        x$setInvermatrix(inverse)
        
        ## return the inverse matrix
        inverse
}

