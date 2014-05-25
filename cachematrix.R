## Description
## ===========
##
## This module contains functions `makeCacheMatrix` and `cacheSolve` which can be used
## to eliminate repetitive calculations of inverse matrix.
##
## Usage:
## ======
##
## cm <- makeCacheMatrix(x = matrix(c(-1,5,-1,0,1,8,0,0,1),3,3) )
## cacheSolve(cm)               ## perform actual calculation and store the result
## cacheSolve(cm) %*% cm$get()  ## use cached result to perform calculation
##
## ---


## `makeCacheMatrix` function creates a special "matrix" object that can
## cache its inverse matrix.
##
## Arguments
## =========
##
## x    matrix
## 
## Public methods
## =======
##
## set(x)  set matrix value and clear cached inverse matrix if it exists
## get()   get matrix value
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## setter method to store matrix and clear inverse martix value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## getter for matrix
    get <- function() x
    
    ## setter for inverse matrix
    setInverse <- function(inverse) i <<- inverse
    
    ## getter for inverse matrix
    getInverse <- function() i
    
    ## return list with setter and getter methods
    ## type is used to distinguish special "matrix" object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         type = "CacheMatrix")
    
}


## `cacheSolve` function takes special "matrix" object created by `makeCacheMatrix` and calculates inverse matrix.
## In case the input object contains cached inverse matrix the function returns it,
## otherwise the function performs actual calculation, caches the result and returns inverse matrix.
## Actual calculation is performed by `solve` function from `base` package.
## Additional arguments to `solve` function might be passed by ... parameter.

## Arguments
## =========
## 
## x    special "matrix" object
## ...  parameters passed to solve function
cacheSolve <- function(x, ...) {

    ## check if x is special "matrix" object
    if (!is.list(x) || is.null(x$type)) 
        type <- "Wrong object type"
    else
        type <- x$type
    
    if ( type != "CacheMatrix")
        stop("Wrong argument type - x must be special \"matrix\" object created by makeCacheMatrix function.")
    
    i <- x$getInverse()
    ## if inverse matrix is already stored in x object return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## otherwise calculate inverse matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i  
}
