## makeCacheMatrix and cacheSolve functions provide ability to caclulate inverse of a matrix once, save it in its cache
## and return previously calculated inverse for that matrix without having to recaclulate it

## makeCacheMatrix function saves and returns the matrix whose inverse needs to be calculated
## In addition, it also allows save and return of its inverse
## Returns a list whose members are four functions
## set matrix
## get matrix
## setInverse
## getInverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL #initialize inverse of the matrix to null  
    set <- function(y) {
        x <<- y           #save the original matrix
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse  
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function calculates inverse of a matrix and saves it in a data structure created using makeCacheMatrix 
## it first tries to get the inverse from the cache
## if the cache is empty, it calculates the inverse and saves it in the cache for future use

cacheSolve <- function(x,...) {    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse() 
    if(!is.null(inverseMatrix)) {
        message("returning cached inverse")
        return(inverseMatrix)
    }
    data <- x$get()  #if here, it means cache was empty, so create inverse
    inverseMatrix <- solve(data,...)
    x$setInverse(inverseMatrix)  #save it in cache
    inverseMatrix 
}