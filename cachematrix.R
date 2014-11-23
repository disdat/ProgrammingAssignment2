## makeCacheMatrix and cacheSolve functions provide ability to caclulate inverse of a matrix once, save it in its cache
## and return previously calculated inverse for that matrix without having to recaclulate it
## Note: The order in which these functions must be run is critical
## Step 1 - create your matrix whose inverse you want to caculate
## Step 2 - call makeCacheMatrix with the matrix from step 1 in the argument.  Please note, this function returns a list NOT a matrix
## Step 3 - call cacheSolve with list from step 2 as the argument
##
## Example.
## a<-(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3) #create a 3x3 matrix
## a
##       [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
## b<-makeCacheMatrix(a)  #b is a list with four functions as members
## c<-cacheSolve(b)
## c
##          [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
##
## d<-cacheSolve(b)
## returning cached inverse #finds that b contains inverse and returns the pre-calculated inverse matrix
## d
##          [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625

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
    get <- function() x   #return the original matrix
    setInverse <- function(inverse) inverseMatrix <<- inverse  #allows the caller to set the inverse which is calculated and passed by the caller
    getInverse <- function() inverseMatrix      #return the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function calculates inverse of a matrix and saves it in a list created using makeCacheMatrix 
## it first tries to get the inverse from the cache
## if the cache is empty, it calculates the inverse and saves it in the cache for future use

cacheSolve <- function(x,...) {    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse() 
    if(!is.null(inverseMatrix)) {
        message("returning cached inverse")     #found the inverse
        return(inverseMatrix)
    }
    data <- x$get()  #code beyond this line will only be executed if the inverse wasn't found in the previous step
    inverseMatrix <- solve(data,...) #calculate inverse
    x$setInverse(inverseMatrix)  #save it in the cache
    inverseMatrix 
}