## The two functions makeCacheMatrix and cacheSolve cache the inverse of
## a matrix rather than solve it multiple times in a single script.
## The inverse of a matrix is computed once and cached.
## Until the matrix is mutating throughout the script we retrieve the cached value
## of the computed inverse.
## In case the matrix is mutates, we recompute the inverse and cache it.
## WARNING! - These functions assume that the matrix supplied is invertible

## makeCacheMatrix creates a special type of matrix which is a list of 4 functions:
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
    ## creates a empty local variable
    imat <- matrix(nrow=0, ncol=0)
    
    ## sets the value of the matrix with the data passed as argument
    setMatrix <- function(vmat)
    {
        ## vmat is a vector of numbers and we assign the vector to the formal argument matrix 'x'
        ## sqrt since the number of elements in matrix would be the square of the nrow/ncol
        x <<- matrix(vmat, sqrt(length(vmat)), sqrt(length(vmat)))
        
        ## to set the value of the cache inverse variable to null, since it we are caching
        ## we need to use an environment different from the parent
        imat <<- matrix(nrow=0, ncol=0)
    }
    
    ## returns the value of the matrix
    getMatrix <- function() x
    
    ## sets the value of the inverse of the matrix to the local variable 'imat'
    ## rather caches it to be retrieved later
    setInverse <- function(Inverse) imat <<- Inverse
    
    ## retrieves the value of the inverse matrix which was computed and cached earlier in 'imat'
    getInverse <- function() imat
    
    ## returns a list of functions we can call with
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function either computes the inverse in case the matrix mutates/not yet computed
## or returns the cached inverse.

cacheSolve <- function(x, ...)
{
    ## getting the cached inverse in the matrix created(if already computed)
    imat <- x$getInverse()
    
    ## checks for the conditions: 
    ## 1) if inverse is already computed and stored
    ## 2) if the matrix is changed
    ## Executes the code in the braces the condition 1 is true i.e inverse is stored and 
    ## condition 2 is also true i.e matrix is not changed
    if (!is.null(imat) & identical(x, x$getMatrix()))
    {
        ## returns the cached inverse matrix along with a message
        message("Returning Cached Data")
        return(imat)
    }
    
    ## checks if the original matrix is changed and updates it
    if (!identical(x, x$getMatrix()))
    {
        ## set the matrix to the new matrix i.e the changed one, array function is used since our setMatrix funtion takes a vector
        x$setMatrix(array(x))
    }
    
    ## get the matrix from the cache which is updated(in case it mutated)
    dataMatrix <- x$getMatrix()
    
    ## compute the inverse of the matrix
    imat <- solve(dataMatrix)
    
    ## cache the inverse of the matrix
    x$setInverse(imat)
    
    ## return the inverse matrix
    imat
}