## Put comments here that give an overall description of what your
## functions do

## The code below contains solution of Project 2 R Programming by JHU.
## It is a pair of FUNCs that cache the inv of a matrix.

## Write a short comment describing this function

## Creates a special matrix object that can cache its inv.
makeCacheMatrix <- function( m = matrix() ) {
        ## Initialize the inv.
        i <- NULL
        
        ## Set the matrix.
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        ## Get the matrix.
        get <- function() m
        
        ## Set the inv of the matrix.
        setInv <- function(inverse) i <<- inverse
        
        ## Get the inv of the matrix.
        getInv <- function() i
        
        ## Return the list of methods.
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

## Compute the inv of the special matrix returned by "makeCacheMatrix"
## above. If the inv has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inv from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        
        ## Return the inv matrix.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix from the object.
        data <- x$get()
        
        ## Calculate the inv matrix using multiplication.
        m <- solve(data) %*% data
        
        ## Set the matrix.
        x$setInv(m)
        
        ## Return the matrix.
        m
}
