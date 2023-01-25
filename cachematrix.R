## The 'makeCacheMatrix' function creates object that has the matrix passed in,
## the inverse of the matrix (defaulted to NULL), and the setter and retriever
## functions for the matrix and inverse.

## The 'cacheSolve' function takes an object created by the 'makeCacheMatrix'
## function and does the work of either pulling an already calculated inverse
## from memory, or calculating and storing the inverse in memory.


## makeCacheMatrix take a matrix, if one isn't provided it defaults to a 1x1
## matrix with a value of NA.  It returns a named list with each name holding/
## set to a function.
makeCacheMatrix <- function(mtrx = matrix()){
    slv <- NULL
    
    set <- function(y) {
        ## set global (variable not defined in this function) mtrx, and slv.
        ## mtrx is set to the matrix passed in via the y argument.
        ## slv is set to NULL
        ## technically this function returns slv as it's the last statement
        ## performed.  However, since this function sets global variables
        ## what it returns doesn't really matter in this instance.
        mtrx <<- y    
        slv <<- NULL
    }
    
    get <- function() mtrx  ## returns the global (variable not defined in this
    ## function) mtrx variable
    
    setsolve <- function(solve) slv <<- solve
    ## set global (variable not defined in this function) slv variable to
    ## solve that was passed in.  Technically the function also returns
    ## slv, but since this is a global what the function returns doesn't
    ## really matter in this instance.
    
    getsolve <- function() slv ## returns the global (variable not defined in
    ## this function) slv variable
    
    list( set = set,
          get = get,
          setsolve = setsolve,
          getsolve = getsolve)
    ## return a list of name value pair where name, in this case, is the 
    ## function name built above and the value is the variable holding the
    ## function built above.
} 


## cacheSolve takes a 'cacheMatrixObject', created by using the makeCacheMaxtrix
## function.  It also accepts optional arguments in the ...; these optional
## arguments will be passed/used by the solve function.

## Using the 'cacheMaxtrixObject' passed in, it checks to see if the inverse for
## the matrix has been calculated.  If it has it pulls the inverse from cache/
## memory instead of doing the calculation.  If it has not been calculated,
## then it will calculate the inverse and cache/store the inverse

## Whether calculated or pulled from the cache/memory, it will return the 
## inverse of the 'cacheMatrixObject'

cacheSolve <- function(cacheMatrixObject, ...) {

    ## retrieves the inverse for the cacheMaxtrixObject   
    inv <- cacheMatrixObject$getsolve()  
    
    ## if the inverse retrieved isn't null it has already been calculated and
    ## that will be returned.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## we don't have the inverse in cache/memory
    
    ## get our matrix data we want to perform the inverse on.
    data <- cacheMatrixObject$get()
    
    ## call the solve function with matrix data we retrieved, and any
    ## additional arguments that were passed in for use in the solve function.
    inv <- solve(data, ...)
    
    ## cache, or store in memory, the inverse calculated for this matrix.
    cacheMatrixObject$setsolve(inv)
    
    ## return the inverse
    inv
}