## Matrix inversion is usually a costly computation and there are benefits 
## to caching the inverse rather than computing it repeatedly. 
## The following pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" that can cache its inverse.
makeCacheMatrix <- function(data = matrix()) 
{
    cache <- NULL
    
    set <- function(value) 
    {
        data  <<- value
        cache <<- NULL
    }
    get <- function() { return(data) }
    
    setCache <- function(value) { cache <<- value }
    getCache <- function()      { return(cache)   }
    
    return(list(
         set      = set
        ,get      = get
        ,setCache = setCache
        ,getCache = getCache))  
}

## This function computes the inverse of the special "matrix" returned 
## from makeCacheMatrix.
cacheSolve <- function(x, ...) 
{
    ## If the value has not been cached then calculate the inverse and
    ## set it as the new cache. The "dim" function returns a vector when 
    ## given a matrix and returns NULL when given a NULL object. 
    if (is.null(dim(x$getCache())))
    {
        x$setCache(solve(x$get(), ...))
    }
    ## Write a message if the value has been previously cached.
    ## The last line of the function will return the cache so there is
    ## no need to return it here.
    else
    {
        message("Getting cached data")
    }    
    
    return(x$getCache())
}
