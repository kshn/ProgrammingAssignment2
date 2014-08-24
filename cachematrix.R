## These functions are used to get the inverse of a given matrix. It makes use
## of caching to avoid repetitive, time-consuming operations. If the inverse
## has previously been computed, it can be looked up from the cache; else, the 
## inverse is computed and then stored in the cache.

## makeCacheMatrix returns a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function( y ) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <- function( inverse ) inv <<- inverse
    getinv <- function() inv
    list( set = set, get = get, setinv = setinv, getinv = getinv )

}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if( !is.null( inv ) ){ 
        message( "getting cached matrix inverse " ) 
        return( inv )
    }
    mat <- x$get()
    inv <- solve( mat )
    x$setinv( inv )
    inv
}
