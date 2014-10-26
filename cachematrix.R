
## Ali:
## Following set of functions will help to reduce
## invert matrix calculation times bu caching result and reusing it
## I did the same style as the example
## however I think it can be written better
## e.g. Do set also when defining by makeCacheMatrix


## This will initialize the object including 4 functions
## It can set a matrix to its internal environment
## , set its inverse, and return either of them

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) I <<- Inv
    getInv <- function() I
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Following function will be used when we need 
## to have inverse of our initialized matrix.
## If it is the first time call, inverse will be calculated
## otherwise only the result of last calculation will be returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getInv()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInv(I)
    I
}
