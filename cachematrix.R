makeCacheMatrix <- function(x = matrix()) {
    
    ##initially m is empty
    m   <- NULL
    
    ##set the matrix, invalidates the cached content
    set <- function(y){
        x  <<- y
        m  <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    
    ## Return a list with the routines for setting/getting content and
    ## cached matrix inverse.
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    
    ##calculate the inverse
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
