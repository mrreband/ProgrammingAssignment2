## These functions calculate the inverse of a matrix using the solve() function and store the inverse in cache
## if the inverse has already been previously calculated, it is retrieved from the cache instead of computing again

## makeCacheMatrix creates a list containing a function to set and get the matrix, set and get the inverse
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


#cacheSolve() looks for the inverse in cache: if it exists, return it; if it doesn't, compute using solve() and cache for future use
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        ## Retrieve from cache if it has been computed previously
        message("getting cached data")
        return(m)
    }

    ## compute the inverse if it does not exist in cache
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
