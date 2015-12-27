## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will create a list containing 4 functions
## set, get, setmatrix, and getmatrix

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

## Write a short comment describing this function
## cacheSolve will get a matrix x and test whether the matrix is in the cache.
## if yes, cacheSolve function will get the values from cache, otherwise compute inverse using solve function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}