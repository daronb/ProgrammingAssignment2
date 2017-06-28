## To save time when inverting matrices, it can be usefull to cache the result rather than calculating repeatedly.
## These two functions will cache the inverse of a matrix.

## creates cached matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)

}


## this function returns the inverse of a matrix:
##
##1. checking if the inverse had been calculated previously, if it has it returns the answer and skips the calculation
##2. if the inverse has not been calculated before, then it calculates it and caches the answer

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<- solve(data,...)
        x$setinv(inv)
        inv
}
        ## Return a matrix that is the inverse of 'x'
}
