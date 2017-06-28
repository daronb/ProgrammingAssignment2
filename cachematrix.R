## create matrix that can cache its inverse

## Write a short comment describing this function

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


## if the matrix inverse has been solved previously then given from cache else inverse is calculated and cached

cacheSolve <- function(x, ...) {inv<-x$getinv()
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

