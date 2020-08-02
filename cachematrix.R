## The following function calculates the inverse of the special "matrix" created with the previous function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setInversa function.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() {x}
        setInversa<-function(inversa) (inv<<-inversa)
        getInversa<-function() {inv}
        list(set=set,get=get,setInversa=setInversa,getInversa=getInversa)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInversa()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInversa(inv)
        inv
}
