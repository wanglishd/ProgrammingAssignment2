## Put comments here that give an overall description of what your
## functions do

## These two functions are used to create a special object that stores 
## a matrix and caches its inverse.

## Write a short comment describing this function

## This function creates a matrix to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the vaule of the inverse of the matrix
## 4.get the vaule of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set,get=get,
             setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed
##), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
