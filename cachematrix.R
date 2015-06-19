##Below shown is a pair of functions that cache the inverse of the matrix.This may be helpful for a very large matrix
##in which calculating every time the inverse, will be time consuming. Hence the function will take the inverse from
##cached data rather than computed it repeatedly, if it was cached before.


## The first function makeCacheMatrix creates a special matrix. It's a list for housing the functions
## to set the matrix, get the matrix, set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
        {
                inv <-NULL
                setmatrix<-function(y)
                        {
                                x<<-y
                                inv<<-NULL
                        }
                
                getmatrix<-function()x
                setinverse<-function(inverse) inv<<-inverse
                getinverse<-function()inv
                list(setmatrix=setmatrix, getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)

        }


## cacheSolve function will first check the inverse of specified matrix from the cached data and return
##if its available or else it will compute the inverse, store it and return the inverse.

cacheSolve <- function(x, ...) 
        {
                
        inv<-x$getinverse()
                if(!is.null(inv))
                        {
                                return(inv)
                        
                        }
        
        matrix<-x$getmatrix()
        inv<-solve(matrix, ...)                 ## Return a matrix that is the inverse of 'x'
        x$setinverse(inv)
        inv
        
        }
