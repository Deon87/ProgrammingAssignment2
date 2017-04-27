          ## The R code is written by Gideon Obeisun ##  
          

## This function creates a special matrix object with 2X2 dimension. ##

makeMatrix <- function(x = matrix(, nrow=2, ncol=2)) {
        m<-matrix(, nrow=2,ncol=2)
        m <- NULL
        set <- function(y=matrix(,nrow=2,ncol=2)) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of a 2X2 matrix created by the ##
## makeMatrix function above. If the inverse of the matrix is already ##
## calculated, the cacheMatrix will try to retrieve the result from the cache. ##

cacheMatrix <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

m_atrix<- makeMatrix(matrix(c(1,2,3,4), nrow=2,ncol=2))
m_atrix
c_m_atrix<-cacheMatrix(m_atrix)
c_m_atrix

