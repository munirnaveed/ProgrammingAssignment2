#Author: Munir Naveed
#Acknowledgement : code is a modification of the example given on R programming course 
#for Assignment 2

#Code in this file is based on two functions
#1. makeCacheMatrix: creates a cache object m of a matrix x

#2. cacheSolve:  gets the m object of the x matrix, calculates inverse if m has not been calculated previously


#however, there is one check on solve as well, becuase I got the errors without a check
# if we do not put checks, the program was giving error me on matrix(1:9, 3, 3)
# and matrix(1:10, 2, 5)

# The validation is here becuase I read the assumption on the assignment page after modifying the code-- assumption
# that matrix is always invertible. anyway, it was fun to do more programming 


#----- The code starts from the following line-----------------------------------------

makeCacheMatrix <- function(x = matrix())  {
        m <- NULL
        set <- function(y) {
                x <<- y           
                m <<- NULL
        }
        get <- function() x
        
        #Set Cache Value
        setinv <- function(inv) m <<- inv
        #Get Cache value
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv= getinv)
}

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
                     
        data <- x$get()
       #get dimensions of Matrix
        d<-dim(data)
        mDet<-det(data) 
        if(d[1]==d[2] & mDet!=0) # if it is a square matrx and not a singular then calculate inverse 
         {
            m <- solve(data, ...)
            x$setinv(m)
            return(m)
        }
        else
         {     if(mDet==0){
                             message("Matrix must a non Singular one")

                           }
                           else
                            {
                              message("Matrix must be Square")
                            }   
          } 
}