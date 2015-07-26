## functions for programming assignment 2 by Jeffery Smith
## the functions create tools for calculating the inverse of a matrix and saving the inverse in a cahce

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   #requires object of square matrix
      m<-NULL                       #clears the dataset m
      set <-function(y) {      #needed if someone wants to change the matrix without rerunning makecachematrix
            x<<- y                  #sets the x values in parent environment
            m<<-NULL                #sets value of m to NULL in parent env
      }
      get<-function() x  #creates matrix from the vallues input when calling makeCacheMatrix(since is square, nrow and ncol can use r)
      setinv<-function(zzz) m<<-zzz   #sets value of m in current function from zzz in parent env
      getinv<-function() m          #pulls value of m in current function
      list(set=set,               # creates a list of the functions created by function makeCacheMatrix
            get=get,
            setinv=setinv,
            getinv=getinv)     

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m<- x$getinv()          # grabs m from the makeCacheMatrix environment and brings it to the cacheSolve environment
      if(!is.null(m)) {         # check if m is not null
            message("getting cached data")  # is m isn't null, it returns this
            return(m)
      }
      data<-x$get()         #if m is null, then it grabs the magtrix in makeCacheMatrix and calls it data
      m<-solve(data, ...)   # creates the mean of the data and calls it m
      x$setinv(m)         # pushes m back into the makeVector enviroment
      m                    # calls the value of m
}
