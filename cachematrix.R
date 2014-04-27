# Part 1
# Create a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
        m<-NULL

        # Set the value of the matrix
        # "<<-" used because x and m are defined in parent environment  # Parent environment is MakeCacheMatrix
        set<-function(y){
                x<<-y
                m<<-NULL
                }

        # Get the value of the matrix   
        get<-function()x

        # Set the value of the inverse matrix
        # "<<-" used because m is defined in parent environment         # Parent environment is MakeCacheMatrix
        setinverse<-function(solve) m <<- solve

        # Get the value of the inverse matrix
        getinverse<-function()m

        # Return a list of the defined functions
        list(set=set, get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}



# Part 2
# Function to compute the inverse of the special "matrix"
cacheSolve<-function(x,...){

        # Query the x matrix's cache    
        m<-x$getinverse()

        # If there is a cache, return the cache's inverse matrix
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                }

        # If there is not a cache, compute the inverse of the matrix    given in Part 1
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)

        # Return the inverse of the matrix      
        m
}
