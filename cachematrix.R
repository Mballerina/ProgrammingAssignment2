## My first function is a repository that takes in a matrix as an argument and allows you to set the matrix object, 
## retrieve the matrix object, set the matrix inverse or get the matrix inverse of the original.
## The second function will take in the same matrix object that is passed to the first function, and determine whether 
## the first function has stored the inverse matrix and if not it will calculate it and send it to the first function to be stored/cached.


## Description:
## Function takes in input of a matrix, I have set the default is an empty or 0, 2x2 matrix in order to establish
## that the inverse must be a square matrix. The function then takes the matrix and creates a matrix object in 
## a list. The list has the functions that set the matrix value from input, get the matrix value inputted, 
## set the calculated inverse passed to the function, and then allow one to store/retrieve that mean with the getinv.
## I have done it all in list form rather than setting up the list later.
makeCacheMatrix <- function(FinalMatrix = matrix(0,2,2)) {
        # InverseMatrix is set with a default value of NULL the inverse matrix
        # This must be done according to lexical scoping with double pointer to avoid reintiation as NULL 
        # when getinv() is called again after being set by other function.
        InverseMatrix<<-NULL
        
        # Create special matrix object in a list of functions according to 
        #l ist(set=set, get=get, setinv=setinv, getinv=getinv)
        list(
        # Sets the value of the matrix, either from the input value in the argument, or any other value when just set() is called
        # by taking the value input in set() as the matrix value and setting it as value of FinalMatrix
        #the <<- in the function makes it search the parent environ for a value and
        #then resets the value in the parent to that determined inside the function environ
        set=function(InputMatrix){
                InverseMatrix<<-NULL
                FinalMatrix<<-InputMatrix
                #t he inverse matrix still doesn't exist and is null here
                },
        # Get the set value of matrix either from what was input as the argument of the function or passed in directly to set() 
        # from another function that might have called it.
        get= function() {
                return(FinalMatrix)
                # returns MatrixValue just to let user know it is there/double check
                },
        # set the inverse of the matrix (note: you can use one line anonymous function input without {} aroun m<<-inv)
        # sets the input inverse matrix passed to the function as that Inverse matrix in the parent directory/overall function
        setinv=function(InputInverse) {
                InverseMatrix<<-InputInverse
                },
        #gets inverse of matrix no matter what argument given to function, and when called by other functions
        getinv=function() {
                InverseMatrix
                }
        )
        #Special matrix object is now created in a list.
}


## DESCRIPTION:
## This function tries to get the cached value of the inverse of the matrix from the special matrix object if it exists.
# If it doesn't exist or has changed then it calculates it again and passes the calculated inverse back to be chached.
# Pass in the actual matrix that user is working with and will call the other function to use it as the 'matrix object' with cached data
# Use an argument with ... as other function parameters
cacheSolve <-function(FinalMatrix, ...) {
        # Calls the first function to use it as the 'matrix object' and check if the matrix input has an inverse matrix in the cache
        x<-makeCacheMatrix(FinalMatrix)
        # InverseMatrix is set as the value of the inverse matrix that is pulled from what has been cached
        InverseMatrix<-x$getinv()
        # If the result is not null and it has been calculated it is returned to the user by this function (returned on the console)
        if(!is.null(InverseMatrix)){
                message("getting cached data")
                return(InverseMatrix)
        }
        # Otherwise it calculates the inverse matrix by getting the cached value of the original matrix
        data<-x$get()
        # Then it uses the built in 'solve' function to calc the inverse and sets it as the official InverseMatrix in this function
        InverseMatrix<-solve(data,...)
        #It then takes the calculated inv result and passes it back to the other function to be cached
        x$setinv(InverseMatrix)
        #returns the InverseMatrix that was asked for to the User, this is now the overall global value of the inverse matrix
        InverseMatrix
}

##SAMPLE OUTPUT:
##source('/Applications/R-Programming/cachematrix.R')
##> VariableMatrix<-matrix(1:4,2,2)
##> newmatrix<-makeCacheMatrix(VariableMatrix)
##> cacheSolve(VariableMatrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> newmatrix$getinv()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## Note that as stated above; before list must have InverseMatrix<<-NULL not InverseMatrix<-NULL 
## otherwise output will be.... 
##> newmatrix$getinv()
## NULL
##This is because the cacheSolve function calls this one again and sets the inversematrix to null each time
##Setting the double arrow allows for setting the inverse matrix in the parent directory due to lexical scoping.
