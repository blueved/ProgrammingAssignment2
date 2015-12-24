
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	## set function
	set<-function(y){
		x<<-y       ## access the parent environment variable 'x'
		m<<-NULL	## access the parent environment variable 'm'
	}
	## source matrix accessor
	get<-function() x
	## calculates the inverse of the given matrix
	setmatrix<-function(solve) m<<- solve
	## inverse matrix accessor
	getmatrix<-function() m
	## returns a list
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
}


cacheSolve <- function(x=matrix(), ...) {
	## check if there is cached value for this matrix
    m<-x$getmatrix()
    if(!is.null(m)){
      message("*** getting cached data")
      return(m)
    }
	## no cached value:
    matrix <- x$get() 		## get the matrix to invert
    m<-solve(matrix, ...)	## invert it
    x$setmatrix(m)			## cache the value 
    return(m)				## return the inverted matrix
}

