##The below functions allow you to calculate the inverse of
##a matrix and cache the output so that the same can be
##retrieved from the cache later to avoid computing it everytime

## This function acts as a cache for
## storing the input data i.e. matrix for which the computation
## needs to be done as well as the computed  value i.e. inverse
makeCacheMatrix <- function(x = matrix()) {
	
	##init the computation value to NULL on instantiation 
	i<-NULL

	##function to set the input matrix
	set<-function(y){
		x<<-y
		i<<-NULL	
	}

	##function to get the input matrix
	get<-function() x

	##function to set the computation value
	setSolve<-function(solve) i<<-solve

	##function to get the computation value	
	getSolve<-function() i

	##The list containing 4 sub functions is returned so 
	##that the subfunctions can be accessed from outside
	list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)	
}

## This function takes the cache as input and
## does the calculation if not already cached and caches it
## If already cached it retrieves the value from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	##Check if the cache already contains the value
	i<-x$getSolve()

	##If yes, print message that value is returned from cache
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}

	##else get the input from the cache
	data<-x$get()

	##do the computation
	i<-solve(data,...)

	##store the value back in the cache
	x$setSolve(i)

	##return the value
	i
}
