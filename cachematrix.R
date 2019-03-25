## The make Cache Matrix fucntion is the function used to create the cache and store the inverse of the matrix
##The function walks through step by step to read in and get the values of the original matrix and then to ultimately store the value of the inverse





makeCacheMatrix <- function(x = matrix()) {

  inv=NULL
  set=function(y){
    
    x<<-y
    inv<<-null
    
  }
  
  get=function()x
  setinv=function(inverse) inv <<-solve(x)
  getinv=function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
  
}


## The following function solves for the inverse of the matrix.The inverse is store in the variable inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv<-x$getinverse()
  if(!is.null(inv)){
    message('Getting Cahced Data')
    return(inv)
    
  }
  
 
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
  
  
  }



##Below code tests the results of the functions
x=rbind(c(4,-1),c(-1,4))

m=makeCacheMatrix(x)


m$get()


cacheSolve(m)

