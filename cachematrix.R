## Creates a matrix object capable of calculating and caching its inverse
makeCacheMatrix <- function(x=matrix()) {
    ## the inverse 
    inv<<-NULL
    ##the matrix  
    mat<-x
    
    getMatrix<-function()mat
    setMatrix<-function(x=matrix()){s
        mat<<-x
    }
    getInv<-function()  inv
    setInv<-function(c){
        inv<<-c
    }
    list(getMatrix= getMatrix,
         getInv=getInv,setInv=setInv)
}

##Calculates,caches, and returns the inverse of the argument 
cacheSolve <- function(x,...) {
    i<-x$getInv()
    if(!is.null(i)){
        print("Reading cached data")
        print(i)
    }else{
        data<-x$getMatrix()
        
        print("Calculating the inverse...")
        inver<-solve(data)
        
        print("Caching the data")
        x$setInv(inver)
        
        print("The inverse is ")
        y<-x$getInv()
        print(y)
    }
    
}
##Test cases
testMatrix1<-matrix(rbind(1:2,2:1), nrow = 2, ncol = 2, byrow = TRUE)
obj<-makeCacheMatrix(testMatrix1)
print("The matrix is")
testMatrix1
cacheSolve(obj)
cacheSolve(obj)

testMatrix2<-matrix(rbind(1:3,c(0,1,4),c(5,6,0)),nrow=3,ncol=3,byrow=FALSE)
obj<-makeCacheMatrix(testMatrix2)
print("The matrix is")
testMatrix2
cacheSolve(obj)
cacheSolve(obj)







