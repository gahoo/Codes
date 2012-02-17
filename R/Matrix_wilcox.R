rmatrix<-function(M,N){matrix(runif(M*N),nrow=M)}
#配对数据
wilcox.martix.byrow.test.paired<-function(MatrixA,MatrixB){
  P<-c()
  M.length<-nrow(MatrixA)
  for(i in 1:M.length){
    P<-c(P,wilcox.test(MatrixA[i,],MatrixB[i,])$p.value)
  }
  P
}
#非配对数据
wilcox.martix.byrow.test<-function(Matrix){
  P<-c()
  M.length<-nrow(Matrix)
  for(i in 1:M.length){
    P<-c(P,wilcox.test(Matrix[i,])$p.value)
  }
  P
}
#非配对数据
wilcox.martix.byrow.test.fast<-function(Matrix){
  P<-c()
  M.length<-nrow(Matrix)
  Wil<-apply(Matrix,1,wilcox.test)
  for(i in 1:M.length){
    P<-c(P,Wil[[i]]$p.value)
  }
  P
}

A<-rmatrix(5000,6)
B<-rmatrix(500,6)
system.time(P<-wilcox.martix.byrow.test.paired(A,B))


DataA<-read.table("testarray.txt",header=TRUE)
#把除了第一列之外的数值转换为矩阵
MatrixA<-as.matrix(DataA[2:length(colnames(DataA))])
MatrixB<-MatrixA+runif(1)
system.time(DataA$P1<-wilcox.martix.byrow.test(MatrixA))
system.time(DataA$P2<-wilcox.martix.byrow.test.paired(MatrixA,MatrixB))
DataA
wilcox.martix.byrow.test.fast(A)