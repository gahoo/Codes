#Generate Random Bool vectors to form simulate Keywords matrix
Keyword.GenMatrix.faster<-function(Article_num,Keyword_num){
  IDnum<-as.integer(runif(1,min=Keyword_num,max=Article_num*Keyword_num*.2))
  ID<-as.integer(runif(IDnum,min=1,max=Article_num*Keyword_num))
  K<-rep(c(F),times=Article_num*Keyword_num)
  K[ID]<-c(T)
  dim(K)<-c(Keyword_num,Article_num)
  K
}


#Generate Random Bool vectors to form simulate Keywords matrix
Keyword.GenMatrix<-function(Article_num,Keyword_num){
  K<-matrix(!!abs(as.integer(rnorm(Article_num))),ncol=Article_num)
  for(i in 1:(Keyword_num-1))
    {K<-rbind(K,!!abs(as.integer(rnorm(Article_num))))}
  K
}

#calculate co-occurrence matrix with loop
Keywords.loop<-function(Matrix){
  Keyword_num<-nrow(Matrix)
  KK<-Matrix[2:Keyword_num,]&Matrix[1:(Keyword_num-1),]
  for (i in 2:(Keyword_num-1))
    {KK<-rbind(KK,Matrix[(i+1):Keyword_num,]&Matrix[1:(Keyword_num-i),])}
  KK
}

#calculate co-occurrence matrix with recursive
Keywords.recursive<-function(Matrix1,Matrix2){
  rownum<-nrow(Matrix1)
  Matrix1<-Matrix1[1:(rownum-1),]
  Matrix2<-Matrix2[2:rownum,]
  if(rownum>2)
    {KK<-rbind(Matrix1&Matrix2,Keywords.recursive(Matrix1,Matrix2))
    KK
    }
  else
    {KK<-Matrix1&Matrix2
    KK}
}

#counts the num of TRUE
count.true<-function(v){length(v[v])}
Keywords.count<-function(Matrix){apply(Matrix,1,count.true)}

#Generate Random List to form simulate Keywords Sets
Keyword.GenSets<-function(Article_num,Keyword_num){
  ID<-list()
  for(i in 1:Keyword_num){
    IDnum<-as.integer(runif(1,min=1,max=Article_num))
    ID[[i]]<-as.integer(runif(IDnum,min=1,max=Article_num))
    }
  ID
}

#calculate co-occurrence with intersect
Keyword.CoSet<-function(Sets){
  ID<-c()
  Sets.length<-length(Sets)
  for(i in 1:(Sets.length-1)){
    for(j in (i+1):Sets.length){
    ID<-c(ID,length(intersect(Sets[[i]],Sets[[j]])))
    }
  }
  ID
}

#convert Sets to Matrix
Keyword.Set2Matrix<-function(Sets,Article_num){
  Keyword_num=length(Sets)
  K<-matrix(FALSE,nrow=Keyword_num,ncol=Article_num)
  for(i in 1:Keyword_num){
  K[i,Sets[[i]]]<-TRUE
  }
  K
}

#convert Matrix to Sets
Keyword.Matrix2Set<-function(M){
  ID<-list()
  M.length<-nrow(M)
  cnames<-c(1:ncol(M))
  for(i in 1:M.length){
    ID[[i]]<-cnames[M[i,]]
  }
  ID
}


K<-Keyword.GenMatrix.faster(Article_num=10000,Keyword_num=100)
Sets<-Keyword.Matrix2Set(K)

#
#Sets<-Keyword.GenSets(Article_num=100,Keyword_num=555)
#K<-Keyword.Set2Matrix(Sets,Article_num=100)


system.time(TT<-Keywords.recursive(K,K))
system.time(CC<-Keywords.count(TT))
system.time(CCC<-Keyword.CoSet(Sets))
