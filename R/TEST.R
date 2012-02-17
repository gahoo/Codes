panicle<-read.table("996_test_wilcoxon.txt",header=TRUE)
panicle<-panicle[-2,]
#panicle[1:2,]
wilcox.martix.byrow.test.fast<-function(Matrix){
  P<-c()
  M.length<-nrow(Matrix)
  Wil<-apply(Matrix,1,wilcox.test)
  for(i in 1:M.length){
    P<-c(P,Wil[[i]]$p.value)
  }
  P
}

group1<-c(2,4,6,8,10,12)
group2<-c(3,5,7,9,11,13)
Mpanicle1<-as.matrix(panicle[group1])
Mpanicle2<-as.matrix(panicle[group2])
Mpanicle1<-t(apply(Mpanicle1,1,type.convert))
Mpanicle2<-t(apply(Mpanicle2,1,type.convert))
#Mpanicle1<-rbind(Mpanicle1[1,1:6],Mpanicle1[3:nrow(panicle),1:6])
#Mpanicle2<-rbind(Mpanicle2[1,1:6],Mpanicle2[3:nrow(panicle),1:6])
system.time(panicle$P1<-wilcox.martix.byrow.test.fast(Mpanicle1))
system.time(panicle$P2<-wilcox.martix.byrow.test.fast(Mpanicle2))
CK<-cbind(Mpanicle1[,1],Mpanicle2[,1])
T20<-cbind(Mpanicle1[,2],Mpanicle2[,2])
system.time(panicle$P<-wilcox.martix.byrow.test.paired(Mpanicle1,Mpanicle2))
write.table(panicle,"panicle.txt")

#计算差异
DifSum<-apply(abs(Mpanicle1-Mpanicle2),1,sum)
plot(DifSum)

#画图看选择哪个参数进行筛选好
NumOfAbove<-c()
for (i in 1:20){
  NumOfAbove<-c(NumOfAbove,length(DifSum[DifSum>i]))
}
plot(1:20,NumOfAbove)

