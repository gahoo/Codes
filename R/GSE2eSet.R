library(GEOquery)
gse661<-getGEO(filename="GSE/GSE661_family.soft.gz")
Meta(gse661)

#查看是否所有的GSM的GPL都一样
gsmplatforms <- lapply(GSMList(gse), function(x) {Meta(x)$platform})

GSE2matrix<-function(gse,islog=TRUE){
  probesets <- Table(GPLList(gse)[[1]])$ID
  data.matrix <- do.call("cbind", lapply(GSMList(gse), function(x) {
	tab <- Table(x)
	mymatch <- match(probesets, tab$ID_REF)
	return(tab$VALUE[mymatch])
	}))
  data.matrix <- apply(data.matrix, 2, function(x) {
	as.numeric(as.character(x))
	})
  if(!islog) {data.matrix <- log2(data.matrix)}
  data.matrix
}

GSE2matrix(gse661)[1:5,]


GSE2ExpressionSet<-function(gse,islog=TRUE){
  probesets <- Table(GPLList(gse)[[1]])$ID
  data.matrix<-GSE2matrix(gse)
  require(Biobase)
  rownames(data.matrix) <- probesets
  colnames(data.matrix) <- names(GSMList(gse))
  pdata <- data.frame(samples = names(GSMList(gse)))
  rownames(pdata) <- names(GSMList(gse))
  pheno <- as(pdata, "AnnotatedDataFrame")
  eset2 <- new("ExpressionSet", exprs = data.matrix, phenoData = pheno)
  eset2
}

eset2<-GSE2ExpressionSet(gse661)
read.exprSet(eset2)
exprs(eset2)[1:5,]