library(RIP)
data(RIP.demo_ExpressionSets)
ls(pattern = "test_eset")
dim(test_eset_01)

#Step 1: Calculate Correlation
str(args(createCorData))
test_eset.names
testset.filenames <- createCorData(ExpressionSets = test_eset.names, series = "RIP_testset")
?createCorData

#Step 2: Correlation Neighbors
?getCorNeighbors
testset.CorNeighbors <- getCorNeighbors(cordata.filenames = testset.filenames, CC = 0.6, FoC = 0.25)

#Step 3: Define RIs
TFs <- sample(RIP.universe$TF, size = 25)
targets <- sample(testset.CorNeighbors@entrez_annotation, size = 100)
testset.RIs <- createPotentialRIs(target.genes = targets, TFs = TFs, annotation.targets = "entrezID")
str(RIP.universe)
head(testset.RIs)

#Step 4: RI Features
testset.features <- calculateRIPfeatures(pRIs = testset.RIs, cor.neighbors = testset.CorNeighbors)

#Step 5: RIP Classifier
testset.classifedRIs_v80 <- classifyRIs(RIfeatures = testset.features, percent.votes = 80, filename = "test_classification_v80.csv")
str(testset.classifedRIs_v80)
testset.classifedRIs_v60 <- classifyRIs(RIfeatures = testset.features, percent.votes = 60, filename = "test_classification_v60.csv")
str(testset.classifedRIs_v60)
head(testset.classifedRIs_v60@predictions)
