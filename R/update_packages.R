#run in root is needed to install
#http://www.r-bloggers.com/staying-up-to-date-on-r-packages/
installed<-installed.packages()
available<-available.packages()
ia <- merge(installed, available, by="Package")[,c("Package", "Version.x", "Version.y")]
updates<-ia[as.character(ia$Version.x) != as.character(ia$Version.y),]
updates
update.packages()