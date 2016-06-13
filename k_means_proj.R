prac<-read.csv('merge_1.txt',header=TRUE)
prac1<-na.omit(prac)

prac1$AuthfailureReason<-as.factor(prac1$AuthfailureReason)
levels(prac1$AuthfailureReason)<-1:length(levels(prac1$AuthfailureReason))
prac1$AuthfailureReason<-as.numeric(prac1$AuthfailureReason)

prac1$Impact<-as.factor(prac1$AuthfailureReason)
levels(prac1$Impact)<-1:length(levels(prac1$Impact))
prac1$Impact<-as.numeric(prac1$Impact)

prac1$Authstats<-as.factor(prac1$Authstats)
levels(prac1$Authstats)<-1:length(levels(prac1$Authstats))
prac1$Authstats<-as.numeric(prac1$Authstats)

prac1$Serverity<-as.factor(prac1$Serverity)
levels(prac1$Serverity)<-1:length(levels(prac1$Serverity))
prac1$Serverity<-as.numeric(prac1$Serverity)


prac1$Subcat<-as.factor(prac1$Subcat)
levels(prac1$Subcat)<-1:length(levels(prac1$Subcat))
prac1$Subcat<-as.numeric(prac1$Subcat)


prac1$Category<-as.factor(prac1$Category)
levels(prac1$Category)<-1:length(levels(prac1$Category))
prac1$Category<-as.numeric(prac1$Category)



pc<-prac1[,c(4,5,7,8,9,10)]
ir.pca<-prcomp(pc,center = TRUE,scale. = TRUE)
comp <- data.frame(ir.pca$x[,1:4])

k<-kmeans(comp,3, nstart=25, iter.max=100)

