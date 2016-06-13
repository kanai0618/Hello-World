
set.seed(42)

data<-c("AuthfailureReason","Authstats","Serverity","Impact","Category","Subcat")

match_col<-c("match1","match2","match3","match4","match5","match6")

wt_att<-c(1,1,1,1,1,1)

count<-1




fit_data_al<-fit_data

fit_data_al$order<-1:nrow(fit_1)


weigtage<-function()
{
  return(1)
}

convert<-function(k)
{
  return(as.name(k))
}

normal_bound<-function(l)
{
  
  lowerq = quantile(l,0.25)
  upperq = quantile(l,0.75)
  iqr= upperq-lowerq
  upperoutlier =(iqr *3)+upperq
  loweroutlier= lowerq-(iqr *3)
  k<-c(loweroutlier,upperoutlier)
  return(k)
  
}

attribute_risk_score<-function(data_fr,o,count)
{
  k<-kmeans(data_fr$o,2)
  
  l<-which.min(k$size)
  
  
 
  
  if(which.min(k$size)<nrow(data_fr)/100)
    
  {
  data_clus_1<-data_fr[k$cluster==l,]  
  
  }
  
  l<-convert(match_col[count])
  
  print(l)
  
  data_fr$l<-match(data_fr$order,data_clus_1$order, nomatch=0)
  
  data_fr$l<-ifelse(data_fr$o>0,1,0)
  
  
  
  
  m<-which.max(k$size)
  data_clus_2<-data_fr[k$cluster==m,]
  
  p<-normal_bound(data_clus_2$o)
  
  data_clus_3<-data_fr[data_clus_2$o>p[1]&&data_clus_2$o<p[2],]
  
  data_fr$l<-match(fit_data_al$order,data_clus_3$order, nomatch=0)
  
  data_fr$l<-ifelse(fit_data_al$o>0,1,0)
  
  
  
  

}






for(i in data)
{
    
    o<-convert(i)
    print(o)
    data_fr<-fit_data_al
    attribute_risk_score(data_fr,o,count)
    count<-count+1
    
  
  
  
}




fit_data_al$Total_score<-with(fit_data_al,match1+match2+match3+match4+match5+match6)


#plot<-boxplot(fit_1$Category ~k$cluster,data=fit_1,xlab='cluster', ylab='Authsfailure',main='Authfailure Cluster',range=0)


