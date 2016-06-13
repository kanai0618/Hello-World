
set.seed(42)

data<-c("AuthfailureReason","Authstats","Serverity","Impact","Category","Subcat")

match_col<-c("match1","match2","match3","match4","match5","match6")

wt_att<-c(1,1,1,1,1,1)

count<-1




fit_data_al<-fit_data
data_fr<-fit_data

fit_data_al$order<-1:nrow(fit_1)


weigtage<-function()
{
  return(1)
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

attribute_risk_score<-function(data_fr,i,count,n)
{
  k<-kmeans(data_fr[i],n)
  print(k$size)
  
  
  for(j in 1:n)
  {
    if(k$size[j]<(nrow(data_fr))*0.01)
    {
     
      
      data_clus_1<-data_fr[k$cluster==j,]  
      z<-match_col[count]
      
      
      data_fr$z<-match(data_fr$order,data_clus_1$order, nomatch=0)
      
      data_fr$z<-ifelse(data_fr$z>0,1,0)
    }
    else
    {
     
      data_clus_2<-data_fr[k$cluster==j,]
      
      p<-normal_bound(data_clus_2$i)
      
      data_clus_3<-data_fr[data_clus_2$i>p[1]&&data_clus_2$i<p[2],]
      
      data_fr$z<-match(fit_data_al$order,data_clus_3$order, nomatch=0)
      
      
      
      data_fr$z<-ifelse(data_fr$z>0,1,0)
      
    }
  }
  
} 
  
  
  
  
for(i in data)
{
  
  
  
  attribute_risk_score(fit_data_al,i,count,2)
  count<-count+1
  
  
  
  
}




fit_data_al$Total_score<-with(fit_data_al,match1+match2+match3+match4+match5+match6)


#plot<-boxplot(fit_1$Category ~k$cluster,data=fit_1,xlab='cluster', ylab='Authsfailure',main='Authfailure Cluster',range=0)


