
user_details$diffcol<-abs(user_details$Timestamp_X - user_details$TimeStamp_Y)
user_details$diffrowx<-abs(c(0,diff(user_details$Timestamp_X)))
user_details$diffrowy<-abs(c(0,diff(user_details$TimeStamp_Y)))


user_details$Authstats_count<-1
k<-aggregate(Authstats_count ~ UserId + Authstats, data = user_details,sum)
merge_data<-merge(k,user_details,by=c("UserId","Authstats"))

user_details$Authfail_count<-1
k<-aggregate(Authfail_count~ UserId + AuthfailureReason, data = user_details,sum)
merge_data<-merge(k,merge_data,by=c("UserId","AuthfailureReason"))

user_details$Serverity_count<-1

k<-aggregate(Serverity_count ~ UserId + Serverity, data = user_details,sum)
merge_data<-merge(k,merge_data,by=c("UserId","Serverity"))

user_details$Impact_count<-1

k<-aggregate(Impact_count ~ UserId + Impact, data = user_details,sum)
merge_data<-merge(k,merge_data,by=c("UserId","Impact"))

user_details$Category_count<-1
k<-aggregate(Category_count ~ UserId + Category, data = user_details,sum)
merge_data<-merge(k,merge_data,by=c("UserId","Category"))



user_details$diffrowy <- ave(user_details$TimeStamp_Y, factor(user_details$UserId), FUN=function(x) c(NA,diff(x)))
user_details$diffrowx <- ave(user_details$Timestamp_X, factor(user_details$UserId), FUN=function(x) c(NA,diff(x)))
