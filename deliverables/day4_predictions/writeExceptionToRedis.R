writeExceptionToRedis<-function(uuid, err){
data<-sprintf('%s', err)
complicated.list<-list("exception"=unbox(data))
writeToRedis(uuid, complicated.list)
}