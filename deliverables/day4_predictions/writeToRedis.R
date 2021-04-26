writeToRedis<-function(uuid,results){
jsonout<-toJSON(results, auto_unbox = TRUE)
#jsonout = gsub('probability.','probability_', jsonout) #remove periods from json output
redishost<-Sys.getenv("REDIS_ADDR")
redisport<- if (Sys.getenv("REDIS_PORT")=="") 6379 else as.numeric(Sys.getenv("REDIS_PORT"))
redisConnect(host=redishost, port=redisport, nodelay=FALSE)
redisSet(uuid,charToRaw(jsonout))
cat("DONE WITH REDIS")
}