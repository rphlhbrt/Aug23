ListGrepFiles<-function(pattern,folderpath){
    flz<-list.files(path = folderpath,full.names = TRUE)
    if(length(pattern)==1)  return(flz[grep(pattern = pattern,x = flz)])
    return(Flatten(lapply(pattern,function(pp) flz[grep(pattern = pp,x = flz)])))
}