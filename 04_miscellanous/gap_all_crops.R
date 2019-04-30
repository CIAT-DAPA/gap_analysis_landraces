
############ Function ###############
###### María Victoria Díaz L ########
############ CIAT 2019 ##############

crop<-c("banana", "sorghum", "potato", "barley", "maize", "african_maize", "wheat_durum", "wheat_bread", "rice_african", "common_bean")
region<- c("banana_custom", "sgh_custom", "americas", "world", "americas", "africa", "world", "world", "africa", "americas")


f<-lapply(1:length(crop), function(i){
  
  
  cat(paste0("Processing ", crop[i]), "\n")
  
  occnames<-read.csv(paste0("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/diversity_tree_results/",crop[i],"/gap_count.csv"))
  
  occnames[which(occnames[,"Total_gaps"] != 0) ,"Total_gaps"]<-1
  
  base_nueva<-data.frame(ISO2 = occnames$ISO2, Country = occnames$Country, Crop = occnames$Total_gaps)
  
  colnames(base_nueva)[ncol(base_nueva)]<-crop[i]
  
  rownames(base_nueva)<-as.character(base_nueva$Country)
  
  return(base_nueva)
  
})



merged.data.frame = Reduce(function(...) merge(..., all=T), f)
merged.data.frame$alternative_column<-NA
merged.data.frame$Total_gaps<-NA



db1<-as.data.frame(merged.data.frame[,3:(ncol(merged.data.frame)-1)])
colnames(db1)<-colnames(merged.data.frame)[3:(ncol(merged.data.frame)-1)]


for(i in 1:nrow(merged.data.frame)){
  
  condition<-which(db1[i,] != 0)
  
  if(length(condition) == 0){
    
    merged.data.frame$alternative_column[i]<- "No gaps"
    merged.data.frame$Total_gaps[i]<-0
    
  }else{
    
    merged.data.frame$alternative_column[i]<- paste(names(db1)[condition], collapse = " ; ")
    merged.data.frame$Total_gaps[i]<-sum(db1[i,condition])
    
  }
  
}



json_table<-function(crop){
  
  
  cat("writting the JSON file to map", "\n")
  

  db<-db[,c(1,2, ncol(db),ncol(db)-1)]
  
  col_names_1 <- gsub(colnames(db)[1], paste0("var crop = [['", colnames(db)[1], "',"), colnames(db)[1])
  col_names_2 <- gsub(colnames(db)[2], paste0("'", colnames(db)[2], "_name',"), colnames(db)[2])
  col_names_3 <- gsub(colnames(db)[3], paste0("'",colnames(db)[3], "',"), colnames(db)[3])
  col_names_4 <- gsub(colnames(db)[4], paste0("{role: 'tooltip', p:{html:true}}],"), colnames(db)[4])
  
  
  
  col_names<- c(col_names_1, col_names_2, col_names_3, col_names_4); rm(col_names_1, col_names_2, col_names_3, col_names_4)
  
  
  
  
  x <- lapply(1:nrow(db), function(h){ 
    cat(h, "\n")
    
    a <-db[h,1] %>%  gsub(., paste0("['", as.character(.), "',"), .)
    
    b <- db[h,2] %>% gsub(., paste0("'",., "',"), . )
    
    c <- db[h,3] %>% gsub(., paste0(., ","), . )
    
    d <- db[h,4] %>% gsub(., paste0("'",., "'],"), . )
    
    if(h == nrow(db)){d <- db[h,4] %>% gsub(., paste0("'",., "']];"), . )}
    
    
    base <- data.frame(a, b,c,d )
    colnames(base)<-col_names
    
    return(base)
  })
  
  
  x<-do.call(rbind,x)
  x<-as.data.frame(x)
  rownames(x)<- seq(1:nrow(x))
  
  write.table(x, paste0(baseDir,"/diversity_tree_results/all_crops.js"), row.names = F, quote = F)
  gc()
  
  return(x)
}
