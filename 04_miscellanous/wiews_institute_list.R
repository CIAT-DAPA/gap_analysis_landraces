library(XML);library(rvest);library(xlsx)


dir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/datasets_20180416"
orDir <- paste0(dir, "/", "compressed"); if(!file.exists(orDir)){dir.create(orDir)}
out_Dir <- paste0(dir, "/", "outcomes"); if(!file.exists(out_Dir)){dir.create(out_Dir)}
inDir <- paste0(dir, "/", "inputs"); if(!file.exists(inDir)){dir.create(inDir)}
auxDir <- paste0(dir, "/", "auxiliar"); if(!file.exists(auxDir)){dir.create(auxDir)}

url <- "https://www.genesys-pgr.org/wiews/active?page="

wiews_list <- lapply(1:9,function(i){
  cat(i,"\n")
theurl <- paste0(url,i)
file<-read_html(theurl)
tables<-html_nodes(file,"a")
text_tab <- html_text(tables,trim=T)

if(i == 9){
a1 <- text_tab[which(text_tab=="«")[1]:which(text_tab=="«")[2]]
a1 <- a1[-c(grep("«",a1))]#;a1 <-as.data.frame(a1)
} else {
  a1 <- text_tab[which(text_tab=="»")[1]:which(text_tab=="»")[2]]
  a1 <- a1[-c(grep("»",a1))]#;a1 <-as.data.frame(a1)  
}

a1 <- gsub("\t\t\t\t\t\t","",a1);

a1 <- sub(">","",a1);
a1 <- trimws(a1)
wiews1 <- as.data.frame(t(do.call(cbind,strsplit(a1,"\n\n",fixed = T))))
#wiews1 <- wiews1[-c(nrow(wiews1)),]
return(wiews1)

})

wiews_list <- do.call(rbind,wiews_list)
colnames(wiews_list) <- c("ID","INSTITUTE")
write.xlsx(wiews_list,paste0(auxDir,"/","Genesys_institution.xls"),sheetName = "WIEWS",col.names = T,row.names = F)
