library(stringr)
library(fs)
organize <- function(keywords) {

    list_of_conditions <<- strsplit(x=keywords,split=", ")

    doublet_count_summary_file<-read.csv2(file="Doublet_Count_Summary.csv")

    l <- NULL
    for (l in seq_along(list_of_conditions[[1]])) {
        #print(list_of_conditions[[1]][l])
        folder_name<-paste0(list_of_conditions[[1]][l],"_folder")
        dir.create(path=paste0("./",folder_name))
        sub_file_dcsf<-doublet_count_summary_file[which(grepl(list_of_conditions[[1]][l],doublet_count_summary_file$File, fixed = TRUE) == TRUE),]
        sub_file_dcsf<-sub_file_dcsf[,c("File","Dots","Doublets")]
        write.table(x=sub_file_dcsf,file=paste0("DCSF_",list_of_conditions[[1]][l],".csv"),row.names = FALSE,col.names = TRUE)
        condition_files<-list.files(pattern=list_of_conditions[[1]][l])
        condition_files<-condition_files[condition_files != folder_name]
        fs::file_move(path=condition_files,new_path=paste0("./",folder_name))


    }
    #
    #     folder_name<-paste0(list_of_conditions[[1]][l],"_folder")
    #     dir.create(path=paste0("./",folder_name))
    #     sub_file_dcsf<-doublet_count_summary_file[which(grepl(list_of_conditions[[1]][l],doublet_count_summary_file$File, fixed = TRUE) == TRUE),]
    #     sub_file_dcsf<-sub_file_dcsf[,c("File","Dots","Doublets")]
    #     write.table(x=sub_file_dcsf,file=paste0("DCSF_",list_of_conditions[[1]][l],".csv"),row.names = FALSE,col.names = TRUE)
    #     condition_files<-list.files(pattern=list_of_conditions[[1]][l])
    #     condition_files<-condition_files[condition_files != folder_name]
    #     fs::file_move(path=condition_files,new_path=paste0("./",folder_name))
    # }
}



