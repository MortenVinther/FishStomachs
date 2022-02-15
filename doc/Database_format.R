## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")


## ---- maketable1, results = "asis", echo=FALSE,message=FALSE------------------

a<-readr::read_csv(file.path("..","inst","stomach_format.csv"),col_types = readr::cols()) 
a<-  dplyr::mutate_if(a,is.logical,as.character()) 
a<-  dplyr::mutate(a,mandatory=dplyr::if_else(mandatory=='FALSE','F','T'),
                   PRED=dplyr::if_else(PRED=='FALSE','F','T'),PREY=dplyr::if_else(PREY=='FALSE','F','T'))
a<-  dplyr::mutate(a,units=dplyr::if_else(is.na(units),' ',units))
knitr::kable(a[,4:10])


## ---- maketable2, results = "asis", echo=FALSE,message=FALSE------------------
a<-dplyr::mutate(a,alias_1=dplyr::if_else(is.na(alias_1),' ',alias_1),alias_2=dplyr::if_else(is.na(alias_2),' ',alias_2),alias_3=dplyr::if_else(is.na(alias_3),' ',alias_3))
knitr::kable(a[,1:4]) 


