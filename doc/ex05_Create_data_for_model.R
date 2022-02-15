## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(rmarkdown.html_vignette.check_title = FALSE) 
)

## ----ex05_01------------------------------------------------------------------
library(FishStomachs)
suppressMessages(library(readr))

load(file=file.path(system.file( package = "FishStomachs"),"extdata","ex03d.Rdata"))
d

control<-attr(d,'control')
print(control,show=c('model_output','constants'))

# in this case the default setting are fine, however 
# they could be changed by a call to edit_control(), see example 3


## ----ex05_02------------------------------------------------------------------

read_csv(system.file("extdata","length_classes.csv",package = "FishStomachs"),col_types=cols())
read_csv(system.file("extdata","species_info.csv",package = "FishStomachs"),col_types=cols())

# create data for the SMS model
SMS<-model_output(d, length_classes_file=system.file("extdata","length_classes.csv",package = "FishStomachs"),
                  sp_info_file=system.file("extdata","species_info.csv",package = "FishStomachs"),
                  intoSMSformat=FALSE) 
 
SMS

