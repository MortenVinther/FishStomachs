## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(rmarkdown.html_vignette.check_title = FALSE),
  fig.width=7, fig.height=8
)

## ----ex04_01------------------------------------------------------------------
library(FishStomachs)
suppressMessages(library(dplyr))

load(file=file.path(system.file( package = "FishStomachs"),"extdata","ex03d.Rdata"))
d

d[['PRED']]  # predator related information
d[['PREY']]  # prey related information

# show parts of the control attributes
print(attr(d,'control'),show=c('calc_diet','stratification'))



## ----ex04_02------------------------------------------------------------------

# make a subset of data
d1<-subset(d,pred_size=='0400-0499' & stratum_time=="1991-Q1")
d1

# another subset
d2<-subset(d, stratum_time=="1991-Q2")
d2

#combine subsets
c(d1,d2)

rm(d1,d2)

## ----ex04_03------------------------------------------------------------------

# convert into a data frame
df<-as.data.frame(d)
df

# add a variable 
df$cannibal<-as.character(df$pred_name)==as.character(df$prey_name)

#convert it back to a STOMdiet object with a new PREY variable
d2<-as_STOMdiet(df,new_prey_var = 'cannibal')

# "cannibal" s now included (in the PREY data set)
names(d2[['PREY']])

rm(d2)


## ----ex04_04------------------------------------------------------------------

summary(d)
summary(d,level=2)

# make a subset of data
d1<-subset(d,pred_size=='0300-0399' & stratum_time=="1991-Q1")
d1
summary(d1,level=3)
summary(d1,level=4,drop.unused.levels=TRUE)


## ----ex04_05------------------------------------------------------------------

#default
plot(d)

# change in layout, shorter predator size on the x-axis ("from" value),
#   added number of stomachs and added title
plot(d,Ncol=1,cut_pred_size=c(1,4),addTitle = TRUE, addNstom=TRUE,refac_prey=TRUE)


## ----ex04_06------------------------------------------------------------------

# Change names into English names
d2<-from_to_species_diet(d,pred_from_to=c("species","short"),prey_from_to=c("species","short"),
          sp_info_file=file.path(system.file( package = "FishStomachs"),"extdata",'species_info.csv'))
                         
plot(d2,Ncol=1,cut_pred_size=c(1,4),addTitle = TRUE, addNstom=TRUE)





