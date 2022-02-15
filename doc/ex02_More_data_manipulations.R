## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(rmarkdown.html_vignette.check_title = FALSE)
)

## ----setup--------------------------------------------------------------------
library(FishStomachs)
suppressMessages(library(dplyr))
suppressMessages(library(readr))

## ----ex01_15------------------------------------------------------------------

library(FishStomachs)

# load data from example 01
load(file=file.path(system.file( package = "FishStomachs"),"extdata","ex01.Rdata"))
s

## ----ex02_01------------------------------------------------------------------
# Data for grouping of prey items based on the NODC which provide an easy grouping.  
read_csv(file.path(system.file( package = "FishStomachs"),"extdata","consum_nodc.csv"))

# Parameters for the stomach evacuation model
read_csv(file.path(system.file( package = "FishStomachs"),"extdata","parameters.csv"))

# Ambient temperature by ICES rectangel and quarter
read_csv(file.path(system.file( package = "FishStomachs"),"extdata","temperature.csv"))

# Data on energy contents and armament etc
read_csv(file.path(system.file( package = "FishStomachs"),"extdata","energy_density_and_armament.csv"))


## ----ex02_02------------------------------------------------------------------
s<-bias_correct_energy_etc(s,hour_multiplier=2190,
         nodc_group_file=file.path(system.file( package = "FishStomachs"),"extdata","consum_nodc.csv"),
              param_file=file.path(system.file( package = "FishStomachs"),"extdata","parameters.csv"),
        temperature_file=file.path(system.file( package = "FishStomachs"),"extdata","temperature.csv"),
             energy_file=file.path(system.file( package = "FishStomachs"),"extdata","energy_density_and_armament.csv")
      )
      
  do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE)
  do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,rel_weight = TRUE)


## ----ex02_03------------------------------------------------------------------
head(read.csv(file.path(system.file('extdata', package = 'FishStomachs'),'length_classes_config.csv')))
ll<-make_length_classess(inp_dir=system.file('extdata', package = 'FishStomachs'),inp_file='length_classes_config.csv')
ll
# we use the coarser 1981 definition, just for illustrative purposes
ll<-subset(ll,year==1981); ll$year<-1991 

## ----ex02_04------------------------------------------------------------------
s<-put_size_class_on_predator(s,len_classes=ll) # add variables pred_size_class and pred_size.
s<-put_size_class_on_prey(s,len_classes=ll,too_small_to_other = FALSE)     # add variables prey_size_class and prey_size.
s
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE)


## ----ex02_05------------------------------------------------------------------
#overview of number of stomachs
summary(s,level=2)

s<-bias_correct_regurgitated(s)
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE)

summary(s,level=2)

## ----ex02_06,  include = FALSE------------------------------------------------
# save data
save(s,file=file.path(system.file( package = "FishStomachs"),"extdata","ex02.Rdata"))

