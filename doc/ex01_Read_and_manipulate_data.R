## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(rmarkdown.html_vignette.check_title = FALSE) 
)

## ----ex01_01------------------------------------------------------------------

suppressMessages(library(dplyr))
library(FishStomachs)

control<-new("STOMcontrol",
        name="Test cod",  # just a name
        # specify where to find data on exchange format. This example uses data for cod incl. in the package
        stomach_dir=file.path(system.file( package = "FishStomachs"),"extdata"), 
        dataSets="cod_stom_1991.csv",
        predators=c("Gadus morhua"),
        years=1991L # keep only data from 1991
        )

#print(control) # This will show the full list of options
print(control,show=c('general','data_used'))


## ----ex01_02------------------------------------------------------------------
s<-read_exchange_data(control,keep_just_mandatory_fields = TRUE)


## ----ex01_03------------------------------------------------------------------
str(s)
s[['PRED']]
s[['PREY']]

## ----ex01_03b-----------------------------------------------------------------

# subset and show overview
s1<-subset(s,quarter==1)
print(s1,show_attributes=FALSE)

s234<-subset(s,quarter >1)
print(s234,show_attributes=FALSE)


s1234<-c(s1,s234)
print(s1234,show_attributes=FALSE)

rm(s1,s234,s1234)


## ----ex01_04------------------------------------------------------------------
s

## ----ex01_05------------------------------------------------------------------
 print(get_control(s),show=c('general','detailed_test_output'))

## ----ex01_06------------------------------------------------------------------
# makes a list with the criteria for the sub-set 
crit<-list(
  pred_name=expression(pred_name== "Gadus morhua"),
  year=expression(year==1991),
  quarter=expression(quarter==1),
  sample_id=expression(sample_id %in% c("Gadus morhua_FRA_y:91_q:1_ID:1008", "Gadus morhua_SCO_y:91_q:1_ID:582"))
  )

s<-edit_control(s,detailed_tst_output=TRUE,
                detailed_tst_file='cod_sample.txt',
                detailed_tst_criteria=crit)

# extract the control object from s, and print the option groups selected
print(get_control(s),show=c('general','detailed_test_output'))
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE, label='Start of example 01')


## ----ex01_07------------------------------------------------------------------
# relative weight is shown
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,rel_weight = TRUE,label='Relativ weight')


## ----ex01_08------------------------------------------------------------------
s<-change_data(s)

s

## ----ex01_09------------------------------------------------------------------

s<-add_NODC_ID(s, predator_or_prey = c("predator", "prey")[1]) 
s<-add_NODC_ID(s, predator_or_prey = c("predator", "prey")[2])


## ----ex01_10------------------------------------------------------------------

# add a variable, box17 to the predator set
s[['PRED']]<- s[['PRED']] %>% mutate(box17=if_else(rectangle %in% c('40F6','40F7'),'inside','outside'))

s<-update(s) # you have to run update() after adding or removing variables


## ----ex01_11------------------------------------------------------------------

as.data.frame(s) %>% 
        mutate(cannibal=as.character(pred_name) == as.character(prey_name), 
               large_prey=prey_l> pred_l*0.3,box17=NULL) %>% 
        as_STOMobs(new_pred_var='cannibal',new_prey_var='large_prey')


## ----ex01_12,  include = FALSE------------------------------------------------
# save data
 save(s,file=file.path(system.file( package = "FishStomachs"),"extdata","ex01.Rdata"))

