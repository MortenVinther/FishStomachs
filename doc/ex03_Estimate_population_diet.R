## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(rmarkdown.html_vignette.check_title = FALSE) 
)

## ----ex03_01------------------------------------------------------------------
library(FishStomachs)
suppressMessages(library(dplyr))
suppressMessages(library(readr))

load(file=file.path(system.file( package = "FishStomachs"),"extdata","ex02.Rdata"))
s

do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE, label='Begin example 3')


## ----ex03_02------------------------------------------------------------------

a<-read_csv(file.path(system.file( package = "FishStomachs"),"extdata","species_info.csv"),
        col_types = cols()) %>% 
        filter(nodc>0 & prey_sp) %>%
        select(species,nodc)%>%distinct() %>%
        rename(species_group=species,First=nodc) %>% 
        mutate(Last=First,named=TRUE)
head(a)


b<-read_csv(file.path(system.file( package = "FishStomachs"),"extdata","other_food_nodc.csv"),
        col_types = cols()) %>%
        mutate(named=FALSE)
head(b)

NODC_split<-bind_rows(a,b)
NODC_split


## ----ex03_03------------------------------------------------------------------

s<-group_prey_species(s,NODC_split=NODC_split,show_allocations=FALSE )
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE, label='After group_prey_species')

## ----ex03_04------------------------------------------------------------------
s<-aggregate_within_sample(s)
s
do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE, label="Exampel 2, begins")

## ----ex03_05------------------------------------------------------------------

#structure and default contents
print(get_control(s),show=c('Stratification'))

s<-edit_control(s,
     strata_area_exp=expression(paste('R',area,sep='-')),
     strata_sub_area_exp=expression(rectangle),
     strata_time_exp=expression(paste0(year, "-Q", quarter)),
     strata_year_back=expression(as.numeric(substr(stratum_time, 1, 4))),
     strata_quarter_back=expression(as.numeric(substr(stratum_time, 7, 8)))
    )

# to see the changes
# print(get_control(s),show=c('stratification'))


## ----ex03_06------------------------------------------------------------------
s<-add_strata(s)

do_detailed_output(s,label='After add_strata', by_sample_id=TRUE,to_screen=TRUE)

## ----ex03_07------------------------------------------------------------------
from_to <-make_from_to_species(inp_file=
        file.path(system.file( package = "FishStomachs"),"extdata","from_to_species.csv"))
from_to

## ----ex03_08------------------------------------------------------------------

do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,use_criteria=FALSE,rel_weight=FALSE,
        label="Before reallocation of unindentified preys")


do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,use_criteria=FALSE,rel_weight=TRUE,
        label="Before reallocation of unindentified preys")

# by sample
s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=sample_id,
        dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = FALSE)

do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,use_criteria=FALSE,rel_weight=TRUE,
        label="Before reallocation of unindentified preys")


## ----ex03_10------------------------------------------------------------------

# by sub_area (rectangel)                                                         
s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=stratum_sub_area,
        dist_pred_size=pred_size, do_only=c(1,2),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = FALSE)

# by area (=roundfish area)                                                               
s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area=stratum_area,
        dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = FALSE)

# all areas                                                               
s<-redist_unidentified_prey_sp(s,dist_time=stratum_time,dist_area='All',
        dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = FALSE)


# all areas and half-year (dist_time is changed)                                                             
s<-redist_unidentified_prey_sp(s,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),
        dist_area='All',dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = FALSE)


## ----ex03_11------------------------------------------------------------------

as.data.frame(subset(as.data.frame(s),prey_name=='Gadidae'))


## ----ex03_12------------------------------------------------------------------

# the same as above, but unallocated remains to "other food".                                                             
s<-redist_unidentified_prey_sp(s,dist_time=paste(year,ifelse(quarter %in% c(1,2),'S1','S2')),
        dist_area='All',dist_pred_size=pred_size, do_only=c(1,2,3),from_to_species=from_to,
        by_prey_size=FALSE,remains_to_other = TRUE)

do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,use_criteria=FALSE,rel_weight=TRUE,
        label="After reallocation of unindentified preys")



## ----ex03_15------------------------------------------------------------------

# a very complex way of getting a list of prey names!
keep_prey <-(read_csv(file.path(system.file( package = "FishStomachs"),"extdata","species_info.csv"),
        col_types = cols())  %>% 
        filter(prey_sp)  %>% select(species) %>% unique())$species
keep_prey #input is just vector of prey names

s<-edit_control(s,sel_preys=keep_prey)


## ----ex03_145-----------------------------------------------------------------

# by statum_area (=roundfish area) 
s<-redist_unidentified_prey_lengths(s,dist_time=stratum_time,dist_area=stratum_area,
      dist_pred_size=pred_size,
      remains_to_other = FALSE, # do not add records with missing length allocation to "other food"
      others_to_other=TRUE      # do add other species that selected prey species to "other food"
      )

# within all areas                                                     
s<-redist_unidentified_prey_lengths(s,dist_time=stratum_time,dist_area='All',
      dist_pred_size=pred_size,remains_to_other = FALSE)

#  within all areas, and year (ignoring quarter)                                                   
s<-redist_unidentified_prey_lengths(s=s,dist_time=substr(stratum_time,1,4),dist_area='All',
      dist_pred_size=pred_size,remains_to_other = FALSE)

# as above, but with conversion of unallocated into "other"
s<-redist_unidentified_prey_lengths(s=s,dist_time=substr(stratum_time,1,4),dist_area='All',
      dist_pred_size=pred_size,remains_to_other = TRUE)

do_detailed_output(s, by_sample_id=TRUE,to_screen=TRUE,use_criteria=FALSE,rel_weight=TRUE)


## ----ex04_02------------------------------------------------------------------

s<-edit_control(s,
        calc_sub_strata=list(
          # transform into relative weight before data are compiled
          relative_weight=FALSE,      
          # use number of stomach in the sample as weighting factor
          weighting_factor=expression(n_tot),
          # set file to NA when weighting_factor is given
          weigthing_factor_file=NA
        ),
        calc_strata=list(
          relative_weight=FALSE,
          weighting_factor=expression(sqrt(mean_cpue)),
          weigthing_factor_file=NA
        ),
        calc_total=list(
          relative_weight=FALSE,
          # set to NA when input is provided in a file
          weighting_factor=NA,
          # use data from a specified file (in thos case, a file provided by the FishStomachs)
          weigthing_factor_file=file.path(system.file( package = "FishStomachs"),"extdata","weighting_total.csv")
        )
)

print(attr(s,"control"),show='calc_diet')

# informtion from the group constants in the control obsject is also used.
print(attr(s,"control"),show='constants')


## ----ex04_03------------------------------------------------------------------

make_template_strata_weighting(s,strata=c('sub_strata','strata','total')[3],write_CSV=FALSE)

# to show the contents of stratification file as created above (with write_CSV=TRUE)
read_csv(file.path(system.file( package = "FishStomachs"),"extdata","weighting_total.csv"))

## ----ex04_04------------------------------------------------------------------
print(attr(s,"control"))

d<-calc_population_stom(s)
d


## ----ex03_1122,  include = FALSE----------------------------------------------
# save data
 save(s,file=file.path(system.file( package = "FishStomachs"),"extdata","ex03s.Rdata"))
 save(d,file=file.path(system.file( package = "FishStomachs"),"extdata","ex03d.Rdata"))

