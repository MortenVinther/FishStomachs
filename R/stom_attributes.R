all_stom_attributes<-function() {
  x<-    c('Changes done',                                             #1
           'Prey weights by record done',                              #2
           'Bias corrected, energy contents, armament etc',            #3
           'Predator size class assigned',                             #4
           'Bias corrected, regurgitated stomachs',                    #5
           'Prey size class assigned',                                 #6
           'Aggregate by sample ID and predator and prey size group',  #7
           'Temporal and spatial strata added',                        #8
           'Allocate partly identified prey species',                  #9
           'Allocate missing prey length')                           #10
  names(x)<-c('changes','prey_w_id','bias_energy','pred_size','bias_regur',
              'prey_size','aggregate_id','strata','mis_prey','mis_size')
  return(x)
}
