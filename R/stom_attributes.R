all_stom_attributes<-function() {
  x<-    c('Changes done',                                             #1
           'Prey weights by record done',                              #2
           'Bias corrected, energy contents, armament etc',            #3
           'Predator size class assigned',                             #4
           'Prey size class assigned',                                 #5
           'Pooled within sample_id and predator size class',          #6
           'Bias corrected, regurgitated stomachs',                    #7
           'Aggregate by sample ID and predator and prey size group',  #8
           'Temporal and spatial strata added',                        #9
           'Allocate partly identified prey species',                  #10
           'Allocate missing prey length')                             #11
  names(x)<-c('changes','prey_w_id','bias_energy','pred_size',
              'prey_size','pooled','bias_regur','aggregate_id','strata','mis_prey','mis_size')
  return(x)
}

