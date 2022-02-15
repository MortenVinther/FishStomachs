


check_against_old_diet<-function(diet,prey_colors,dev='screen',outDir='.',...) {
# diet<-pok
  area<-calc.prey.number<-config_dir<-haul.prey.no<-mean.weight<-number<-pred<-pred_size<-prey.mean.length.ALK<-prey_size<-prey_sp<-short<-SMS_area<-SMS_dir<-sz<-used.prey.number<-NULL
  if (missing(prey_colors)) prey_colors<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

  SMS<-model_output(diet, length_classes_file=file.path(config_dir,paste0("length_classes_",sz,".csv")),sp_info_file=file.path(config_dir,'species_info.csv'))
  #SMS<-dplyr::mutate_if(SMS,is.character,as.factor)
  predators<-unique(SMS$predator)


  info<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols())
  sp_num<-dplyr::filter(info,prey_sp | number==0) %>% dplyr::select(number,short) %>% dplyr::mutate(number=as.integer(number))


plot_SMS_diet<-function(SMS,dev=dev,nox=2,noy=3,i=0,maxi=NA,add_stom_number=FALSE,label='stom',ncol_lgdn=1,prey_colors) {
 # dev=c('screen','wmf','png')[1];nox=2;noy=3;i=0;maxi=NA;add_stom_number=FALSE;label='stom';ncol_lgdn=1
  if (missing(prey_colors)) prey_colors<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

  tmp<-  sort(unique(SMS$prey_id_number))
  sp_col<-dplyr::mutate(sp_num, prey_colors=prey_colors[1:dim(sp_num)[[1]]]) %>% dplyr::filter(number %in% tmp) %>%arrange(desc(number))

  all.prey.col<- sp_col$prey_colors; names(all.prey.col)<-as.character(sp_col$number)
  all.names<-sp_col$short; names(all.names)<-as.character(sp_col$number)


  aa<-by(SMS,list(SMS$year,SMS$quarter,SMS$pred_id_number),function(x) {
    # x<-subset(SMS,predator=='POK' & year==1987 & quarter==3)
    x<-  x %>% dplyr::mutate_if(is.factor,as.character)
    b<- tapply(x$prey_w,list(x$prey_id_number,substr(x$pred_size,1,4)),sum)
    b[is.na(b)]<-0
    prey.names<-as.numeric(dimnames(b)[[1]])
    length.names<-dimnames(b)[[2]]
    if ((is.na(maxi) & (i %% (nox*noy-1))==0) | (!is.na(maxi) & (i %% (maxi-1))==0 )) {
      newplot(dev,nox,noy,Portrait=F,filename=paste(label,x[1,'year'],x[1,'quarter'],x[1,'predator'],sep='-'),dir=outDir);
      # c(bottom, left, top, right)
      if (add_stom_number) par(mar=c(3,5,3,2)) else par(mar=c(3,5,3,2))
      plot.new(); legend(x=0,y=1,all.names,fill=all.prey.col,cex=1.5,ncol=ncol_lgdn)
    }
    i<<-i+1
    ylims<-c(0,1)
    if (add_stom_number) {
      nstom<-tapply(x$sampling_effort,x$pred_size,mean)
      ylims=c(0,1.1)
    }

    my_bar<-barplot(b,names=length.names,col= all.prey.col[rownames(b)],xpd=TRUE,ylim=ylims)
    # Add number of stomachs
    if (add_stom_number) text(x=my_bar,1 , paste0("n: ", nstom) ,cex=1,pos=3,offset=0.4)

    title(main=paste0(x[1,'year'],' Q:',x[1,'quarter']," Pred:",as.character(x[1,'predator'])))
  })
  if (dev=='png') cleanup()
}
cleanup()

plot_SMS_diet(SMS,dev=dev,nox=2,noy=3,i=0,maxi=NA,add_stom_number=TRUE,label='stom',ncol_lgdn=2)

# compare with old SMS data

old<-read_delim(file.path(SMS_dir,"stomcon_list_evac_Y.dat"),delim=' ') %>%
  dplyr::select(-haul.prey.no ) %>% dplyr::filter( pred %in%  predators)

SMS<-SMS%>% dplyr::mutate(pred_size_key=as.numeric(substr(pred_size,1,4)),prey_size_key=as.numeric(substr(prey_size,1,4)))

tmp<-  sort(unique(SMS$prey_id_number))
sp_col<-dplyr::mutate(sp_num, prey_colors=prey_colors[1:dim(sp_num)[[1]]]) %>% dplyr::filter(number %in% tmp) %>%arrange(desc(number))


b<-full_join(SMS,old,by=c("year" = "year","quarter"="quarter","pred_id_number"="pred.no","predator"="pred","pred_size_key"="pred.size",
                          "prey_id_number"="prey.no", "prey"="prey","prey_size_key"="prey.size")) %>%
  dplyr::select(-area,-SMS_area,-calc.prey.number, -used.prey.number,-prey.mean.length.ALK,-mean.weight)


# dd<-dplyr::select(b,year,quarter,predator,pred_size,pred_size_key,haul.no,sampling_effort) %>%dplyr::filter(quarter==1) %>% dplyr::distinct(); dd;
# as.data.frame(dd)

aaa<-by(b,list(b$pred_id_number,b$year,b$quarter) ,function(tst){
  # tst<-dplyr::filter(b,pred_id_number==19 & year==1991 & quarter==2)
  t_sms<- tapply(tst$prey_w,list(tst$prey_id_number,tst$pred_size_key),sum,na.rm=TRUE)
  colSums(t_sms,na.rm=TRUE)
  t_old<- tapply(tst$stomcon,list(tst$prey_id_number,tst$pred_size_key),sum,na.rm=TRUE)
  colSums(t_old,na.rm=TRUE)
  t_old[is.na(t_old)]<-0
  t_sms[is.na(t_sms)]<-0

  y<-unlist(tst[1,'year']);  q<-unlist(tst[1,'quarter']); p<-unlist(tst[1,'predator'])

  newplot(dev=dev,nox=1,noy=3,Portrait=TRUE,filename=paste("comp",p,y,q,sep='_'),dir=outDir,w8=11,w11=10,pointsize=12)

   all.prey.col<- sp_col$prey_colors; names(all.prey.col)<-as.character(sp_col$number)
   all.names<-sp_col$short; names(all.names)<-as.character(sp_col$number)

  plot.new(); legend(x=0,y=1,all.names,fill=all.prey.col,cex=2,ncol=2)

  barplot(t_old,xpd=TRUE,col=all.prey.col[rownames(t_old)],main=paste('old',p,y,q))
  barplot(t_sms,xpd=TRUE,col=all.prey.col[rownames(t_sms)],main='new')

  if (dev=='png') cleanup()
  t_sms<- tapply(tst$prey_w,list(tst$prey,tst$pred_size_key),sum,na.rm=TRUE)
  colSums(t_sms,na.rm=TRUE)
  t_old<- tapply(tst$stomcon,list(tst$prey,tst$pred_size_key),sum,na.rm=TRUE)
  colSums(t_old,na.rm=TRUE)
  t_old[is.na(t_old)]<-0
  t_sms[is.na(t_sms)]<-0

  t_d<-t_sms-t_old
  cat('\n',p,y,q,'\n','\nold\n')
  print(round(t_old,2))
  cat('\n',p,y,q,'\n','\nnew\n')
  print(round(t_sms,2))
  cat('\ndifference, new-old\n')
  print(round(t_d,2))
  if (dev=='png') cleanup()

})
}

