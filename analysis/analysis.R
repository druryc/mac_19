library(tidyverse);library(janitor);library(readxl);library(lubridate)
library(cowplot);library(ggdendro);library(ggVennDiagram);library(ggnewscale);library(ggheatmap);library(ggridges)
library(vegan)
library(tools)
library(plotrix)
library(factoextra)
library(car)
library(broom)
library(scales)
library(Hmisc)
library(caret)
cols <- c("B"="#F39C12","NB"="#0000FF")

########################################## TIMELINE ######################################################################## #####
rawtemp<-read_table("./data/temp/master.txt")%>%rename(YY=1)%>%filter(YY!="#yr")%>%dplyr::select(YY,MM,DD,hh,mm,WTMP)%>%filter(WTMP<50)%>%unite(date,YY,MM,DD,sep="-")%>%unite(time,hh,mm,sep=":")%>%
     unite(date,date,time,sep=" ")%>%mutate(date=ymd_hm(date))%>%rename(temp=2)%>%mutate(temp=as.numeric(temp))%>%
     group_by(date(date))%>%summarise(mean=mean(temp))%>%rename(date=1)%>%
     mutate(a=month(date))%>%
     mutate(b=day(date))%>%
     unite(day,a,b,sep="-")%>%mutate(dummy_year=1999)%>%unite(dummy_date,dummy_year,day,sep="-")%>%mutate(year=as.factor(year(date)))%>%
     mutate(dummy_date=ymd(dummy_date)) #set up dummy date for plotting all years on same axes using different colors, is simply the day of the year with all observations set as year 1999

plotdata<-rawtemp%>%
     filter(year==2019)%>%
     mutate(color=case_when(mean>28.5~"red",mean<=28.5~"black"))%>%
     filter(date>='2019-05-01')

timeline<-ggplot()+
     geom_hline(yintercept=28.5,linetype="dotted",color="gray")+
     annotate("rect",ymin=20,ymax=40,xmin=as_date("2019-07-14"),xmax=as_date("2019-07-19"),fill="red",alpha=0.4)+
     annotate("rect",ymin=20,ymax=40,xmin=as_date("2019-08-15"),xmax=as_date("2019-11-11"),fill="gray",alpha=0.4)+
     annotate("rect",ymin=20,ymax=40,xmin=as_date("2019-11-12"),xmax=as_date("2019-11-26"),fill="red",alpha=0.4)+
     geom_line(aes(date,mean,color=mean),data=plotdata)+
     scale_color_gradient2(low="black",mid="blue",high="red",name="Temperature",midpoint=26,guide="none")+
     scale_fill_manual(values=c("gray","white"),name="Visual Status")+
     scale_x_date(date_breaks="1 months",minor_breaks=waiver(),labels=date_format("%B-%Y"))+
     ylab("Temp (°C)")+
     theme_classic(base_size=8)+
     theme(legend.position="none",axis.title.x=element_blank(),
           legend.spacing.y=unit(0.1,"cm"))+
     coord_cartesian(ylim=c(25,31),xlim=c(as_date("2019-05-12"),as_date("2019-12-15")))+
     annotate("text",x=as_date("2019-05-20"),label="MMM +1°C",y=29.5,size=2,alpha=0.8,hjust=1,vjust=1,fontface="italic")+
     annotate("text",x=as_date("2019-10-1"),label="Ambient Temperature",y=30.5,size=2,hjust=1,vjust=0.5,fontface="italic")+
     annotate("segment",x=as_date("2019-09-3"),xend=as_date("2019-8-30"),y=30.5,yend=29.3,color="darkgray")+
     annotate("text",x=as_date("2019-07-14"),y=30.5,label="T1",size=2,hjust=1)+
     annotate("text",x=as_date("2019-07-19"),y=30.5,label="T2",size=2,hjust=0)+
     annotate("text",x=as_date("2019-05-28"),label="Pre-exposure Treatments (panel B)",y=26,size=2,hjust=0,vjust=0.5,fontface="italic")+
     annotate("segment",x=as_date("2019-07-12"),xend=as_date("2019-07-17"),y=26,yend=27)+
     annotate("text",x=as_date("2019-10-23"),label="Field Deployment @ Collection Site",y=26,size=2,hjust=1,vjust=0.5,fontface="italic")+
     annotate("text",x=as_date("2019-12-18"),label="Stress Testing",y=30.5,size=2,hjust=1,vjust=0.5,fontface="italic")+
     annotate("segment",x=as_date("2019-11-20"),xend=as_date("2019-11-29"),y=29.5,yend=30.5);timeline

########################################## TREATMENT INFO ################################################################## #####
working<-read_table("./data/temp/master.txt")%>%rename(YY=1)%>%filter(YY!="#yr")%>%dplyr::select(YY,MM,DD,hh,mm,WTMP)%>%filter(WTMP<50)%>%unite(date,YY,MM,DD,sep="-")%>%unite(time,hh,mm,sep=":")%>%
     unite(date,date,time,sep=" ")%>%mutate(date_time=ymd_hm(date))%>%rename(temp=2)%>%mutate(temp=as.numeric(temp))%>%
     mutate(date=date(date_time))%>%
     filter(date_time>='2019-07-08 00:00:00'&date_time<='2019-07-09 10:00:00')%>%
     mutate(treatment="Control",meantemps=temp,sd=0,hour=hour(date_time))%>%select(date_time,treatment,temp,hour,date,meantemps,sd)

treatments<-read_xlsx("./data/metadata.xlsx",sheet="profiles")%>%
     rename("Constant High"=5)%>%
     separate(Time,into=c("trash","time"),sep=" ")%>%select(-trash)%>%
     unite(date_time,Date,time,sep=" ")%>%
     mutate(date_time=ymd_hms(date_time))%>%
     gather(treatment,temp,-date_time)%>%
     mutate(hour=hour(date_time),date=date(date_time))%>%
     group_by(hour,date,treatment)%>%
     mutate(meantemps=mean(temp),sd=sd(temp))%>%
     filter(date>='2019-07-10')%>%
     bind_rows(.,working)

treatments$treatment <- factor(treatments$treatment,levels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"),labels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"))
a<-ggplot(treatments)+
     #geom_hline(yintercept=28.5,linetype="dotted")+
     geom_line(aes(date_time,meantemps,color=treatment),size=0.5)+
     geom_ribbon(aes(date_time, ymin=meantemps-sd, ymax=meantemps+sd, fill =treatment), alpha = 0.5)+
     theme_classic(base_size=8)+
     scale_color_viridis_d(direction=-1)+
     scale_fill_viridis_d(direction=-1)+
     scale_x_datetime(date_breaks="24 hour",minor_breaks=waiver(),labels=date_format("%H%p\n%b-%d"))+
     theme_classic(base_size=8)+
     theme(legend.key.size=unit(0.3,"cm"),
           legend.background=element_blank(),
           axis.title.x=element_blank(),
           legend.position=c(0.1,0.82),
           legend.title=element_blank())+
     coord_cartesian(xlim=c(as_datetime("2019-07-09 07:00:00"),as_datetime("2019-07-15 00:00:00")))+
     #annotate(y=28.3,x=as_datetime('2019-07-14 06:00:00'),"text",label="MMM+ 1°C",size=2,hjust=0,fontface="italic")+
     annotate("point",x=as_datetime('2019-07-09 12:00:00'),pch=21,y=28.4,size=3,fill="blue",color="black")+
     annotate("text",x=as_datetime('2019-07-09 12:00:00'),y=28.9,size=3,label='T1')+  
     annotate("point",x=as_datetime('2019-07-14 12:00:00'),pch=21,y=31.5,size=3,fill="blue",color="black")+
     annotate("text",x=as_datetime('2019-07-15 04:00:00'),y=32,size=3,label='T2')+
     annotate("point",x=as_datetime('2019-07-14 12:00:00'),pch=21,y=28.5,size=3,fill="blue",color="black")+
     annotate("segment",x=as_datetime("2019-07-15 00:00:00"),xend=as_datetime('2019-07-14 12:00:00'),y=32,yend=31.5)+
     annotate("segment",x=as_datetime("2019-07-15 00:00:00"),xend=as_datetime('2019-07-14 12:00:00'),y=32,yend=28.5)+
     ylab("Temp (°C)");a

roc<-treatments%>%mutate(date=date(date_time),hour=hour(date_time))%>%
     group_by(treatment,date,hour)%>%mutate(temp=mean(temp))%>%
     ungroup()%>%
     mutate(diff=lag(temp,1L)-temp)%>%group_by(treatment)%>%summarise(roc=mean(abs(diff),na.rm=TRUE))
dhw<-treatments%>%mutate(heat=temp-27.5)%>%mutate(heat=case_when(heat<0~0,TRUE~as.numeric(heat)))%>%group_by(treatment)%>%summarise(heat_sum=sum(heat))%>%mutate(dhw=heat_sum/4/24/7)

chars<-left_join(roc,dhw,by="treatment")
#saveRDS(chars,"./data/chars")
chars$treatment <- factor(chars$treatment,levels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"),labels = c("Control","Pulse\nIncrease","Pulse","Pulse\nHigh","Constant\nHigh"))
b<-ggplot(chars)+geom_bar(aes(treatment,dhw,fill=treatment),stat="identity")+
     theme_classic(base_size=8)+
     scale_fill_viridis_d(direction=-1)+
     ylab("DHW")+
     theme(legend.position="none",
           axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
           axis.title.x=element_blank())
c<-ggplot(chars)+geom_bar(aes(treatment,roc,fill=treatment),stat="identity")+
     theme_classic(base_size=8)+
     scale_fill_viridis_d(direction=-1)+
     ylab("ROC (°C/hr)")+
     theme(legend.position="none",
           axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
           axis.title.x=element_blank())

quartz(w=7.2,h=2.5)
plot_grid(NULL,timeline,plot_grid(a,b,c,rel_widths=c(4,1,1.1),nrow=1,align="h",axis="tb",labels=c("B","C","D"),label_size=8),ncol=1,rel_heights=c(0.15,1,2.5),labels=c("A",""),label_size=8,label_y=1.15)


y<-read_xlsx("./data/metadata.xlsx",sheet="small_frags")%>%clean_names()
x<-read_xlsx("./data/metadata.xlsx",sheet="field_pam")%>%clean_names()%>%inner_join(.,y,by="fragment")%>%select(colony.y,treatment)
table(x$colony.y,x$treatment)



########################################## DRM OVERALL ##################################################################### #####
fieldfrags<-read_xlsx("./data/metadata.xlsx",sheet="field_pam")%>%clean_names()%>%select(fragment)%>%mutate(group="field")
meta<-read_xlsx("./data/metadata.xlsx",sheet="small_frags")%>%clean_names()%>%left_join(.,fieldfrags,by="fragment")%>%
     mutate(group=replace_na(group,"lab"))

quartz()
st<-read_xlsx("./data/metadata.xlsx",sheet="stress_temps")%>%
     separate(Time,into=c("trash","time"),sep=" ")%>%select(-trash)%>%
     unite(date_time,Date,time,sep=" ")%>%
     mutate(date_time=ymd_hms(date_time))%>%
     filter(Tank_6>28,Tank_7>28)%>%
     mutate(temp=(Tank_6+Tank_7)/2)%>%
     filter(date_time<'2019-11-27')%>%
     ggplot(.)+
     geom_point(aes(date_time,temp),size=0.2,alpha=0.5)+
     geom_smooth(aes(date_time,temp),size=0.5,color="red")+
     scale_x_datetime(date_breaks="1 day",minor_breaks=waiver(),labels=date_format("%m-%d"))+
     theme_classic(base_size=8)+
     ylab("Temperature (°C)")+xlab("Date")+
     theme(legend.position = "none")+
     #annotate("text",x=as_datetime('2019-11-14'),y=32.5,label="N=2 tanks",fontface="italic",hjust=0,size=2)+
     annotate("text",x=as_datetime('2019-11-26'),y=28.7,label="PAM timepoints",fontface="italic",hjust=1,size=2)+
     annotate("point",x=as_datetime('2019-11-15'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-16'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-18'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-20'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-22'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-24'),y=28.3)+
     annotate("point",x=as_datetime('2019-11-26'),y=28.3);st

temperature<-read_xlsx("./data/metadata.xlsx",sheet="stress_temps")%>%clean_names()%>%
     drop_na()%>%
     mutate(hour=hour(time))%>%
     group_by(date,hour)%>%
     mutate(temp6=mean(tank_6),
            temp7=mean(tank_7))%>%
     dplyr::select(date,hour,temp6,temp7)%>%
     gather(tank,temp,-date,-hour)%>%distinct()%>%
     mutate(excess=temp-27.5)%>%
     arrange(date)%>%
     ungroup()%>%
     mutate(cumulative=cumsum(excess)/24/7/2)%>%
     filter(hour==0)%>%
     dplyr::select(tank,date,cumulative)%>%
     mutate(date=ymd(date))%>%
     mutate(duration=case_when(date=="2019-11-16"~0,
                               date=="2019-11-18"~2,
                               date=="2019-11-20"~4,
                               date=="2019-11-22"~6,
                               date=="2019-11-24"~8,
                               date=="2019-11-26"~10,
                               date=="2019-11-27"~11))%>%
     drop_na()%>%
     mutate(tank=case_when(tank=='temp6'~6,
                           tank=='temp7'~7))

drcdat<-read_xlsx("./data/metadata.xlsx",sheet="stress_pam")%>%janitor::clean_names()%>%
     left_join(.,meta,by="fragment")%>%
     dplyr::select(-contains('record'))%>%
     mutate(t1=(t14_fvfm_1+t14_fvfm_2)/2)%>%
     dplyr::select(-contains('t14'),-row,-rack,-column)%>%
     mutate(t15_fvfm=t15_fvfm/t1,
            t16_fvfm=t16_fvfm/t1,
            t17_fvfm=t17_fvfm/t1,
            t18_fvfm=t18_fvfm/t1,
            t19_fvfm=t19_fvfm/t1,
            t20_fvfm=t20_fvfm/t1,
            t1=1)%>%
     gather(time,fvfm,-fragment,-treatment,-colony,-phenotype,-tank,-group)%>%
     mutate(duration=case_when(time=="t1"~0,
                               time=="t15_fvfm"~2,
                               time=="t16_fvfm"~4,
                               time=="t17_fvfm"~6,
                               time=="t18_fvfm"~8,
                               time=="t19_fvfm"~10,
                               time=="t20_fvfm"~11))%>%
     arrange(fragment,time)%>%
     group_by(fragment)%>%
     mutate(diff=lag(fvfm,1L)-fvfm)%>%
     mutate(fvfm=case_when(duration<=4&diff>0.15~NA_real_,
                           TRUE~as.numeric(fvfm)))%>%
     mutate(fvfm=case_when(duration>=10&diff<(-0.05)~NA_real_,
                           TRUE~as.numeric(fvfm)))%>%
     filter(duration<=11)%>% #comment out here to use duration instead of accumulated heat
     left_join(.,temperature,by=c("tank","duration"))%>%
     dplyr::select(-diff,-date)%>%
     dplyr::rename(accumulated=cumulative)%>%
     filter(group=="field")%>%select(-group)%>%
     mutate(fvfm=case_when(fvfm>1~1,TRUE~as.numeric(fvfm)))%>%drop_na()
# saveRDS(drcdat,"./data/drcdata")

initial_pam<-read_xlsx("./data/metadata.xlsx",sheet="stress_pam")%>%janitor::clean_names()%>%left_join(.,meta,by="fragment")%>%
     mutate(t1=(t14_fvfm_1+t14_fvfm_2)/2)
model<-aov(t1~phenotype*treatment,data=initial_pam)
summary(model)
TukeyHSD(model)
initial_pam%>%select(phenotype,treatment,t1)%>%group_by(phenotype,treatment)%>%summarise(mean=mean(t1))

drcdat<-readRDS("./data/drcdata")
library(drc)
p_list<-drcdat%>%ungroup()%>%dplyr::select(phenotype)%>%distinct()
t_list<-drcdat%>%ungroup()%>%dplyr::select(treatment)%>%distinct()
out<-data.frame(treatment=character(),phenotype=character(),duration=numeric(),predicted=numeric(),pmin=numeric(),pmax=numeric())
ED<-data.frame(best=character(),treatment=character(),phenotype=character(),ed10=numeric(),se=numeric(),lwr=numeric(),uppr=numeric())
for (i in t_list$treatment){
     for (j in p_list$phenotype){
          temp<-drcdat%>%filter(treatment==paste0(i),phenotype==paste0(j))
          model<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA),names = c("Slope","Upper Limit","ED50")))
          best<-(as.data.frame(mselect(model, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1]
          summary(model)
          summary<-ED(model, c(10), interval = "delta")
          ED<-ED%>%add_row(best=best,treatment=i,phenotype=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])
          newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
          pm<-predict(model, newdata=newdata, interval="confidence")
          newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
          out<-out%>%add_row(treatment=i,phenotype=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
     }
}
detach("package:drc", unload=TRUE);detach("package:MASS", unload=TRUE)

z<-drcdat%>%group_by(treatment,phenotype)%>%select(fragment)%>%distinct()%>%select(-fragment)%>%add_tally()%>%distinct()

pvals<-bind_cols(ED%>%select(-best)%>%filter(treatment=="Control",phenotype=="B")%>%dplyr::rename(control=ed10,control_se=se,control_lwr=lwr,control_uppr=uppr)%>%select(-treatment,-phenotype),
                 ED%>%select(-best)%>%filter(treatment=="Pulse",phenotype=="B")%>%dplyr::rename(high=ed10,high_se=se,high_lwr=lwr,high_uppr=uppr)%>%select(-treatment,-phenotype))%>%
  mutate(t=(high-control)/sqrt((control_se^2)+(high_se^2)))%>%
  mutate(p=pt(abs(t),55,lower.tail=FALSE));pvals$p

pvals<-bind_cols(ED%>%select(-best)%>%filter(treatment=="Control",phenotype=="NB")%>%dplyr::rename(control=ed10,control_se=se,control_lwr=lwr,control_uppr=uppr)%>%select(-treatment,-phenotype),
                 ED%>%select(-best)%>%filter(treatment=="Pulse",phenotype=="NB")%>%dplyr::rename(high=ed10,high_se=se,high_lwr=lwr,high_uppr=uppr)%>%select(-treatment,-phenotype))%>%
  mutate(t=(high-control)/sqrt((control_se^2)+(high_se^2)))%>%
  mutate(p=pt(abs(t),54,lower.tail=FALSE));pvals$p

out$treatment <- factor(out$treatment,levels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"),labels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"))
ED$treatment <- factor(ED$treatment,levels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"),labels = c("Control","Pulse\nIncrease","Pulse","Pulse\nHigh","Constant\nHigh"))

trajectory<-ggplot(out) +
     geom_hline(yintercept=0.9,linetype="dotted")+
     geom_ribbon(aes(duration, predicted, ymin=pmin, ymax=pmax,group=interaction(phenotype,treatment)), alpha=0.1)+
     geom_line(aes(duration,predicted,group=interaction(phenotype,treatment),linetype=phenotype,color=treatment),size=0.5)+
     scale_color_viridis_d(direction=-1,name="Acclimatization\nTreatment")+
     scale_linetype_manual(values=c("solid","dotted"),name="Phenotype",labels=c("Bleached","Nonbleached"))+
     theme_classic(base_size=8)+
     scale_x_continuous(limits=c(1,8),trans='log2')+
     scale_y_continuous(limits=c(0.3,1.05),breaks=seq(0.3,1,0.1))+
     xlab("Accumulated DHW")+ylab("Relative fv/fm")+
     theme(legend.position=c(0.25,0.4),legend.key.width=unit(0.5,"cm"),legend.key.height=unit(0.1,"cm"),legend.spacing.y = unit(0.05,"cm"),legend.background=element_blank());trajectory

ed<-ggplot(ED)+geom_bar(aes(interaction(treatment,phenotype),ed10,fill=treatment),stat="identity")+
     scale_fill_viridis_d(direction=-1,name="Treatment")+
     theme_classic(base_size=8)+
     theme(legend.position="none",
            axis.text.y=element_blank())+
     scale_y_continuous(position="left",limits=c(1,8),trans='log2')+
     coord_flip()+
     ylab("ED(10) - Degree Heating Weeks")+
     xlab("Treatment")+
     annotate("text",x=2.5,y=3.8,label="*",size=4)+
     annotate("text",x=4.5,y=3.8,label="*",size=4)+
     annotate("text",x=7.5,y=6,label="*",size=4)+
     annotate("text",x=9.5,y=6.3,label="*",size=4)+
     annotate("text",x=3,y=6.2,label="B",size=3,color="lightgray")+
     annotate("text",x=8,y=8,label="NB",size=3,color="lightgray")+
     annotate("text",x=3.5,y=5.2,label="}",size=6,color="lightgray",fontface="italic")+
     annotate("text",x=8.5,y=7,label="}",size=6,color="lightgray",fontface="italic");ed

wilcox.test(ed10~phenotype,data=ED)
ED%>%group_by(phenotype)%>%summarise(ed10=mean(ed10))

########################################## DRM COLONY ###################################################################### #####
detach("package:drc", unload=TRUE);detach("package:MASS", unload=TRUE)
meta<-read_xlsx("./data/metadata.xlsx",sheet="big_frags")%>%clean_names()%>%select(colony,phenotype)%>%distinct()
library(drc)
drcdat<-readRDS("./data/drcdata")
c_list<-drcdat%>%ungroup()%>%dplyr::select(colony)%>%distinct()%>%mutate(colony=as.factor(colony))
t_list<-drcdat%>%ungroup()%>%dplyr::select(treatment)%>%filter(treatment=="Control"|treatment=="Constant High")%>%distinct()
out<-data.frame(treatment=character(),colony=character(),duration=numeric(),predicted=numeric(),pmin=numeric(),pmax=numeric())
EDcol<-data.frame(best=character(),treatment=character(),colony=character(),ed10=numeric(),se=numeric(),lwr=numeric(),uppr=numeric())
for (i in t_list$treatment){
     for (j in c_list$colony){
          temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
          model1<-drm(fvfm~accumulated,data=temp,fct=W1.3())
          best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1]
          cut<-3*mean(cooks.distance(model1))
          newtemp<-bind_cols(temp,as.data.frame(cooks.distance(model1)))%>%dplyr::rename(cooks=10)%>%mutate(fvfm=case_when(cooks>cut~NA_real_,TRUE~as.numeric(fvfm)))
          model2<-drm(fvfm~accumulated,data=newtemp,fct=W1.3(fixed=c(NA,1,NA)))
          #plot(model2)
          summary<-ED(model2, c(10), interval = "delta")
          EDcol<-EDcol%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])
          newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
          pm<-predict(model2, newdata=newdata, interval="confidence")
          newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
          out<-out%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
     }
}
out$treatment <- factor(out$treatment,levels = c("Control","Pulse","Pulse Increase","Pulse High","Constant High"),labels = c("Control","Pulse","Pulse Increase","Pulse High","Constant High"))
#ED2$treatment <- factor(ED2$treatment,levels = c("Control","Pulse Increase","Pulse","Pulse High","Constant High"),labels = c("Control","Pulse\nIncrease","Pulse","Pulse\nHigh","Constant\nHigh"))
remove<-bind_cols(c('11','20','202','222','222'),c("Constant High","Control","Constant High","Constant High","Control"))%>%dplyr::rename(colony=1,treatment=2)
plot<-out%>%anti_join(.,remove,by=c("colony","treatment"))
EDworking<-EDcol%>%filter(ed10<30)

#11 high
#20 control
#202 high
#222 high
#222 control

j<-"11";i<-"Constant High"
temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
model1<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1];best
#plot(model1,type="all")
model2<-drm(fvfm~accumulated,data=temp,fct=W2.3(fixed=c(NA,1,NA)))
#plot(model2,type="all")
cut<-3*mean(cooks.distance(model2))
newtemp<-bind_cols(temp,as.data.frame(cooks.distance(model2)))%>%dplyr::rename(cooks=10)%>%mutate(fvfm=case_when(cooks>cut~NA_real_,TRUE~as.numeric(fvfm)))
model3<-drm(fvfm~accumulated,data=newtemp,fct=W2.3(fixed=c(NA,1,NA)))
#plot(model3,type="all")
newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
pm<-predict(model3, newdata=newdata, interval="confidence")
newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
plot<-plot%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
summary<-ED(model3, c(10), interval = "delta")
EDworking<-EDworking%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])

j<-"20";i<-"Control"
temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
model1<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1];best
#plot(model1)
model2<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
#plot(model2)
cut<-3*mean(cooks.distance(model2))
newtemp<-bind_cols(temp,as.data.frame(cooks.distance(model2)))%>%dplyr::rename(cooks=10)%>%mutate(fvfm=case_when(cooks>cut~NA_real_,TRUE~as.numeric(fvfm)))
model3<-drm(fvfm~accumulated,data=newtemp,fct=W1.3(fixed=c(NA,1,NA)))
#plot(model3)
newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
pm<-predict(model3, newdata=newdata, interval="confidence")
newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
plot<-plot%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
summary<-ED(model3, c(10), interval = "delta")
EDworking<-EDworking%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])

j<-"202";i<-"Constant High"
temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
model1<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1];best
#plot(model1)
model2<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
#plot(model2)
newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
pm<-predict(model2, newdata=newdata, interval="confidence")
newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
plot<-plot%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
summary<-ED(model2, c(10), interval = "delta")
EDworking<-EDworking%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])

j<-"222"
i<-"Constant High"
temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
model1<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1];best
#plot(model1)
model2<-drm(fvfm~accumulated,data=temp,fct=W2.3(fixed=c(NA,1,NA)))
#plot(model2)
cut<-3*mean(cooks.distance(model2))
newtemp<-bind_cols(temp,as.data.frame(cooks.distance(model2)))%>%dplyr::rename(cooks=10)%>%mutate(fvfm=case_when(cooks>cut~NA_real_,TRUE~as.numeric(fvfm)))
model3<-drm(fvfm~accumulated,data=newtemp,fct=W1.3(fixed=c(NA,1,NA)))
#plot(model3)
newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
pm<-predict(model3, newdata=newdata, interval="confidence")
newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
plot<-plot%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
summary<-ED(model3, c(10), interval = "delta")
EDworking<-EDworking%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])

j<-"222";i<-"Control"
temp<-drcdat%>%filter(treatment==paste0(i),colony==paste0(j))%>%mutate(fvfm=case_when(fvfm=1&&duration>5~NA_real_,TRUE~as.numeric(fvfm)))
model1<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
best<-(as.data.frame(mselect(model1, fctList = list(W1.2(),W1.3(),W1.4(),W2.3(),W2.4(),LL.2(),LL.3(),LL.4())))%>%rownames_to_column())[1,1];best
#plot(model1)
model2<-drm(fvfm~accumulated,data=temp,fct=W1.3(fixed=c(NA,1,NA)))
#plot(model2)
newdata<-expand.grid(accumulated=seq(min(drcdat$accumulated),max(drcdat$accumulated),length=100))
pm<-predict(model2, newdata=newdata, interval="confidence")
newdata$p<-pm[,1];newdata$pmin <- pm[,2];newdata$pmax <- pm[,3]
plot<-plot%>%add_row(treatment=i,colony=j,duration=newdata$accumulated,predicted=newdata$p,pmin=newdata$pmin,pmax=newdata$pmax)
summary<-ED(model2, c(10), interval = "delta")
EDworking<-EDworking%>%add_row(best=best,treatment=i,colony=j,ed10=summary[1],se=summary[2],lwr=summary[3],uppr=summary[4])
detach("package:drc", unload=TRUE);detach("package:MASS", unload=TRUE)

plot$treatment <- factor(plot$treatment,levels = c("Control","Pulse","Pulse Increase","Pulse High","Constant High"),labels = c("Control","Pulse","Pulse Increase","Pulse High","Constant High"))

plotdata<-plot%>%mutate(colony=as.numeric(colony))%>%left_join(.,meta,by="colony")%>%mutate(x="Colony")%>%unite(colony,x,colony,sep=" ")
bleached<-ggplot(plotdata%>%filter(phenotype=="B")) +
     geom_ribbon(aes(duration, predicted, ymin=pmin, ymax=pmax,group=interaction(colony,treatment)), alpha=0.1)+
     geom_line(aes(duration,predicted,group=interaction(colony,treatment),color=treatment),size=0.5)+
     scale_color_viridis_d(direction=-1,name="Treatment")+
     scale_linetype_manual(values=c("solid","dashed"),name="Phenotype")+
     theme_classic(base_size=8)+
     scale_x_continuous(trans="log2",breaks=c(2,3,4,5,6,7,8))+
     scale_y_continuous(limits=c(0.3,1.05),breaks=seq(0.3,1,0.1))+
     xlab("Accumulated DHW")+ylab("Relative fv/fm")+
     geom_hline(yintercept=0.9,linetype="dotted",color="black")+
     theme(legend.position='none',legend.key.size=unit(0.2,"cm"),legend.spacing.y = unit(0.1,"cm"),
           axis.text.x=element_blank(),axis.title.x=element_blank())+facet_wrap(~colony,nrow=1)+
  coord_cartesian(xlim=c(2,8))

nonbleached<-ggplot(plotdata%>%filter(phenotype=="NB")) +
     geom_ribbon(aes(duration, predicted, ymin=pmin, ymax=pmax,group=interaction(colony,treatment)), alpha=0.1)+
     geom_line(aes(duration,predicted,group=interaction(colony,treatment),color=treatment),size=0.5)+
     scale_color_viridis_d(direction=-1,name="Acclimatization\nTreatment")+
     scale_linetype_manual(values=c("solid","dashed"),name="Phenotype")+
     theme_classic(base_size=8)+
     scale_x_continuous(trans="log2",labels = scales::number_format(accuracy = 1),breaks=c(2,3,4,5,6,7,8))+
     scale_y_continuous(limits=c(0.3,1.05),breaks=seq(0.3,1,0.1))+
     xlab("Accumulated DHW")+ylab("Relative fv/fm")+
     geom_hline(yintercept=0.9,linetype="dotted",color="black")+
     theme(legend.position="none",
           legend.background=element_blank(),legend.key.size=unit(0.2,"cm"),legend.spacing.y = unit(0.1,"cm"))+facet_wrap(~colony,nrow=1)+
  coord_cartesian(xlim=c(2,8))

pvals<-EDworking%>%select(-best)%>%filter(treatment=="Control")%>%dplyr::rename(control=ed10,control_se=se,control_lwr=lwr,control_uppr=uppr)%>%select(-treatment)%>%
  left_join(.,EDworking%>%select(-best)%>%filter(treatment=="Constant High")%>%dplyr::rename(high=ed10,high_se=se,high_lwr=lwr,high_uppr=uppr)%>%select(-treatment))%>%
  mutate(t=(high-control)/sqrt((control_se^2)+(high_se^2)))%>%
  mutate(p=2*pt(abs(t),10,lower.tail=FALSE))

advantage<-plotdata%>%mutate(diff=abs(predicted-0.9))%>%group_by(treatment,colony)%>%slice_min(diff)%>%select(colony,treatment,duration)%>%spread(treatment,duration)%>%
  dplyr::rename(control=2,high=3)%>%mutate(advantage=high-control)%>%select(colony,advantage)%>%separate(colony,into=c("trash","colony"),sep=" ")%>%select(-trash)%>%
  inner_join(.,meta%>%select(colony,phenotype)%>%distinct()%>%mutate(colony=as.character(colony)))
saveRDS(advantage,"./data/modeled_advantage")

tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                       hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
     
     gb <- ggplot_build(p)
     lay <- gb$layout$layout
     tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
     p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                   vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}
my_tag1 <- c("p=0.057\nB: -1.5 DHW","p=0.029\nB: +1.5 DHW","p=0.008\nB: +1.4 DHW","p=0.001\nB: +1.9 DHW","p=0.030\nB: -1.7 DHW")
my_tag2 <- c("p=0.026\nNB: +1.3 DHW","p=0.187\nNB: +0.8 DHW","p=0.014\nNB: +1.9 DHW","p=0.214\nNB: +0.6 DHW","p=0.915\nNB: -0.1 DHW")

bleached_labeled<-tag_facet2(bleached, 
              x = 2.1, y = 0.3, 
              vjust = 0, hjust = 0,
              open = "", close = "",
              fontface = 'italic',
              size = 2,
              tag_pool = my_tag1)

nonbleached_labeled<-tag_facet2(nonbleached, 
              x = 2.1, y = 0.3, 
              vjust = 0, hjust = 0,
              open = "", close = "",
              fontface = 'italic',
              size = 2,
              tag_pool = my_tag2)

quartz(w=7.2,h=3)
plot_grid(plot_grid(trajectory,ed,nrow=2,align="v",axis="tb",rel_heights=c(2.5,1),labels=c("A","B"),label_size=8,label_y=c(0.99,1.2)),plot_grid(bleached_labeled,nonbleached_labeled,nrow=2,rel_heights=c(1,1.3),label_size=8,labels=c("C","")),rel_widths=c(1,2))


########################################## BROAD SENSE HERITABILITY ######################################################## #####
advantage<-readRDS("./data/modeled_advantage")
t.test(advantage$advantage,mu=0,alternative="greater")
t.test(advantage~phenotype,data=readRDS("./data/modeled_advantage"))

counts<-drcdat%>%select(treatment,colony,fragment)%>%distinct()%>%group_by(treatment,colony)%>%tally()%>%mutate(colony=as.character(colony))
working<-EDworking%>%select(-best)%>%left_join(.,counts,by=c("treatment","colony"))%>%
  mutate(sd=se*sqrt(n))

c_list<-working%>%select(colony)%>%distinct()
t_list<-working%>%select(treatment)%>%distinct()
out<-data.frame(sample=numeric(),treatment=character(),colony=character())
for (i in t_list$treatment){
  for (j in c_list$colony){
    set.seed(3837)
    temp<-working%>%filter(treatment==paste0(i),colony==paste0(j))
    sd<-temp$sd
    mean<-temp$ed10
    list<-as.data.frame(rnorm(50,mean,sd))%>%mutate(treatment=paste0(i),colony=paste0(j))%>%dplyr::rename(sample=1)
    out<-out%>%add_row(list)
  }
}

herit<-cbind(out%>%filter(treatment=="Control")%>%arrange(colony),
             out%>%filter(treatment=="Constant High")%>%arrange(colony))%>%
  clean_names()%>%
  dplyr::rename(control=1,high=4)%>%select(-treatment,-treatment_2,-colony_2)%>%
  mutate(diff=high-control)%>%select(colony,diff)%>%left_join(.,meta%>%mutate(colony=as.character(colony)),by="colony")

model<-aov(diff~colony,data=herit)
summary(model)
x<-broom::tidy(model)
x[1,3]/(x[1,3]+x[2,3])

model<-aov(diff~colony,data=herit%>%filter(phenotype=="B"))
summary(model)
x<-broom::tidy(model)
x[1,3]/(x[1,3]+x[2,3])

model<-aov(diff~colony,data=herit%>%filter(phenotype=="NB"))
summary(model)
x<-broom::tidy(model)
x[1,3]/(x[1,3]+x[2,3])

anno1=expression("H"^2~"=0.897; ~colony p<0.001")
annob=expression("H"^2~"B only=0.916")
annonb=expression("H"^2~"NB only=0.681")

(0.916-0.681)/0.681


heritplot<-ggplot(herit)+
  geom_hline(yintercept=0,linetype="dotted",color="gray")+
  geom_boxplot(aes(reorder(colony,diff),diff,fill=phenotype),outlier.color = NA)+
  theme_classic(base_size=8)+
  ylab("Acclimatization Potential")+
  xlab("Colony")+
  theme(legend.position=c(0.2,0.9),
        legend.key.size=unit(0.35,"cm"),
        axis.text.x=element_text(angle=90))+
  scale_fill_manual(values=cols,name="Phenotype",labels=c("Bleached","Nonbleached"))+
  annotate("text",label=anno1,x=10,y=-3,size=2,fontface="italic",hjust=1)+
  annotate("text",label=annob,x=10,y=-2.5,size=2,fontface="italic",hjust=1)+
  annotate("text",label=annonb,x=10,y=-2,size=2,fontface="italic",hjust=1)+
  annotate("text",label="N=1000 bootstrapped replicates\nper colony x treatment",x=10,hjust=1,size=2,fontface="italic",y=4)+
  scale_y_continuous(limits=c(-3,4));heritplot

basal_comparison<-EDworking%>%select(treatment,colony,ed10)%>%spread(treatment,ed10)%>%dplyr::rename(constant=2,control=3)%>%
  mutate(diff=constant-control)%>%left_join(.,meta%>%mutate(colony=as.factor(colony)),by="colony")

summary(lm(diff~control,data=basal_comparison))
summary(lm(diff~control,data=basal_comparison%>%filter(control>4)))
summary(lm(diff~control,data=basal_comparison%>%filter(phenotype=='B')))
summary(lm(diff~control,data=basal_comparison%>%filter(phenotype=='NB')))

anno2=expression("R"^2~"=0.187; p=0.212")
basalplot<-ggplot(basal_comparison)+
  geom_hline(aes(yintercept=0),linetype="dotted",color="gray")+
  geom_smooth(aes(control,diff),method="lm",color="darkgray",size=0.5,fill="lightgray")+
  #geom_smooth(aes(control,diff,color=phenotype,fill=phenotype),method="lm",fill=NA,size=0.5)+
  geom_point(aes(control,diff,color=phenotype))+
  scale_color_manual(values=cols,name="Phenotype")+
  scale_fill_manual(values=cols)+
  theme_classic(base_size=8)+
  ylab("Acclimatization Potential")+xlab("Basal Tolerance (Control ED10)")+
  annotate("text",x=5.5,y=-3,label=anno2,size=2,hjust=1,fontface="italic")+
  theme(legend.position="none",
        legend.key.size=unit(0.1,"cm"),
        legend.background=element_blank())+
  scale_y_continuous(limits=c(-3,4))

quartz(w=5.2,h=2.5)
plot_grid(heritplot,basalplot,align="h",axis="tb",labels=c("A","B"),label_size=8)

########################################## DESEQ OVERALL ################################################################### #####
library(DESeq2)#;library(snakecase)
meta<-read_xlsx("./data/metadata.xlsx",sheet="big_frags")%>%clean_names()
cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
list<-as.vector(list.files(path="./data/final_quants/",pattern="*txt"))[1:3]
chars<-readRDS("./data/chars")

# for (i in list){
#      assign(paste0(file_path_sans_ext(i,compression=TRUE)),read_tsv(paste0("./data/final_quants/",i))%>%
#                  mutate(num=as.numeric(TPM))%>%dplyr::rename(contig=Name)%>%
#                  left_join(.,cluster_list,by="contig")%>%select(-contig)%>%
#                  group_by(cluster)%>%summarise(num=sum(num),reads=sum(NumReads))%>%
#                  select(cluster,reads))
# }
# 
# merged_df<-purrr::reduce(mget(ls(pattern = "T.+")), full_join, by = c("cluster"))%>%column_to_rownames(var="cluster")
# saveRDS(merged_df,"./data/deseq_working")

# merged_df<-readRDS("./data/deseq_working")
# colnames(merged_df)[1:ncol(merged_df)]<-(as.data.frame(list)%>%separate(list,into=c("list","trash"),sep=7)%>%select(-trash))$list
# raw_deseq_input<-merged_df%>%mutate_if(is.numeric,as.integer)
# #raw_deseq_input<-merged_df%>%select(-T1_1008,-T1_1009)%>%mutate_if(is.numeric,as.integer)
# treatments<-as.data.frame(colnames(raw_deseq_input))%>%dplyr::rename(id=1)%>%
#      left_join(.,meta,by="id")%>%mutate(treatment=to_any_case(treatment,"snake"))%>%
#      mutate(colony=as.factor(colony))%>%arrange(timepoint,phenotype)%>%
#      bind_cols(.,as.data.frame(rep(rep(1:5,each=5),4))%>%rename(colony.n=1))%>%
#      arrange(id)%>%
#      mutate(colony.n=as.factor(colony.n),timepoint=as.factor(timepoint),treatment=as.factor(treatment),phenotype=as.factor(phenotype))
# 
# dds <- DESeqDataSetFromMatrix(countData = raw_deseq_input,
#                               colData = treatments,
#                               design = ~ timepoint + treatment + phenotype + colony.n + colony.n:phenotype + treatment:timepoint)
# 
# keep <- rowSums(counts(dds)) >= 500;dds <- dds[keep,]
# dds$treatment <- relevel(dds$treatment, ref = "control")
# dds$phenotype <- relevel(dds$phenotype, ref = "B")
# dds$timepoint <- relevel(dds$timepoint, ref = "T1")
# dds_main_results<-DESeq(dds);resultsNames(dds_main_results)
# vsd<-(as.data.frame(assay(vst(dds, blind=FALSE))))
# 
# saveRDS(vsd,"./data/vsd")
# saveRDS(dds_main_results,"./data/dds_main_results")
# saveRDS(dds,"./data/dds")

dds_main_results<-readRDS("./data/dds_main_results")
res1<-results(dds_main_results,contrast=list(c("timepoint_T2_vs_T1","timepointT2.treatmentconstant_high")),alpha=0.01);summary(res1)
res2<-results(dds_main_results,contrast=list(c("timepoint_T2_vs_T1","timepointT2.treatmentpulse_high")),alpha=0.01);summary(res2)  
res3<-results(dds_main_results,contrast=list(c("timepoint_T2_vs_T1","timepointT2.treatmentpulse")),alpha=0.01);summary(res3)
res4<-results(dds_main_results,contrast=list(c("timepoint_T2_vs_T1","timepointT2.treatmentpulse_increase")),alpha=0.01);summary(res4)  
detach("package:DESeq2", unload=TRUE);detach("package:MASS", unload=TRUE);deatch("package:dendextend",unload=TRUE)

out<-bind_rows(as.data.frame(res1)%>%select(padj,log2FoldChange)%>%mutate(treatment='constant_high')%>%rownames_to_column(var="cluster"),
          as.data.frame(res2)%>%select(padj,log2FoldChange)%>%mutate(treatment='pulse_high')%>%rownames_to_column(var="cluster"),
          as.data.frame(res3)%>%select(padj,log2FoldChange)%>%mutate(treatment='pulse')%>%rownames_to_column(var="cluster"),
          as.data.frame(res4)%>%select(padj,log2FoldChange)%>%mutate(treatment='pulse_increase')%>%rownames_to_column(var="cluster"))

TukeyHSD(aov(log2FoldChange~treatment,data=out))
bardat<-out%>%group_by(treatment)%>%
     summarise(mean=mean(abs(log2FoldChange)),se=std.error(abs(log2FoldChange)))%>%
     left_join(.,out%>%group_by(treatment)%>%filter(padj<0.01)%>%tally(),by="treatment")
bardat$treatment <- factor(bardat$treatment,levels = c("control","constant_high","pulse_high","pulse","pulse_increase"),labels = c("Control","Constant High","Pulse Increase","Pulse","Pulse High"))
reg_plotdata<-bardat%>%left_join(.,chars,by="treatment")
summary(lm(mean~dhw,data=reg_plotdata))
anno1=expression("R"^2~"=0.816; p=0.096")

reg<-ggplot(reg_plotdata)+geom_smooth(aes(mean,dhw),method="lm",color="darkgray")+
     geom_point(aes(mean,dhw,fill=treatment),pch=21,size=2,color="black")+
     scale_fill_manual(values=c(scales::viridis_pal()(5))[-5],name="Treatment")+
     theme_classic(base_size=8)+
     ylab("Experimental DHW")+xlab("mean log2FC")+
     theme(legend.position=c(0.3,0.9),
           legend.background=element_blank(),
           legend.title = element_blank(),
           #legend.position=c(0.2,0.2),
           legend.key.size=unit(0.2,"cm"),
           legend.spacing.x=unit(0.1,"cm"))+
     #scale_x_continuous(limits=c(0,6))+
     scale_y_continuous(position="right")+
     annotate("text",x=5.25,y=0.7,label=anno1,size=2,fontface="italic")+
     guides(fill=guide_legend(nrow=4));reg

bar<-ggplot(bardat)+geom_bar(aes(treatment,mean,fill=treatment),stat="identity")+
     geom_errorbar(aes(treatment,ymin=mean-se,ymax=mean+se),width=0)+
     scale_fill_manual(values=c(scales::viridis_pal()(5))[-5])+
     theme_classic(base_size=8)+
     theme(legend.position="none",
           #axis.title.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank())+
     scale_y_continuous(limits=c(0,6))+
     scale_x_discrete(position="top",limits = rev)+
     annotate("text",x=4,y=6,label="a",color="black",size=2,fontface="italic")+
     annotate("text",x=3,y=5,label="b",color="black",size=2,fontface="italic")+
     annotate("text",x=2,y=5.6,label="c",color="black",size=2,fontface="italic")+
     annotate("text",x=1,y=5.4,label="d",color="black",size=2,fontface="italic")+
           ylab("mean log2FC")+xlab("Treatment")+coord_flip();bar

vsd<-readRDS("./data/vsd")
pca<-prcomp(as.data.frame(t(vsd)))
axes<-fviz_pca_ind(pca,axes = c(1,2))

permanova_data<-right_join(meta,as.data.frame(t(vsd))%>%rownames_to_column(var="id"),by="id")
t1<-permanova_data%>%filter(timepoint=="T1")
t2<-permanova_data%>%filter(timepoint=="T2")

set.seed(3839);adonis(t1[,10:ncol(t1)]~phenotype,data=t1,method="manhattan")
set.seed(3839);adonis(t1[,10:ncol(t1)]~colony,data=t1,method="manhattan")
set.seed(3839);adonis(t2[,10:ncol(t2)]~phenotype*treatment,data=t2,method="manhattan")
summary(prcomp(permanova_data[,10:ncol(permanova_data)]))

pca_plotdata<-as.data.frame(axes$data)%>%dplyr::rename(id=1)%>%left_join(.,meta,by="id")%>%mutate(colony=as.factor(colony))
pca<-ggplot(pca_plotdata%>%filter(timepoint!="T7")%>%unite(group,phenotype,timepoint,sep="_",remove=FALSE))+
  stat_ellipse(aes(x,y,lty=timepoint,color=phenotype),alpha=1)+
  geom_point(aes(x,y,fill=timepoint),pch=21)+
  scale_fill_manual(values=c("black","white"),name="Timepoint")+
  scale_linetype_manual(values=c("solid","dotted"),name="Timepoint")+
  scale_color_manual(values=c("#F39C12","#0000FF","#F39C12","#0000FF"),name="Phenotype",labels=c("B","NB"))+
  new_scale_color()+
  new_scale_fill()+
  geom_point(aes(x,y,color=group,fill=group),pch=21)+
  scale_color_manual(values=c("#F39C12","#F39C12","#0000FF","#0000FF"),name="Phenotype",guide="none")+
  scale_fill_manual(values=c("#F39C12","white","#0000FF","white"),guide="none")+
  theme_classic(base_size=8)+
  theme(axis.ticks=element_blank(),
        legend.position=c(0.87,0.2),
        legend.key.height=unit(.2,"cm"),
        legend.key.width=unit(0.5,"cm"),
        legend.background = element_blank(),
        legend.spacing.y=unit(0,"cm"))+
  annotate("text",x=-75,y=-30,label="T1:~pheno p=0.012,~colony p=0.012",size=2,fontface="italic",hjust=0,vjust=1)+
  annotate("text",x=-75,y=-34,label="T2:~pheno p=0.003,~colony ND",size=2,fontface="italic",hjust=0,vjust=1)+
  scale_x_continuous(limits=c(-75,75))+scale_y_continuous(limits=c(-35,29))+
  xlab("PC1 (9.1%)")+ylab("PC2 (3.6%)");pca

# venndata<-out%>%filter(padj<0.01);venndatalist<-split(venndata$cluster, venndata$treatment)
# venn<-ggVennDiagram(venndatalist,label='count',
#               label_alpha = 0,
#               color = 'black',
#               category.names = c("Constant High","Pulse","Pulse High","Pulse Increase"),
#               set_size=2,
#               edge_color="black",
#               label_size=2,
#               edge_size=0.2)+
#      scale_fill_gradient(low="gray",high="turquoise2")+
#      scale_color_manual(values=c("black","black","black","black"))+
#      theme(legend.position="none")+
#      scale_x_continuous(expand = expansion(mult = .3));venn

list<-out%>%filter(padj<0.01)%>%select(cluster)%>%distinct()
dat<-out%>%semi_join(list,by='cluster')%>%select(-padj)%>%group_by(cluster)%>%spread(treatment,log2FoldChange)%>%column_to_rownames(var="cluster")
m<-dat
heat_plotdat<-m%>%
  rownames_to_column(var="cluster")%>%
  gather(treatment,exp,-cluster)

clust_rows <- hclust(dist(m),method="complete")
clust_cols <- hclust(dist(t(m)),method="average")

heat<-ggplot(heat_plotdat)+geom_tile(aes(treatment,cluster,fill=exp))+
  scale_fill_gradient2(low="blue",mid="black",high="red",name="Exp",midpoint=0)+
  theme_classic(base_size=8)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_text(angle=30,vjust=1,hjust=1),
        legend.key.width=unit(0.15,"cm"),
        legend.position=c(1.5,0.5),
        legend.background=element_blank())+
  scale_y_discrete(limits = rownames(m)[clust_rows$order],expand=c(0,0))+
  scale_x_discrete(limits = colnames(m)[clust_cols$order],labels=c("Constant\nHigh","Pulse\nHigh","Pulse","Pulse\nIncrease"),expand=c(0,0));heat

hcdata_rows<-dendro_data(clust_rows,type="rectangle")
hcdata_cols<-dendro_data(clust_cols,type="rectangle")
hcdata_cols$segments<-hcdata_cols$segments%>%mutate(yend=case_when(yend==0~1500,TRUE ~ (yend)))

side<-ggplot()+ geom_segment(data=segment(hcdata_rows), aes(x=x, y=y, xend=xend, yend=yend),size=0.35)+scale_x_continuous(expand = c(0,0))+coord_flip()+theme_void()
top<-ggplot()+ geom_segment(data=segment(hcdata_cols), aes(x=x, y=y, xend=xend, yend=yend),size=0.35)+theme_void()
plots<-cowplot::align_plots(pca,bar,heat,side,align="h",axis="tb")

quartz(w=7.2,h=2.5)
plot_grid(plot_grid(NULL,NULL,top,NULL,NULL,rel_widths=c(2,3.7,1,0.35,0.4),nrow=1),
          plot_grid(plots[[1]],plot_grid(reg,plots[[2]],ncol=1,align="v",axis="lr",labels=c("B","C"),label_size=8,label_x=c(-0.05,-0.05)),plots[[3]],plots[[4]],NULL,nrow=1,rel_widths=c(2,1.5,0.75,0.2,0.2),labels=c("A","","D"),label_size=8,label_x=c(0,0,-0.05)),
          nrow=2,rel_heights=c(1,25))+
  draw_label("N=5267",fontface = "italic", size=6,x=0.93,y=0.96)

########################################## SHARED CONSTNAT PULSE PATTERNS ENRICHMENT ####################################### #####
cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
GO<-read_tsv("./data/reference/GO_output.txt",col_names = FALSE)%>%dplyr::rename(gene=1,GO=2)
blast<-read_tsv("./data/reference/blast_output.txt",col_names=FALSE)%>%
  select(X1,X2,X11)%>%dplyr::rename(contig=X1,gene=X2,e_value=X11)%>%
  separate(gene,into=c("trash","gene"),sep="\\|",extra='drop')%>%select(-trash)%>%
  group_by(contig)%>%arrange(e_value,.by_group = TRUE)%>%group_by(contig)%>%slice_min(e_value,n=1,with_ties=F)

table<-left_join(blast,cluster_list,by="contig")%>%left_join(.,GO,by="gene")%>%ungroup()%>%select(cluster,GO)%>%drop_na()

569/3826
dixon<-read_xlsx("data/dixon_go_mwu.xlsx")%>%select(delta.rank,pval,term,name)
shared<-out%>%filter(treatment=="constant_high"|treatment=="pulse")%>%
  filter(padj<0.01)%>%
  select(-padj)%>%
  spread(treatment,log2FoldChange)%>%
  drop_na()%>%
  mutate(keep=case_when((constant_high<0&pulse<0)|(constant_high>0&pulse>0)~"y",TRUE~"n"))%>%
  filter(keep=='y')%>%
  mutate(exp=(constant_high+pulse)/2)

shared_out<-shared%>%left_join(.,table,by="cluster")%>%drop_na()%>%separate(GO,into=c("a","b","c","d","e","f","g","h","i"),sep=";")%>%
  select(-constant_high,-pulse)%>%
  gather(group,term,-cluster,-exp)%>%
  select(-group)%>%
  left_join(.,dixon,by="term")%>%
  filter(pval<0.01)

########################################## CONSTANT HIGH PATTERNS ########################################################## #####
set.seed(3839)
zscores<-out%>%select(-padj)%>%spread(treatment,log2FoldChange)%>%
     rowwise()%>%
     mutate(mean=mean(c(constant_high,pulse,pulse_high,pulse_increase)),
            sd=sd(c(constant_high,pulse,pulse_high,pulse_increase)))%>%
     mutate(z=((constant_high-mean)/sd))

heats<-zscores%>%select(cluster,z)
cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
GO<-read_tsv("./data/reference/GO_output.txt",col_names = FALSE)%>%dplyr::rename(gene=1,GO=2)
blast<-read_tsv("./data/reference/blast_output.txt",col_names=FALSE)%>%
     select(X1,X2,X11)%>%dplyr::rename(contig=X1,gene=X2,e_value=X11)%>%
     separate(gene,into=c("trash","gene"),sep="\\|",extra='drop')%>%select(-trash)%>%
     group_by(contig)%>%arrange(e_value,.by_group = TRUE)%>%group_by(contig)%>%slice_min(e_value,n=1,with_ties=F)

table<-left_join(blast,cluster_list,by="contig")%>%left_join(.,GO,by="gene")%>%ungroup()%>%select(cluster,GO)%>%drop_na()

options(scipen=999)
write.table(table,"./data/GO_MWU_constant/GO_rawtable.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
write.table(heats,"./data/GO_MWU_constant/0_heats.txt",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)

########################################## CONSTANT HIGH ENRICHMENT GO_MWU ################################################# #####
# system(paste(perlPath="perl","./data/GO_MWU_constant/nrify_GOtable.pl ./data/GO_MWU_constant/GO_rawtable.txt > ./data/GO_MWU_constant/0_table.txt"))
# setwd("~/CRD_GBS/5Mcap_RNAseq/data/GO_MWU_constant/")
# system(paste("rm ./GO_rawtable.txt"))
# system(paste("rm ./MWU*"))
# system(paste("rm ./dissim*"))
# system(paste("rm ./BP*"))
# system(paste("rm ./CC*"))
# system(paste("rm ./MF*"))

# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="BP";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=25, clusterCutHeight=0.15,Alternative="two.sided")
# gomwuPlot(heat_file,goAnnotations,goDivision,absValue=1, level1=0.1, level2=0.025,level3=0.01,txtsize=1, treeHeight=0.5)

# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="MF";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=25, clusterCutHeight=0.25,Alternative="two.sided")
# gomwuPlot(heat_file,goAnnotations,goDivision,absValue=1, level1=0.1, level2=0.025,level3=0.01,txtsize=1, treeHeight=0.5)

# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="CC";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=15, clusterCutHeight=0.25,Alternative="two.sided")
# gomwuPlot(heat_file,goAnnotations,goDivision,absValue=1, level1=0.1, level2=0.025,level3=0.01,txtsize=1, treeHeight=0.5)

a<-bind_rows(read_delim("./data/GO_MWU_constant/MWU_BP_0_heats.txt",delim=" ")%>%mutate(class="BP"),
             read_delim("./data/GO_MWU_constant/MWU_MF_0_heats.txt",delim=" ")%>%mutate(class="MF"),
             read_delim("./data/GO_MWU_constant/MWU_CC_0_heats.txt",delim=" ")%>%mutate(class="CC"))%>%
  filter(p.adj<0.05)%>%
  select(name,term,nseqs,p.adj,class)%>%rename(Name=1,'GO Term'=2,Number=3,'P (FDR)'=4,Class=5)

# write.table(a,"./manuscript/ST1.txt",quote=FALSE,row.names = FALSE,sep="\t")

dixon<-read_xlsx("data/dixon_go_mwu.xlsx")
list<-a%>%select(2)%>%rename(term=1)%>%separate(term,into=c('t1','t2'),sep=";")%>%
  gather(term,group)%>%drop_na()%>%select(-term)%>%dplyr::rename(term=group)

dixon%>%select(delta.rank,pval,term,name)%>%right_join(.,list,by="term")
ATP<-dixon%>%filter(grepl("ATP",name))
GTP<-dixon%>%filter(grepl("GTP",name)) 

########################################## COLONY-SPECIFIC DEGs ############################################################ #####
meta<-read_xlsx("./data/metadata.xlsx",sheet="big_frags")%>%clean_names()
vsd<-readRDS("./data/vsd")
high<-vsd%>%rownames_to_column(var="cluster")%>%gather(id,vsd,-cluster)%>%select(id,cluster,vsd)%>%
     separate(id,into=c('timepoint','plug'),sep="_",remove=FALSE)%>%mutate(plug=as.numeric(plug))%>%
     left_join(.,meta%>%select(id,treatment),by="id")%>%
     filter(treatment=="Constant High")%>%
     select(-id,-treatment)%>%spread(timepoint,vsd)%>%
     mutate(high_foldchange=(T2-T1)/T1)%>%
     select(plug,cluster,high_foldchange)%>%
     left_join(.,meta%>%select(plug,colony),by="plug")%>%distinct()%>%select(-plug)

control<-vsd%>%rownames_to_column(var="cluster")%>%gather(id,vsd,-cluster)%>%select(id,cluster,vsd)%>%
     separate(id,into=c('timepoint','plug'),sep="_",remove=FALSE)%>%mutate(plug=as.numeric(plug))%>%
     left_join(.,meta%>%select(id,treatment),by="id")%>%
     filter(treatment=="Control")%>%
     select(-id,-treatment)%>%spread(timepoint,vsd)%>%
     mutate(control_foldchange=(T2-T1)/T1)%>%
     select(plug,cluster,control_foldchange)%>%
     left_join(.,meta%>%select(plug,colony),by="plug")%>%distinct()%>%select(-plug)

working<-full_join(high,control,by=c('cluster','colony'))%>%
     drop_na()%>%
     mutate(outcome=high_foldchange-control_foldchange)%>%select(cluster,colony,outcome)%>%
     mutate(keep=case_when(outcome==0~1,TRUE~0))%>%
     group_by(cluster)%>%mutate(nonresponsive=sum(keep))%>%
     filter(nonresponsive<=4)%>%select(-keep,-nonresponsive)%>%
     spread(cluster,outcome)%>%mutate(colony=as.character(colony))

advantage<-readRDS("./data/modeled_advantage")%>%select(colony,advantage)

dat<-inner_join(advantage,working,by="colony")%>%select(-colony)
correlation<-rcorr(as.matrix(dat$advantage),as.matrix(dat[,-1]),type=c("pearson"))
heats<-as.data.frame(correlation$r[-1,1])%>%dplyr::rename(rho=1)%>%rownames_to_column(var="cluster")%>%
     bind_cols(.,as.data.frame(correlation$P[-1,1])%>%dplyr::rename(pval=1)%>%rownames_to_column(var="cluster")%>%
     mutate(padj=p.adjust(.$pval,method="fdr"))%>%select(padj))%>%mutate(abs=abs(rho))

(heats%>%arrange((padj)))[1,1]

plot<-dat%>%select(advantage,Cluster77005)
single<-ggplot(plot)+
     geom_smooth(aes(advantage,Cluster77005),method="lm",color="orange")+
     geom_point(aes(advantage,Cluster77005))+
  theme_classic(base_size=8)+
  xlab("Acclimatization Change (DHW)")+
  ylab("Pre-exposure DE")+
  annotate("text",x=-2,y=-0.15,label="R= -0.9658; FDR p=0.073",hjust=0,size=2)+
  annotate("text",x=2,y=0.2,label="Putative ATPase\nCluster77005",color="orange",hjust=1,size=2);single

########################################## RESPONSE RELATIONSHIP ENRICHMENT GO_MWU ######################################### #####
# quartz()
# setwd("~/omics/mac19")
# cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
# GO<-read_tsv("./data/reference/GO_output.txt",col_names = FALSE)%>%dplyr::rename(gene=1,GO=2)
# blast<-read_tsv("./data/reference/blast_output.txt",col_names=FALSE)%>%
#      select(X1,X2,X11)%>%dplyr::rename(contig=X1,gene=X2,e_value=X11)%>%
#      separate(gene,into=c("trash","gene"),sep="\\|",extra='drop')%>%select(-trash)%>%
#      group_by(contig)%>%arrange(e_value,.by_group = TRUE)%>%group_by(contig)%>%slice_min(e_value,n=1,with_ties=F)
# table<-left_join(blast,cluster_list,by="contig")%>%left_join(.,GO,by="gene")%>%ungroup()%>%select(cluster,GO)%>%drop_na()
# 
# options(scipen=999)
# write.table(table,"./data/GO_MWU_adv/GO_rawtable.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
# write.table(heats,"./data/GO_MWU_adv/0_heats.txt",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
# 
# system(paste(perlPath="perl","./data/GO_MWU_adv/nrify_GOtable.pl ./data/GO_MWU_adv/GO_rawtable.txt > ./data/GO_MWU_adv/0_table.txt"))
# setwd("~/CRD_GBS/5Mcap_RNAseq/data/GO_MWU_adv/")
# system(paste("rm ./GO_rawtable.txt"))
# system(paste("rm ./MWU*"))
# system(paste("rm ./dissim*"))
# system(paste("rm ./BP*"))
# system(paste("rm ./CC*"))
# system(paste("rm ./MF*"))
# 
# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="CC";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=15, clusterCutHeight=0.25,Alternative="two.sided")
# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="MF";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=15, clusterCutHeight=0.25,Alternative="two.sided")
# heat_file=paste0("0_heats.txt"); goAnnotations="0_table.txt";goDatabase="go.obo"; goDivision="BP";source("gomwu.functions.R")
# gomwuStats(heat_file, goDatabase, goAnnotations, goDivision, perlPath="perl", largest=0.1, smallest=25, clusterCutHeight=0.25,Alternative="two.sided")
# 
# quartz()
# gomwuPlot(heat_file,goAnnotations,goDivision,absValue=0.5, level1=0.1, level2=0.05,level3=0.01,txtsize=1, treeHeight=0.5)

a<-bind_rows(read_delim("./data/GO_MWU_adv/MWU_BP_0_heats.txt",delim=" ")%>%mutate(class="BP"),
             read_delim("./data/GO_MWU_adv/MWU_MF_0_heats.txt",delim=" ")%>%mutate(class="MF"),
             read_delim("./data/GO_MWU_adv/MWU_CC_0_heats.txt",delim=" ")%>%mutate(class="CC"))%>%
  filter(p.adj<0.1)

b<-bind_rows(read_tsv("./data/GO_MWU_adv/BP_0_heats.txt"),
             read_tsv("./data/GO_MWU_adv/MF_0_heats.txt"),
             read_tsv("./data/GO_MWU_adv/CC_0_heats.txt"))%>%inner_join(.,a,by="name")

list<-b%>%select(name,delta.rank,p.adj)%>%distinct()%>%arrange(delta.rank)%>%
  mutate(label=paste0("p=",round(p.adj,3)))

p1<-ggplot(heats)+
  geom_histogram(aes(rho),bins=100,fill="black")+
  #geom_density(aes(rho),color="red")+
  scale_x_continuous(limits=c(-1,1))+
  theme_classic(base_size=8)+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  ylab("Count")+
  xlab("Correlation")+
  annotate("text",x=-0.965,y=300,label="n=12,772 contigs",angle=0,size=2,color="black",hjust=0)+
  annotate("text",x=-0.965,y=100,label="Cluster77005",angle=45,size=2,color="orange",hjust=0)+
  annotate("segment",x=-0.965,xend=-0.965,y=90,yend=5,color="orange");p1

p2<-ggplot(b%>%mutate(name=case_when(name=="hydrolase activity, acting on acid anhydrides, in phosphorus-containing anhydrides"~"hydrolase activity",
                                     TRUE~as.character(name))))+
  geom_vline(xintercept=0)+
  #geom_density_ridges(aes(value,name,fill=name),stat = "binline", binwidth = .1, scale = 2)+
  geom_density_ridges(aes(value,reorder(name,delta.rank),fill=reorder(name,delta.rank)),
                      scale = 1.1,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0, height = 0),
                      point_shape = '|', point_size = 1, point_alpha = 1)+
  theme_classic(base_size=8)+
  theme(legend.position="none",axis.title.y=element_blank())+
  scale_x_continuous(limits=c(-1,1),breaks=seq(-1,1,0.5))+
  scale_y_discrete(position = "right")+
  annotate("text",x=1,y=1.7,label=list[1,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=2.7,label=list[2,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=3.7,label=list[3,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=4.7,label=list[4,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=5.7,label=list[5,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=6.7,label=list[6,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=1,y=7.7,label=list[7,4],size=2,fontface="italic",hjust=1)+
  annotate("text",x=-1,y=8.7,label=list[8,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=9.7,label=list[9,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=10.7,label=list[10,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=11.7,label=list[11,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=12.7,label=list[12,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=13.7,label=list[13,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=14.7,label=list[14,4],size=2,fontface="italic",hjust=0)+
  annotate("text",x=-1,y=15.7,label=list[15,4],size=2,fontface="italic",hjust=0)+
   annotate("text",x=-1,y=16.7,label=list[16,4],size=2,fontface="italic",hjust=0)+
  # ylab("GOs Enriched in High Correlations")+
  # annotate("text",x=-1,y=16.7,label='FDR p<0.1 (n=16 GOs)',size=2,fontface="italic",hjust=0)+
  xlab("Correlation (DEG ~ Acclimatization Potential)");p2

quartz(w=5.5,h=4)
plot_grid(p1,p2,nrow=2,rel_heights=c(1,1.5),align="v",axis="lr",labels=c("A","C"),label_size=8)+
  draw_plot(single, .6, 0.6, .4,.4)+ #xywh
  draw_label("B", color = "black", size = 8, angle = 0,x=0.59,y=0.98,  fontface = "bold")
  
########################################## CLUSTER 77005 ANNOTATION ######################################################## #####
cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
primarys<-read_delim("./data/reference/mcap_transcriptome_aw_compressed.fasta.clstr",delim=" ",col_names=c("Cluster","gene","info","trash"))%>%
     filter(info=="*")%>%select(gene)%>%separate(gene,into=c("garb","gene"),sep=1)%>%select(-garb)%>%separate(gene,into=c("contig","garbo"),sep=-3)%>%
     select(-garbo)

cluster_list<-read_tsv("./data/reference/cluster_list.txt",col_names = FALSE)%>%dplyr::rename(cluster=1,contig=2)%>%fill(cluster)%>%drop_na()
primarys<-readr::read_delim("./data/reference/mcap_transcriptome_aw_compressed.fasta.clstr",delim=" ",col_names=c("Cluster","gene","info","trash"),skip=1)%>%
  filter(info=="*")%>%select(gene)%>%separate(gene,into=c("garb","gene"),sep=1)%>%select(-garb)%>%separate(gene,into=c("contig","garbo"),sep=-3)%>%
  select(-garbo)

top<-heats%>%mutate(abs=abs(rho))%>%arrange(desc(abs))%>%slice_max(n=1,order_by=abs)%>%
     left_join(.,cluster_list,by="cluster")%>%semi_join(.,primarys,by="contig");top
system(paste("grep -A 1 'TRINITY_DN2564_c0_g2_i1' ./data/reference/mcap_transcriptome_aw_compressed.fasta > ./data/cluster77005.fasta"))
#system(paste("/anaconda3/envs/vcf/bin/bwa index ~/CRD_GBS/genome/Mcap.genome_assembly.fasta"))
system(paste("/anaconda3/envs/vcf/bin/bwa mem ~/CRD_GBS/genome/Mcap.genome_assembly.fasta ./data/cluster77005.fasta > ./data/cluster77005.bam"))
system(paste("/anaconda3/envs/vcf/bin/bedtools bamtobed -i ./data/cluster77005.bam > ./data/cluster77005.bed "))
system(paste("/anaconda3/envs/vcf/bin/bedtools getfasta -bed ./data/cluster77005.bed -fi ~/CRD_GBS/genome//Mcap.genome_assembly.fasta -nameOnly -fo ./data/cluster77005_ref.fasta"))




