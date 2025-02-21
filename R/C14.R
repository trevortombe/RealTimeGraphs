# Setup the environment
source("R/Setup.R")

# Figure 14.1 - Overnight rate and the BoC operating band
target_weekly<-get_cansim("10100139")
plotdata<-target_weekly %>%
  filter(`Financial market statistics` %in% c('Target rate','Operating band, high',
                                              'Operating band, low')) %>%
  select(Date,VALUE,type=`Financial market statistics`) %>%
  filter(Date>="1996-02-01") %>%
  spread(type,VALUE) %>%
  drop_na()
ggplot(plotdata,aes(Date,`Target rate`))+
  geom_ribbon(aes(ymin=`Operating band, low`,
                  ymax=`Operating band, high`),fill='gray80')+
  geom_vline(xintercept = as.Date("2020-03-23"),size=1,linetype='dashed',color=col[1])+
  geom_line(size=1.5,show.legend = F,color=col[2])+
  annotate('text',y=3,x=as.Date('2009-10-01'),hjust=0,label="Overnight rate",size=3,color=col[2],fontface='bold')+
  annotate('text',y=5.75,x=as.Date('2019-10-01'),hjust=1,label="Floor system adopted",size=3,color=col[1])+
  annotate('text',y=0.33,x=as.Date('2006-01-01'),hjust=1,label="Floor system briefly used",size=3,color=col[1])+
  annotate('text',y=4.75,x=as.Date('2008-06-01'),hjust=0,label="Operating band",size=3,color='gray',fontface='bold')+
  geom_segment(x=as.Date('2006-04-01'),xend=as.Date('2008-11-01'),y=0.33,yend=0.33,
               arrow=arrow(length=unit(1,'mm')),color=col[1],size=0.25)+
  geom_hline(yintercept=0,size=1)+
  scale_x_date(date_breaks = '4 years',date_label="%Y")+
  labs(x="",y="Percent")
ggsave('Figures/Fig14-001.png',width=6,height=3)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Fig 14.1")
writeData(wb,"Fig 14.1",plotdata)
insertImage(wb,"Fig 14.1",'Figures/Fig14-001.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 14.2 - target rate vs prime rate and one-year mortgage rate
posted_rates<-fromJSON('https://www.bankofcanada.ca/valet/observations/group/chartered_bank_interest/json')
temp<-data.frame(Mortgage1yr=posted_rates$observations$V80691333,
                 Mortgage5yr=posted_rates$observations$V80691335,
                 Prime=posted_rates$observations$V80691311,
                 Date=posted_rates$observations$d) %>%
  drop_na() %>%
  rename(Mortgage1yr=v,Mortgage5yr=v.1,Prime=v.2) %>%
  mutate(Date=as.Date(Date))
plotdata2<-plotdata %>%
  select(Date,`Target rate`) %>%
  left_join(temp,by="Date") %>%
  drop_na() %>%
  gather(type,value,-Date) %>%
  mutate(value=as.numeric(value))
ggplot(plotdata2,aes(Date,value,group=type,color=type))+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',y=7,x=as.Date('2010-01-01'),hjust=0,label="Mortgage rate, 5 years",size=3,color=col[2],fontface='bold')+
  annotate('text',y=4.25,x=as.Date('2010-02-01'),hjust=0,label="Mortgage rate, 1 year",size=3,color=col[1],fontface='bold')+
  annotate('text',y=1,x=as.Date('2008-01-01'),hjust=1,label="Target overnight rate",size=3,color=col[4],fontface='bold')+
  annotate('text',y=2,x=as.Date('2010-10-01'),hjust=0,label="Prime rate",size=3,color=col[3],fontface='bold')+
  geom_hline(yintercept=0,size=1)+
  scale_x_date(date_breaks = '4 years',date_label="%Y")+
  labs(x="",y="Percent")
ggsave('Figures/Fig14-002.png',width=6,height=4)

addWorksheet(wb,"Fig 14.2")
writeData(wb,"Fig 14.2",plotdata2 %>% 
            spread(type,value))
insertImage(wb,"Fig 14.2",'Figures/Fig14-002.png',
            startCol = 7,startRow=7,width=10,height=6.67)

# A closer look 14.1, the bank rate from 1934 onwards
BoC_monthly<-get_cansim("10100122")
plotdata<-BoC_monthly %>%
  filter(Rates=="Bank rate") %>%
  select(Date,BankRate=VALUE) %>%
  mutate(Date=as.yearmon(Date))
ggplot(plotdata,aes(Date,BankRate))+
  annotate("rect",xmin=as.numeric(as.yearmon("Feb 2020")), xmax=as.numeric(as.yearmon("May 2020")), 
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Oct 2008")), xmax=as.numeric(as.yearmon("May 2009")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Mar 1990")), xmax=as.numeric(as.yearmon("Apr 1992")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Jun 1981")), xmax=as.numeric(as.yearmon("Oct 1982")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Jan 1980")), xmax=as.numeric(as.yearmon("Jun 1980")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Dec 1974")), xmax=as.numeric(as.yearmon("Mar 1975")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Mar 1960")), xmax=as.numeric(as.yearmon("Mar 1961")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Mar 1957")), xmax=as.numeric(as.yearmon("Jan 1958")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Jul 1953")), xmax=as.numeric(as.yearmon("Jul 1954")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Apr 1951")), xmax=as.numeric(as.yearmon("Dec 1951")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Aug 1947")), xmax=as.numeric(as.yearmon("Mar 1948")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Nov 1937")), xmax=as.numeric(as.yearmon("Jun 1938")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate('text',x=1962,hjust=0,size=2.5,y=22,label="Recessions",color='dodgerblue',alpha=0.7)+
  geom_vline(xintercept = as.yearmon("June 1994"),size=1,linetype='dashed',color=col[1])+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',y=20,x=as.yearmon('Jan 1996'),hjust=0,label="Target for the overnight rate\nbecomes the key instrument",
           size=3,color=col[1])+
  annotate('text',y=17.5,x=as.yearmon('Jan 1978'),hjust=1,label="Bank rate",
           size=3,fontface='bold')+
  geom_hline(yintercept=0,size=1)+
  scale_x_continuous(breaks=pretty_breaks(8))+
  scale_y_continuous(expand=c(0,0),limit=c(0,22.5))+
  labs(x="",y="Percent")
ggsave('Figures/Fig14-CloserLook-001.png',width=6,height=3)

addWorksheet(wb,"Fig CloserLook 14.1")
writeData(wb,"Fig CloserLook 14.1",plotdata)
insertImage(wb,"Fig CloserLook 14.1",'Figures/Fig14-CloserLook-001.png',
            startCol = 7,startRow=7,width=10,height=5)

##########################
# Save the full workbook #
##########################
saveWorkbook(wb,"Data/C14 Figures.xlsx",overwrite=T)
