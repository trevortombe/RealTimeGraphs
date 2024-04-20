# Setup the environment
source("R/Setup.R")

# Figure 7.2 - velocity of M1+ and M2+
M2p<-get_cansim_vector('41552798')
M1p<-get_cansim_vector('37151')
GDP<-get_cansim_vector('62305783')
plotdata<-GDP %>%
  mutate(Ref_Date=as.yearqtr(Date)) %>%
  select(Ref_Date,GDP=VALUE) %>%
  left_join(M1p %>% mutate(Ref_Date=as.yearqtr(Date)) %>%
              group_by(Ref_Date) %>%
              summarise(M1p=mean(VALUE)),by="Ref_Date") %>%
  left_join(M2p %>% mutate(Ref_Date=as.yearqtr(Date)) %>%
              group_by(Ref_Date) %>%
              summarise(M2p=mean(VALUE)),by="Ref_Date") %>%
  drop_na() %>%
  gather(type,value,-Ref_Date,-GDP) %>%
  mutate(velocity=GDP/value)
ggplot(plotdata,aes(Ref_Date,velocity,group=type,color=type))+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',x=1990,y=7,label="Velocity of M1+",color=col[1],size=3,fontface='bold')+
  annotate('text',x=2000,y=2.5,label="Velocity of M2+",color=col[2],size=3,fontface='bold')+
  geom_hline(yintercept = 0,size=1)+
  scale_y_continuous(limit=c(0,NA))+
  scale_x_continuous(breaks = seq(1975,2025,5))+
  labs(x="",y="Velocity")
ggsave('Figures/Fig07-002.png',width=6,height=3)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Fig 7.2")
writeData(wb,"Fig 7.2",plotdata %>% ungroup() %>% 
            select(-value,-GDP) %>% 
            spread(type,velocity))
insertImage(wb,"Fig 7.2",'Figures/Fig07-002.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 7.3 - Inflation and nominal interest rates in Canada
bonds<-get_cansim_vector('122558') # government 1-3 years
cpi<-get_cansim_vector('41690973') # all-items monthly
plotdata<-cpi %>%
  mutate(Inflation=100*(VALUE/lag(VALUE,12)-1),
         Ref_Date=as.yearmon(Date)) %>%
  select(Ref_Date,Inflation) %>%
  left_join(bonds %>% mutate(Ref_Date=as.yearmon(Date)) %>%
              select(Ref_Date,Yield=VALUE),by="Ref_Date") %>%
  drop_na() %>%
  gather(type,value,-Ref_Date) %>%
  filter(Ref_Date>="Jan 1960")
ggplot(plotdata,aes(Ref_Date,value,group=type,color=type))+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',x=1995,y=16,label="Nominal interest rate",color=col[2],size=3,fontface='bold')+
  annotate('text',x=1982,y=2,label="Rate of inflation",color=col[1],size=3,fontface='bold')+
  geom_hline(yintercept = 0,size=1)+
  scale_y_continuous(limit=c(0,NA))+
  scale_x_continuous(breaks = seq(1960,2020,10))+
  labs(x="",y="Percent per year")
ggsave('Figures/Fig07-003.png',width=6,height=3)

addWorksheet(wb,"Fig 7.3")
writeData(wb,"Fig 7.3",plotdata %>% ungroup() %>% 
            spread(type,value))
insertImage(wb,"Fig 7.3",'Figures/Fig07-003.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 7.4 - breakeven inflation expectations
BoC<-get_cansim("10100139")
plotdata<-BoC %>%
  filter(`Financial market statistics` %in% c("Government of Canada benchmark bond yields, long term",
                                              "Real return benchmark bond yield, long term"),
         REF_DATE>=1992,VALUE!=0) %>%
  mutate(date=as.Date(REF_DATE,"%Y-%m-%d")) %>%
  select(date,VALUE,type=`Financial market statistics`) %>%
  mutate(type=ifelse(type=="Real return benchmark bond yield, long term","real","nom")) %>%
  spread(type,VALUE) %>%
  mutate(break_even=100*((1+nom/100)/(1+real/100)-1)) %>%
  mutate(Ref_Date=as.yearmon(date)) %>%
  group_by(Ref_Date) %>%
  summarise(Value=mean(break_even)) %>%
  drop_na()
ggplot(plotdata,aes(Ref_Date,Value))+
  geom_hline(yintercept=1,linetype='dashed')+
  geom_hline(yintercept=3,linetype='dashed')+
  geom_line(size=1.5,color=col[2])+
  geom_hline(yintercept = 0,size=1)+
  scale_x_continuous(breaks = seq(1990,2025,5))+
  labs(x="",y="Percentage")
ggsave('Figures/Fig07-004.png',width=6,height=3)

addWorksheet(wb,"Fig 7.4")
writeData(wb,"Fig 7.4",plotdata)
insertImage(wb,"Fig 7.4",'Figures/Fig07-004.png',
            startCol = 7,startRow=7,width=10,height=5)

##########################
# Save the full workbook #
##########################
saveWorkbook(wb,"Data/C07 Figures.xlsx",overwrite=T)

