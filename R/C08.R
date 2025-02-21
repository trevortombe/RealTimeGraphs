# Setup the environment
source("R/Setup.R")

# Figure 8.2 - real output per capita, 1870-present
maddison<-read_excel("Data/BaseData.xlsx",sheet="Figure 8-2") # historical data
RGDP<-get_cansim_vector('62305752')
NGDP<-get_cansim_vector('62305783')
Pop<-get_cansim_vector('1')
plotdata<-NGDP %>%
  mutate(Ref_Date=as.yearqtr(Date)) %>%
  select(Ref_Date,NGDP=VALUE) %>%
  left_join(
    RGDP %>%
      mutate(Ref_Date=as.yearqtr(Date)) %>%
      select(Ref_Date,RGDP=VALUE)) %>%
  left_join(
    Pop %>%
      mutate(Ref_Date=as.yearqtr(Date)) %>%
      select(Ref_Date,Pop=VALUE)
  )
renormalize<-mean(filter(plotdata,year(Ref_Date)==2022)$NGDP)/mean(filter(plotdata,year(Ref_Date)==2022)$RGDP)
plotdata<-plotdata %>% mutate(Value=1000000*renormalize*RGDP/Pop)
splice_adjustment<-mean(filter(plotdata,year(Ref_Date)==1961)$Value)/filter(maddison,Date==1961)$Maddison_RGDPpc_2011
plotdata2<-maddison %>%
  rename(Value=Maddison_RGDPpc_2011) %>%
  mutate(Value=Value*splice_adjustment) %>%
  filter(Date<=1960) %>%
  bind_rows(plotdata %>% mutate(Date=year(Ref_Date)) %>%
              group_by(Date) %>%
              summarise(Value=mean(Value)))
ggplot(plotdata2,aes(Date,log(Value)))+
  annotate("text",x=1993,y=11.58,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2)+
  annotate("rect",xmin=1873,xmax=1879,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1891,xmax=1893,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1894,xmax=1896,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1873,xmax=1879,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1882,xmax=1885,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1887,xmax=1888,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1890,xmax=1891,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1893,xmax=1894,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1895,xmax=1896,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1900,xmax=1901,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1902,xmax=1904,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1906,xmax=1908,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  # annotate("rect",xmin=1910,xmax=1911,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1913,xmax=1915,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1917,xmax=1921,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1929+3/12,xmax=1933+1/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1937+10/12,xmax=1938+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1947+7/12,xmax=1948+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1951+3/12,xmax=1951+11/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1953+6/12,xmax=1954+6/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1957+2/12,xmax=1958+0/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1960+2/12,xmax=1961+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1974+11/12,xmax=1975+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  geom_line(size=1.5,color=col[1])+
  scale_x_continuous(breaks=seq(1870,2020,15))+
  scale_y_continuous(breaks=log(c(3000,6000,12000,24000,48000,96000)),limit=c(8,11.6),
                     label=number(c(3000,6000,12000,24000,48000,96000),big.mark = " "))+
  labs(x="",y="Real GDP per person, 2022 dollars")
ggsave('Figures/Fig08-002.png',width=6,height=3)

# Average annual growth:
wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Fig 8.2")
writeData(wb,"Fig 8.2",plotdata %>%
            select(Year=Ref_Date,Value))
insertImage(wb,"Fig 8.2",'Figures/Fig08-002.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.3 - cyclical behaviour of consumption and investment
data<-get_cansim("36100104")
plotdata<-data %>%
  filter(Estimates %in% c("Services",
                          "Durable goods",
                          "Semi-durable goods",
                          "Non-durable goods",
                          "Gross fixed capital formation"),
         Prices=="Chained (2017) dollars",
         `Seasonal adjustment`=="Seasonally adjusted at annual rates") %>%
  mutate(Estimates=ifelse(Estimates %in% c("Durable goods",
                                           "Semi-durable goods"),
                          "Durable and semi-durable goods",as.character(Estimates))) %>%
  group_by(Date,Estimates) %>%
  summarise(Value=sum(VALUE)) %>%
  filter(Date>='1975-01-01') %>%
  mutate(Date=as.yearqtr(Date))
ggplot(plotdata,aes(Date,Value/1000,group=Estimates,color=Estimates))+
  annotate("text",x=1993,y=675,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  geom_hline(yintercept=0,size=1)+
  geom_line(size=1.5)+
  labs(x="",y="Consumption and investment\n(Billions of 2012 dollars)")
ggsave('Figures/Fig08-003.png',width=6,height=3)

addWorksheet(wb,"Fig 8.3")
writeData(wb,"Fig 8.3",plotdata %>%
            spread(Estimates,Value))
insertImage(wb,"Fig 8.3",'Figures/Fig08-003.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.4 - inventories
plotdata<-data %>%
  filter(Estimates %in% c("Investment in inventories"),
         Prices=="Chained (2017) dollars",
         `Seasonal adjustment`=="Seasonally adjusted at annual rates") %>%
  filter(Date>='1975-01-01') %>%
  mutate(Date=as.yearqtr(Date))
ggplot(plotdata,aes(Date,VALUE/1000,group=Estimates,color=Estimates))+
  annotate("text",x=1993,y=60,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2005,y=32,label="Inventory\ninvestment",color=col[1],size=2.5)+
  geom_hline(yintercept=0,size=1)+
  geom_line(size=1.5,show.legend = F)+
  labs(x="",y="Inventory investment\n(Billions of 2012 dollars)")
ggsave('Figures/Fig08-004.png',width=6,height=3)

addWorksheet(wb,"Fig 8.4")
writeData(wb,"Fig 8.4",plotdata %>%
            select(Date,Estimates,Value=VALUE))
insertImage(wb,"Fig 8.4",'Figures/Fig08-004.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.5 - imports and exports
plotdata<-data %>%
  filter(Estimates %in% c("Exports of goods and services",
                          "Less: imports of goods and services"),
         Prices=="Chained (2017) dollars",
         `Seasonal adjustment`=="Seasonally adjusted at annual rates") %>%
  mutate(Estimates=ifelse(grepl("imports",Estimates),"Imports","Exports")) %>%
  filter(Date>='1975-01-01') %>%
  mutate(Date=as.yearqtr(Date))
ggplot(plotdata,aes(Date,VALUE/1000,group=Estimates,color=Estimates))+
  annotate("text",x=1993,y=700,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2005,y=625,label="Exports",color=col[1],size=2.5)+
  annotate('text',x=2005,y=375,hjust=0,label="Imports",color=col[2],size=2.5)+
  geom_hline(yintercept=0,size=1)+
  geom_line(size=1.5,show.legend = F)+
  labs(x="",y="Exports and imports\n(Billions of 2012 dollars)")
ggsave('Figures/Fig08-005.png',width=6,height=3)

addWorksheet(wb,"Fig 8.5")
writeData(wb,"Fig 8.5",plotdata %>%
            select(Date,Estimates,Value=VALUE) %>%
            spread(Estimates,Value))
insertImage(wb,"Fig 8.5",'Figures/Fig08-005.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.6 - employment
employment<-get_cansim_vector('2062811')
plotdata<-employment %>%
  mutate(Ref_Date=as.yearmon(Date))
ggplot(plotdata,aes(Ref_Date,VALUE/1000))+
  annotate("text",x=1993,y=19.5,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2012,y=16.5,label="Employment",hjust=0,color=col[1],size=2.5)+
  geom_line(size=1.5,color=col[1])+
  labs(x="",y="Employment (millions of people)")
ggsave('Figures/Fig08-006.png',width=6,height=3)

addWorksheet(wb,"Fig 8.6")
writeData(wb,"Fig 8.6",plotdata %>%
            select(Date=Ref_Date,Employment=VALUE))
insertImage(wb,"Fig 8.6",'Figures/Fig08-006.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.7 - unemployment rate
unemployment<-get_cansim_vector('2062815')
plotdata<-unemployment %>%
  mutate(Ref_Date=as.yearmon(Date))
ggplot(plotdata,aes(Ref_Date,VALUE))+
  annotate("text",x=1993,y=13.5,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2012,y=9,label="Unemployment\nrate",hjust=0,color=col[1],size=2.5)+
  geom_line(size=1.5,color=col[1])+
  labs(x="",y="Unemployment rate (percentage of labour force)")
ggsave('Figures/Fig08-007.png',width=6,height=3)

addWorksheet(wb,"Fig 8.7")
writeData(wb,"Fig 8.7",plotdata %>%
            select(Date=Ref_Date,UnempRate=VALUE))
insertImage(wb,"Fig 8.7",'Figures/Fig08-007.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.8 - average labour productivity (real GDP per worker)
RGDP<-get_cansim_vector('62305752')
plotdata<-employment %>%
  mutate(Ref_Date=as.yearqtr(Date)) %>%
  group_by(Ref_Date) %>%
  summarise(Emp=mean(VALUE)) %>%
  left_join(RGDP %>% mutate(Ref_Date=as.yearqtr(Date)) %>% 
              select(Ref_Date,RGDP=VALUE),by="Ref_Date") %>%
  mutate(LabProd=RGDP/Emp)
ggplot(plotdata,aes(Ref_Date,LabProd))+
  annotate("text",x=1993,y=111,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2012,y=100,label="Average labour\nproductivity",hjust=0,color=col[1],size=2.5)+
  geom_line(size=1.5,color=col[1])+
  labs(x="",y="Average labour productivity\n(thousands of 2012 dollars)")
ggsave('Figures/Fig08-008.png',width=6,height=3)

addWorksheet(wb,"Fig 8.8")
writeData(wb,"Fig 8.8",plotdata %>%
            select(Date=Ref_Date,LabProd))
insertImage(wb,"Fig 8.8",'Figures/Fig08-008.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.9 - M2 growth rate
M2p<-get_cansim_vector('41552798')
plotdata<-M2p %>%
  mutate(Ref_Date=as.yearmon(Date)) %>%
  mutate(growth=(VALUE/lag(VALUE,1))^12-1,
         MA=100*rollmean(growth,6,fill=NA,na.pad=F,align='right'))
ggplot(plotdata,aes(Ref_Date,MA))+
  annotate("text",x=1993,y=26,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1974+11/12,xmax=1975+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2012,y=12,label="M2+ growth",hjust=0,color=col[1],size=2.5)+
  geom_line(size=1.5,color=col[1])+
  scale_x_continuous(breaks=seq(1970,2020,10))+
  labs(x="",y="M2+ growth (percent per year)")
ggsave('Figures/Fig08-009.png',width=6,height=3)

addWorksheet(wb,"Fig 8.9")
writeData(wb,"Fig 8.9",plotdata %>%
            select(Date=Ref_Date,M2growth=MA) %>%
            drop_na())
insertImage(wb,"Fig 8.9",'Figures/Fig08-009.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 8.10
bonds<-get_cansim_vector('39065')
plotdata<-bonds %>%
  mutate(Ref_Date=as.yearmon(Date)) %>%
  select(Ref_Date,VALUE) %>%
  drop_na() %>%
  group_by(Ref_Date) %>%
  summarise(Rates=mean(VALUE))
ggplot(plotdata,aes(Ref_Date,Rates))+
  annotate("text",x=1993,y=22,hjust=0,label="Recessions",color="dodgerblue",alpha=0.5,size=2.5)+
  annotate("rect",xmin=1960+2/12,xmax=1961+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1974+11/12,xmax=1975+2/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1980+0/12,xmax=1980+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1981+5/12,xmax=1982+9/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=1990+2/12,xmax=1992+3/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2008+9/12,xmax=2009+4/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=2020+1/12,xmax=2020+5/12,ymin=-Inf,ymax=Inf,alpha=0.2, fill="dodgerblue")+
  annotate('text',x=2013,y=3,label="Nominal\ninterest rate",color=col[1],size=2.5)+
  geom_line(size=1.5,color=col[1])+
  labs(x="",y="Nominal interest rate\n(percentage per year)")
ggsave('Figures/Fig08-010.png',width=6,height=3)

addWorksheet(wb,"Fig 8.10")
writeData(wb,"Fig 8.10",plotdata)
insertImage(wb,"Fig 8.10",'Figures/Fig08-010.png',
            startCol = 7,startRow=7,width=10,height=5)

##########################
# Save the full workbook #
##########################
saveWorkbook(wb,"Data/C08 Figures.xlsx",overwrite=T)
