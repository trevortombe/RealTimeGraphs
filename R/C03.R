# Setup the environment
source("R/Setup.R")

# Figure 3.10 - historical oil prices 1861 to present
# Load the base data from spreadsheet, then update
basedata<-read_excel('Data/BaseData.xlsx',sheet="Figure 3-10")
nominal_data<-fredr("DCOILBRENTEU")
nominal_annual<-nominal_data %>%
  mutate(Year=year(as.Date(date))) %>%
  select(date,Year,value) %>% drop_na() %>%
  group_by(Year) %>%
  summarise(nominal=mean(value))
cpi_data<-get_cansim_vector('v41690914') %>%
  mutate(Year=year(as.Date(Date))) %>%
  group_by(Year) %>%
  summarise(CPI=mean(VALUE))
splice_year<-max(basedata$Year)
plotdata<-basedata %>%
  full_join(nominal_annual %>% rename(newoil=nominal) %>%
              filter(Year>=max(basedata$Year)),by="Year") %>%
  full_join(cpi_data %>% rename(newcpi=CPI) %>%
              filter(Year>=max(basedata$Year)),by="Year") %>%
  mutate(CPI=ifelse(Year>splice_year,(CPI[Year==splice_year]/newcpi[Year==splice_year])*newcpi,CPI),
         Nominal=ifelse(Year>splice_year,newoil,Nominal),
         Real=ifelse(Year>splice_year,Nominal*(CPI[Year==2022]/CPI),Real)) %>%
  select(Year,Nominal,Real) %>%
  gather(type,value,-Year)
ggplot(plotdata,aes(Year,value,group=type,color=type))+
  geom_hline(yintercept=0,size=1)+
  geom_line(size=1.5)+
  annotate('text',y=155,x=1863,hjust=0,size=2.5,label="U.S. Civil War")+
  annotate('text',y=140,x=1870,hjust=0,size=2.5,label="Pennsylvanian\noil boom")+
  annotate('text',y=90,x=1882,hjust=0,size=2.5,label="Russian oil\nexports begin")+
  annotate('text',y=65,x=1903,size=2.5,label="Shell and Standard Oil\ncontrol oil markets")+
  annotate('text',y=10,x=1911,size=2.5,label="Break-up of\nStandard Oil")+
  annotate('text',y=55,x=1931-5,size=2.5,label="East Texas\ndiscoveries")+
  annotate('text',y=35,x=1938,size=2.5,label="Middle East\ndiscoveries")+
  annotate('text',y=10,x=1947,size=2.5,label="Alberta's\nLeduc No. 1")+
  annotate('text',y=75,x=1973-7,size=2.5,label="First\nOPEC shock")+
  annotate('text',y=140,x=1979,size=2.5,label="Second\nOPEC shock")+
  annotate('text',y=75,x=1986+7,size=2.5,label="1986 price\ncollapse")+
  annotate('text',y=150,x=2005,size=2.5,label="Rapid growth\nof China, India")+
  annotate('text',y=30,x=2015,size=2.5,label="2015 price\ncollapse")+
  annotate('text',y=115,x=2022+1,size=2.5,label="Russia\ninvades\nUkraine")+
  scale_x_continuous(breaks=seq(1870,2020,15))+
  scale_color_manual(values=col[1:2],label=c("Nominal dollars","Real, 2022 dollars"))+
  theme(legend.position = 'top')+
  labs(x="Year",y="Dollars per barrel")
ggsave('Figures/Fig03-010.png',width=7,height=5)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Figure 3.10")
writeData(wb,"Figure 3.10",plotdata %>% spread(type,value))
insertImage(wb,"Figure 3.10",'Figures/Fig03-010.png',
            startCol = 7,startRow=7,width=7,height=5)

##########################
# Save the full workbook #
##########################
saveWorkbook(wb,"Data/C03 Figures.xlsx",overwrite=T)

