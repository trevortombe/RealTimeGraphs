# Setup the environment
source("R/Setup.R")

# Fetch Statistics Canada data relevant for this chapter
inflation_data<-get_cansim_vector('v41690973')
yields_data<-get_cansim_vector('v122485') # 3-5 year govt yield
gdp_annualized<-get_cansim_vector('v62305752')

# Figure 1: Real gdp growth
fig<-gdp_annualized %>%
  mutate(growth=100*((VALUE/lag(VALUE,1))^4-1),
         Ref_Date=as.yearmon(Date)) %>%
  select(Ref_Date,growth,VALUE) %>% drop_na()
ggplot(fig,aes(Ref_Date,growth))+
  annotate("rect",xmin=as.numeric(as.yearmon("Feb 2020")), xmax=as.numeric(as.yearmon("May 2020")), 
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  annotate("rect",xmin=as.numeric(as.yearmon("Oct 2008")), xmax=as.numeric(as.yearmon("May 2009")),
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  annotate("rect",xmin=as.numeric(as.yearmon("Mar 1990")), xmax=as.numeric(as.yearmon("Apr 1992")),
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  annotate("rect",xmin=as.numeric(as.yearmon("Jun 1981")), xmax=as.numeric(as.yearmon("Oct 1982")),
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  annotate("rect",xmin=as.numeric(as.yearmon("Jan 1980")), xmax=as.numeric(as.yearmon("Jun 1980")),
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  annotate("rect",xmin=as.numeric(as.yearmon("Dec 1974")), xmax=as.numeric(as.yearmon("Mar 1975")),
           ymin=-Inf, ymax=+Inf, alpha=0.35, fill="gray")+
  geom_hline(yintercept = 0,size=1)+
  annotate('text',y=37,x=1993,hjust=0,
           color='gray40',size=3,label="Recessions")+
  geom_line(size=1.5,color=col[2])+
  labs(x="Year",y="Real GDP growth (percent per year)")
ggsave('Figures/Fig02-001.png',width=6,height=3)

wb<-createWorkbook()
addWorksheet(wb,"Figure 2.1")
writeData(wb,"Figure 2.1",
          fig %>% select(Date=Ref_Date,growth))
insertImage(wb,"Figure 2.1",'Figures/Fig02-001.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 2: Inflation Over Time
fig<-inflation_data %>%
  mutate(inflation=100*(VALUE/lag(VALUE,12)-1))
ggplot(fig,aes(Date,inflation))+
  geom_hline(yintercept = 0,size=1)+
  geom_line(size=1.5,color=col[2])+
  scale_y_continuous(limit=c(NA,25))+
  annotate('text',y=24,x=as.Date('1918-01-01'),hjust=0,size=3,label="World War I")+
  annotate('text',y=-15,x=as.Date('1924-01-01'),hjust=0,size=3,label="Post World War I")+
  annotate('text',y=-8,x=as.Date('1936-01-01'),hjust=0,size=3,label="Great Depression")+
  annotate('text',y=19,x=as.Date('1947-01-01'),hjust=0,size=3,label="Post World War II")+
  annotate('text',y=13,x=as.Date('1954-01-01'),hjust=0,size=3,label="Korean war")+
  annotate('text',y=15,x=as.Date('1973-01-01'),hjust=0,size=3,label="Oil price shocks of 1973 and 1979")+
  annotate('text',y=-3,x=as.Date('1991-01-01'),hjust=0,size=3,label="Inflation targeting begins")+
  annotate('text',y=9,x=as.Date('2021-01-01'),hjust=1,size=3,label="Post-COVID")+
  labs(x="Year",y="Inflation rate (percentage change)")
ggsave('Figures/Fig02-002.png',width=6,height=3)

addWorksheet(wb,"Figure 2.2")
writeData(wb,"Figure 2.2",
          fig %>% select(Date,inflation) %>% drop_na())
insertImage(wb,"Figure 2.2",'Figures/Fig02-002.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 3: Nominal and Real Interest Rates
fig<-yields_data %>%
  select(Date,nominal=VALUE) %>%
  left_join(inflation_data %>%
              mutate(inf_avg_3yr=100*((VALUE/lag(VALUE,36))^(1/3)-1)) %>% 
              select(Date,inf_avg_3yr),by='Date') %>%
  mutate(real=nominal-inf_avg_3yr)
ggplot(fig,aes(Date,real))+
  geom_hline(yintercept = 0,size=1)+
  geom_line(size=1.5,color=col[2])+
  geom_line(size=1.5,color=col[1],aes(y=nominal))+
  scale_y_continuous(limit=c(NA,20))+
  annotate('text',y=-3,x=as.Date('1978-01-01'),hjust=0,fontface='bold',
           color=col[2],size=3,label="Real interest rate")+
  annotate('text',y=18,x=as.Date('1985-01-01'),hjust=0,fontface='bold',
           color=col[1],size=3,label="Nominal interest rate")+
  labs(x="Year",y="Interest rate (percent per year)")
ggsave('Figures/Fig02-003.png',width=6,height=3)

addWorksheet(wb,"Figure 2.3")
writeData(wb,"Figure 2.3",
          fig %>% select(Date,real,nominal))
insertImage(wb,"Figure 2.3",'Figures/Fig02-003.png',
            startCol = 7,startRow=7,width=10,height=5)

# Save the full workbook
saveWorkbook(wb,"Data/C02 Figures.xlsx",overwrite=T)
