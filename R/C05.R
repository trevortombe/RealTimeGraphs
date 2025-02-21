# Setup the environment
source("R/Setup.R")

# Figure 5.1 - interest rates in canada and the US
US<-fredr('TB3MS')
yields<-get_cansim("10100122") %>%
  filter(Rates=="Treasury bills: 3 month")
plotdata<-yields %>%
  select(Date,Canada=VALUE) %>%
  left_join(US %>% select(Date=date,USA=value),by="Date") %>%
  mutate(gap=Canada-USA) %>%
  gather(country,value,-Date,-gap) %>%
  drop_na()
ggplot(plotdata,aes(Date,value,group=country,color=country))+
  geom_col(data=filter(plotdata,country=='Canada'),aes(Date,gap),inherit.aes = F,
           fill='gray70')+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',x=as.Date("1994-01-01"),y=12.5,label="Canada",size=3,color=col[1],fontface='bold')+
  annotate('text',x=as.Date("1982-01-01"),y=6,label="United States",size=3,color=col[2],fontface='bold')+
  annotate('text',x=as.Date("1992-01-01"),y=-1,label="Interest rate gap",size=3,color='gray50',fontface='bold')+
  geom_hline(yintercept=0,size=1)+
  scale_x_date(date_breaks = '5 years',date_labels = "%Y")+
  labs(y="Percent",x="Year")
ggsave('Figures/Fig05-001.png',width=9,height=4.5)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Figure 5.1")
writeData(wb,"Figure 5.1",plotdata %>% 
            spread(country,value) %>%
            rename(`Interest rate gap`=gap))
insertImage(wb,"Figure 5.1",'Figures/Fig05-001.png',
            startCol = 7,startRow=7,width=10,height=5)

# Figure 5.6 - international direct investment
old_series<-get_cansim('36100073')
new_series<-get_cansim('36100008')
fon_macro<-read.csv('https://osf.io/2sm84/download')
NGDP<-get_cansim_vector("v62305783") %>% # latest nominal GDP to supplement the FoN
  mutate(date=year(Date)) %>%
  group_by(date) %>%
  summarise(NGDP=mean(VALUE))
plotdata<-new_series %>%
  filter(`Canadian and foreign direct investment` %in% c("Canadian direct investment abroad - total book value",
                                                         "Foreign direct investment in Canada - total book value"),
         `Countries or regions`=="All countries") %>%
  mutate(type=ifelse(grepl("Canadian",`Canadian and foreign direct investment`),"Canada","Foreign")) %>%
  select(date=REF_DATE,type,Value=VALUE) %>%
  mutate(date=as.numeric(date)) %>%
  bind_rows(
    old_series %>%
      filter(`Canada's international investment position` %in% c("Canadian direct investment abroad",
                                                                 "Foreign direct investment in Canada"),
             `Countries or regions`=="All countries") %>%
      mutate(type=ifelse(grepl("Canadian",`Canada's international investment position`),"Canada","Foreign")) %>%
      select(date=REF_DATE,type,Value=VALUE) %>%
      mutate(date=as.numeric(date)) %>%
      filter(date<=1986)
  ) %>%
  arrange(date,type) %>%
  filter(date>=1945) %>%
  left_join(
    fon_macro %>%
      filter(provname=="Canada",item=="Nominal GDP (Millions)",
             transformation=="linear") %>%
      select(date,NGDP=val),by='date'
  ) %>%
  left_join(NGDP %>% rename(latest=NGDP),by="date") %>%
  mutate(NGDP=ifelse(date>=1961,latest,NGDP)) %>%
  mutate(share=Value/NGDP) %>%
  group_by(date) %>%
  mutate(gap=share[2]-share[1]) # net foreign investment
ggplot(plotdata,aes(date,100*share,group=type,color=type))+
  geom_col(data=filter(plotdata,type=='Canada'),aes(date,100*gap),inherit.aes = F,
           fill='gray70',width=0.5)+
  geom_line(size=1.5,show.legend = F)+
  annotate('text',x=2007,y=65,label="Canadian direct\ninvestment abroad (CDIA)",size=3,color=col[1],fontface='bold')+
  annotate('text',x=1980,y=35,label="Foreign direct\ninvestment in Canada (FDI)",size=3,color=col[2],fontface='bold')+
  annotate('text',x=2000,y=-15,
           label="Net foreign direct\ninvestment in Canada",size=3,color='gray50',fontface='bold')+
  geom_hline(yintercept=0,size=1)+
  scale_x_continuous(breaks=seq(1950,2020,10))+
  labs(y="Percent",x="Year")
ggsave('Figures/Fig05-006.png',width=9,height=4.5)

addWorksheet(wb,"Figure 5.6")
writeData(wb,"Figure 5.6",plotdata %>% 
            select(Year=date,type,share,gap) %>%
            spread(type,share) %>%
            rename(`Net foreign direct investment in Canada`=gap))
insertImage(wb,"Figure 5.6",'Figures/Fig05-006.png',
            startCol = 7,startRow=7,width=10,height=5)

# Save the full workbook
saveWorkbook(wb,"Data/C05 Figures.xlsx",overwrite=T)
