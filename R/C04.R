# Setup the environment
source("R/Setup.R")

# Figure 4.4 - gross net investment as % of GDP, adjusted to be quarterly
# Tables 36-10-0103-01 and 36-10-0104-01
biz_investment<-get_cansim_vector("v62305764")
depreciation<-get_cansim_vector(c("v62295554","v62295558"))
NGDP<-get_cansim_vector("v62305783")
plotdata<-NGDP %>%
  select(Date,NGDP=VALUE) %>%
  left_join(biz_investment %>%
              select(Date,Gross=VALUE),by="Date") %>%
  left_join(depreciation %>%
              group_by(Date) %>%
              summarise(Depreciation=sum(VALUE)),by="Date") %>% 
  mutate(Net=(Gross-Depreciation)/NGDP,
         Gross=Gross/NGDP) %>%
  select(Date,Gross,Net) %>%
  gather(type,value,-Date)
ggplot(plotdata,aes(Date,100*value,group=type,color=type))+
  geom_line(size=1.5)+
  geom_hline(yintercept=0,size=1)+
  scale_y_continuous()+
  scale_color_manual(labels=c("Gross investment","Net investment"),values=col[1:2])+
  labs(x="",y="Percent")
ggsave('Figures/Fig04-004.png',width=9,height=4.5)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Figure 4.4")
writeData(wb,"Figure 4.4",plotdata %>% 
            mutate(value=100*value) %>%
            spread(type,value))
insertImage(wb,"Figure 4.4",'Figures/Fig04-004.png',
            startCol = 7,startRow=7,width=10,height=5)

# Save the full workbook
saveWorkbook(wb,"Data/C04 Figures.xlsx",overwrite=T)
