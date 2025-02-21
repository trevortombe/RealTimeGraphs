# Setup the environment
source("R/Setup.R")

# Figure for A Closer Look 13.2 on inflation decomposition
# Decomposition of to energy, groceries, goods ex energy/food, services ex energy/shelter, shelter
cpi_data<-get_cansim("18100004")
weights_cpi<-get_cansim("18100007")
inf_rates<-cpi_data %>%
  filter(`Products and product groups`=="All-items",
         GEO=="Canada") %>%
  mutate(YoY=VALUE/lag(VALUE,12)-1,
         change=YoY-lag(YoY,12)) %>%
  select(Date,YoY,change,CPI=VALUE) %>%
  mutate(Ref_Date=as.yearmon(Date)) %>%
  drop_na()
weights_monthly<-weights_cpi %>%
  filter(GEO=="Canada",
         `Geographic distribution of weight`=="Distribution to selected geographies",
         `Price period of weight`=="Weight at basket link month prices") %>%
  mutate(basket=year(Date)) %>%
  select(basket,product=`Products and product groups`,w=VALUE)
link_months<-data.frame(
  Ref_Date=seq(as.yearmon("1978-01"),as.yearmon(max(cpi_data$REF_DATE)),1/12)
) %>%
  mutate(basket=case_when(
    Ref_Date>="Oct 1978" & Ref_Date<"Apr 1982" ~ 1974,
    Ref_Date>="Apr 1982" & Ref_Date<"Jan 1985" ~ 1978,
    Ref_Date>="Jan 1985" & Ref_Date<"Jan 1989" ~ 1982,
    Ref_Date>="Jan 1989" & Ref_Date<"Jan 1995" ~ 1986,
    Ref_Date>="Jan 1995" & Ref_Date<"Jan 1998" ~ 1992,
    Ref_Date>="Jan 1998" & Ref_Date<"Jan 2003" ~ 1996,
    Ref_Date>="Jan 2003" & Ref_Date<"Apr 2007" ~ 2001,
    Ref_Date>="Apr 2007" & Ref_Date<"Apr 2011" ~ 2005,
    Ref_Date>="Apr 2011" & Ref_Date<"Jan 2013" ~ 2009,
    Ref_Date>="Jan 2013" & Ref_Date<"Dec 2014" ~ 2011,
    Ref_Date>="Dec 2014" & Ref_Date<"Dec 2016" ~ 2013,
    Ref_Date>="Dec 2016" & Ref_Date<"Dec 2018" ~ 2015,
    Ref_Date>="Dec 2018" & Ref_Date<"Jun 2021" ~ 2017,
    Ref_Date>="Jun 2021" & Ref_Date<"May 2022" ~ 2020,
    Ref_Date>="May 2022" & Ref_Date<"May 2023" ~ 2021,
    Ref_Date>="May 2023" & Ref_Date<"May 2024" ~ 2022,
    Ref_Date>="May 2024" ~ 2023
  )) %>%
  group_by(basket) %>%
  mutate(link_month=min(Ref_Date)) %>% ungroup()
decomp_cpi<-cpi_data %>%
  filter(GEO=="Canada") %>%
  mutate(Ref_Date=as.yearmon(Date)) %>%
  select(Ref_Date,product=`Products and product groups`,Value=VALUE) %>%
  left_join(link_months,by="Ref_Date") %>%
  filter(!is.na(basket)) %>%
  left_join(weights_monthly,by=c("product","basket")) %>%
  group_by(Ref_Date) %>%
  mutate(all=weighted.mean(Value,product=="All-items")) %>%
  group_by(product) %>%
  mutate(period=cumsum(ifelse(Ref_Date==link_month,1,0))) %>%
  group_by(product,period) %>%
  mutate(I_atlink=Value[1],
         all_atlink=all[1]) %>%
  group_by(product) %>%
  mutate(cpi=all/lag(all,12)-1,
         relimp=(w/100)*(Value/I_atlink)/(all/all_atlink), # relative importance
         relimp_old=lag(relimp,12), # relative importance using weights and prices from t-12
         relimp_new=(w/100)*(lag(Value,12)/I_atlink)/(lag(all,12)/all_atlink)) %>% # relative importance using current weights but t-12 prices) %>%
  mutate(contrib_old=(I_atlink/lag(Value,12)-1)*relimp_old, #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_new=(Value/I_atlink-1)*(w/100)*(all_atlink/lag(all,12)), #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_cross=contrib_new+contrib_old, 
         contrib_nocross=(Value/lag(Value,12)-1)*relimp_old, 
         contrib_check=ifelse(basket!=lag(basket,12),contrib_cross,contrib_nocross), # verify statcan same as your main approach
         effective_weight=(relimp_old/(Value/I_atlink)+(1-1/(Value/I_atlink))*relimp_new), # an intuitive way? same as statcan approach
         change=Value/lag(Value,12)-1,
         MoM=Value/lag(Value,1)-1,
         contrib=(1+change)*effective_weight-relimp_old, # main estimate
         value_if_2prc=lag(Value,12)*1.02,
         weight_if_2prc=(relimp_old/(value_if_2prc/I_atlink)+(1-1/(value_if_2prc/I_atlink))*relimp_new),
         contrib_if_2prc=1.02*weight_if_2prc-relimp_old,
         excluding_item=(cpi-contrib)/(1-effective_weight))
plotdata<-decomp_cpi %>%
  filter(product %in% c("Food purchased from stores",
                        "Energy",
                        "Rented accommodation",
                        "Owned accommodation","Water",
                        "Goods excluding food purchased from stores and energy")) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(contrib),
         `Services, excluding shelter`=cpi-total) %>%
  select(Ref_Date,product,contrib,`Services, excluding shelter`,cpi) %>%
  spread(product,contrib) %>%
  gather(product,contrib,-Ref_Date,-cpi) %>%
  mutate(product=ifelse(product %in% c("Rented accommodation",
                                       "Owned accommodation","Water"),
                        "Shelter, excluding energy",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(contrib=sum(contrib),
            cpi=mean(cpi)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2015") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Groceries",
    product=="Goods excluding food purchased from stores and energy" ~ "Goods, excluding groceries and energy",
    TRUE ~ product
  )) %>%
  filter(!is.na(cpi)) %>%
  mutate(product=factor(product,levels=c("Services, excluding shelter",
                                         "Shelter, excluding energy",
                                         "Groceries",
                                         "Goods, excluding groceries and energy",
                                         "Energy"))) # ensure negatives are at bottom
ggplot(plotdata,aes(Ref_Date,contrib,group=product,fill=product))+
  geom_col(position='stack')+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=cpi),size=1.5)+
  scale_y_continuous(label=percent,breaks=pretty_breaks(5))+
  scale_x_continuous(breaks=pretty_breaks(6))+
  annotate('text',x=2021.5,hjust=1,y=0.05,label="All-Items CPI",size=3)+
  labs(x="",
       y="Year-over-Year Change")
ggsave('Figures/Fig13-CloserLook-002.png',width=8,height=4)

wb<-createWorkbook() # create fresh workbook
addWorksheet(wb,"Fig CloserLook 13.2")
writeData(wb,"Fig CloserLook 13.2",plotdata %>% 
            spread(product,contrib) %>%
            rename(Date=Ref_Date,`All-items CPI`=cpi))
insertImage(wb,"Fig CloserLook 13.2",'Figures/Fig13-CloserLook-002.png',
            startCol = 7,startRow=7,width=10,height=5)

##########################
# Save the full workbook #
##########################
saveWorkbook(wb,"Data/C13 Figures.xlsx",overwrite=T)
