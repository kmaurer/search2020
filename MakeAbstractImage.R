Original <- oxford_real_estate %>%
  mutate(x=my_house_std$sqft,
         y=my_house_std$lot_size,
         xend=std_sqft,
         yend=std_lot,
         type="Original") %>%
  select(x,xend,y,yend,sale_price,type)

ggplot()+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend, color=sale_price),
                size=1,
             data=Original)+
  scale_color_viridis_c("Price",
                        limits = c(0,400000),
                        breaks = seq(0,400000,by=100000),
                        labels = paste0("$",seq(0,400,by=100),"k"))+
  theme_void()+
  theme(legend.position = "None")

ggsave(file="TeachingDemoAbstractImage.png",dpi=600,
       width=6, height=6,units="in")
