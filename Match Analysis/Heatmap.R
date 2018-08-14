Hmap=function(Fdata){
  
  lawn=readJPEG("field.jpg")  
  ggplot()+
    coord_cartesian(xlim=c(-52.5,52.5),ylim=c(-34,34))+
    xlab("")+ylab("")+
    annotation_custom(rasterGrob(lawn,width=unit(1,"npc"),height=unit(1,"npc"),interpolate=FALSE), -Inf, Inf, -Inf, Inf)+
    stat_density_2d(data=Fdata,aes(x=Fdata[,1],y=Fdata[,2],fill=..level..,alpha=..level..),size=0.01,bins=100,geom="polygon",show.legend=FALSE )+
    scale_fill_gradient(low = "green", high = "red")+
    theme_bw()+
    theme(panel.grid=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),panel.background = element_blank(),
          plot.background=element_blank())
}

