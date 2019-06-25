Kv4_20190516_1n<-read.table('kv4_20190516_1n.dat',header=T)
dat<-Kv4_20190516_1n
dat$Idensity<-dat$reci*1000/12
dat$gbarkv4<-dat$recscale*1000
dat$reclat<-dat$reclat-100
fac_exp<-as.factor(dat$gbarkv4)
levels(fac_exp)

df<-as.data.frame(matrix(,ncol=nrow(dat)/length(levels(fac_exp)),nrow=length(levels(fac_exp))))
colnames(df)<-dat$Idensity[dat$gbarkv4==levels(fac_exp)[1]]
row.names(df)<-levels(fac_exp)

for (i in 1:length(levels(fac_exp))) {
  df[row.names(df)==levels(fac_exp)[i],]<-dat$reclat[dat$gbarkv4==levels(fac_exp)[i]]
}

kv4_exp<-dat[dat$gbarkv4%in%c(0,11),]
kv4_exp$gbarkv4<-as.factor(kv4_exp$gbarkv4)
kv4_exp<-kv4_exp[-c(1:4,52:55),]
kv4_exp<-kv4_exp[-which(kv4_exp$Idensity>35),]

p<-ggplot(kv4_exp,aes(Idensity,reclat,group=gbarkv4))+geom_line(aes(color = gbarkv4),size=0.5)+
  labs(x="Current Density (pA/pF)",y="Latency of 1st spike (ms)",color=bquote(bar('g')*'Kv4 (mS/'*cm^2*')'))+
  theme_bw()+
  scale_color_manual(values=c("red","black"))+
  theme(axis.title=element_text(color='black',size=14),
        axis.text=element_text(color='black',size=14),
        legend.title=element_text(size=14),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
p

cairo_pdf(filename = 'Kv4_CLTMR_line_20190516.pdf',
          width=8.4,height=4.2,family = 'Arial Unicode MS')
p
dev.off()

df<-df[,-(1:4)]
colnames(df) = c('4',rep('',5),'10',rep('',9),'20',rep('',9),'30',rep('',9),'40',rep('',9),'50')
ht = Heatmap(df, 
             #name = "Number of\nAPs",
             name = "Onset Latency(ms)",
             row_names_side = "left",
             column_title = "Current Density (pA/pF)",
             column_title_side = "bottom",
             row_title = expression(paste(bar(g),'Kv4 (mS/',cm^2,')',sep='')),
             col = blue2green2red(60),
             na_col= "black",
             cluster_rows = FALSE,
             cluster_columns = FALSE, 
             show_heatmap_legend = T,
             heatmap_legend_param =list(at=c(0,100,200,300),legend_height=unit(10, "cm"),
                                        labels_gp = gpar(fontsize = 15, fontface='bold'),
                                        title_gp = gpar(fontsize = 15, fontface='bold')),
             row_names_gp = gpar(fontsize = 15, fontface='bold'),
             column_names_gp = gpar(fontsize = 15, fontface='bold'),
             row_title_gp = gpar(fontsize = 15, fontface='bold'),
             column_title_gp = gpar(fontsize = 15, fontface='bold')
)

ht

cairo_pdf(filename = 'kv1_20190516.pdf',
          width=8.5,height=6,family = 'Arial Unicode MS')
draw(ht)
dev.off()