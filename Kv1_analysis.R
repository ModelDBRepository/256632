
Kv1_20190516_1n<-read.table('kv1_20190516_1n.dat',header=T)
dat<-Kv1_20190516_1n
dat$Idensity<-dat$reci*1000/30
dat$gbarkv1<-dat$recscale*1000
dat$reclat<-dat$reclat-100
fac_exp<-as.factor(dat$gbarkv1)
levels(fac_exp)

df<-as.data.frame(matrix(,ncol=nrow(dat)/length(levels(fac_exp)),nrow=length(levels(fac_exp))))
colnames(df)<-dat$Idensity[dat$gbarkv1==levels(fac_exp)[1]]
row.names(df)<-levels(fac_exp)

for (i in 1:length(levels(fac_exp))) {
  df[row.names(df)==levels(fac_exp)[i],]<-dat$recn[dat$gbarkv1==levels(fac_exp)[i]]
}

kv1_exp<-dat[dat$gbarkv1%in%c(0,6),]
kv1_exp$gbarkv1<-as.factor(kv1_exp$gbarkv1)

p<-ggplot(kv1_exp,aes(Idensity,recn,group=gbarkv1))+geom_line(aes(color = gbarkv1),size=0.5)+
  labs(x="Current Density (pA/pF)",y="Number of APs",color=bquote(bar('g')*'Kv1 (mS/'*cm^2*')'))+
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

cairo_pdf(filename = 'Kv1_AbLTMR_line_20190516.pdf',
          width=8.4,height=4.2,family = 'Arial Unicode MS')
p
dev.off()

df[df==0]<-NA
colnames(df) = c('0',rep('',9),'10',rep('',9),'20',rep('',9),'30',rep('',9),'40',rep('',9),'50')
ht = Heatmap(df, 
             name = "Number of\nAPs",
             #name = "Onset Latency(ms)",
             row_names_side = "left",
             column_title = "Current Density (pA/pF)",
             column_title_side = "bottom",
             row_title = expression(paste(bar(g),'Kv1 (mS/',cm^2,')',sep='')),
             col = blue2green2red(60),
             na_col= "black",
             cluster_rows = FALSE,
             cluster_columns = FALSE, 
             show_heatmap_legend = T,
             heatmap_legend_param =list(at=c(1,10,20,30,40,50,60),legend_height=unit(10, "cm"),
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