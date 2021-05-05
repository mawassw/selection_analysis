#plot n of individuals per year
library(ggplot2)
#charlevoix
nind_char <-  data.frame(table(ped_charlevoix$yob,useNA = "no"))
colnames(nind_char) <- c("yob", "N")
nind_char$yob <-as.numeric(as.character(nind_char$yob))

tiff("nind_char.tiff", units="in", width = 5, height = 5, res=300)
ggplot(nind_char, aes(x=yob, y=N))+
  geom_line()+xlab("Year of birth")+ylab("Number of individuals")+
  annotate("pointrange", x=1841, xmin=1837, xmax=1871, y=0, color = "cyan3", size =1, alpha=0.5)+
  annotate("text", x=1841, y=10, label = "Immigration event", size = 2)+
  scale_y_continuous(breaks = c(seq(0,600,by=100)))+
  theme_classic()
dev.off()

#saguenay
nind_sag <-  data.frame(table(ped_saguenay$yob,useNA = "no"))
colnames(nind_sag) <- c("yob", "N")
nind_sag$yob <-as.numeric(as.character(nind_sag$yob))

tiff("nind_sag.tiff", units="in", width = 5, height = 5, res=300)
ggplot(nind_sag, aes(x=yob, y=N))+
  geom_line()+xlab("Year of birth")+ylab("Number of individuals")+
  annotate("pointrange", x=1841, xmin=1837, xmax=1871, y=0, color = "cyan3", size =1, alpha=0.5)+
  annotate("text", x=1841, y=10, label = "Immigration event", size = 2)+
  scale_y_continuous(breaks = c(seq(0,9000,by=1000)))+
  theme_classic()
dev.off()
