library(ggplot2)
library(tidyverse)
library(ggthemes) 
library(extrafont)
library(ztable)

load("carsale.Rda")
mydata = na.omit(mydata)  #去掉所有NA 

g <- ggplot(mydata,aes(size))
g + geom_bar(position = "dodge" ,aes(fill = transmission),width = 0.7)
# library(scales)
color <- c("turquoise", "slategray2", "azure3")
g + geom_bar(position = "fill" ,aes(fill = transmission)) + scale_fill_manual(values=color) + scale_y_continuous(labels=percent)

### theme ###
compare <- filter(mydata, maker == "honda"|maker == "hyundai")

g2 <- ggplot(compare,aes(maker))
g2 + geom_bar(position = "dodge",aes(fill = transmission)) +
 ggtitle(" Honda V.S Hyundai") +
  theme_economist()

# position = "stack" , "dodge", position_dodge(width = 1)
# subtitle = "JP & KR"
# theme_solarized() 

### fonts % theme() ###
# install.packages("extrafont")
loadfonts(device="win")
fonts()  

g <- ggplot(mydata, aes(x = price)) 
g + geom_histogram()+ ylab("counts")+ggtitle("Price avg.")+
  theme(plot.title=element_text(family="Comic Sans MS", face="bold", size=15))

# theme(text=element_text(size=16, family="Comic Sans MS"))
# font_import(pattern="[C/c]omic")  # for mac
# font_import(pattern="[A/a]rial")  

### facet & axis ###
compare2 <- filter(mydata,maker == "honda"|maker == "hyundai"| 
                     maker == "bmw"|maker == "ford")

g <- ggplot(compare2,aes(price,colour = maker, fill = maker))
g + geom_density() + ggtitle("Japan/Korea/German/USA car price")  

g + geom_density()+facet_wrap(~maker) + ggtitle("Japan/Korea/German/USA car price")+
  theme(axis.text.y=element_blank())  # 隱藏y軸的機率值

### lm ###

g <- ggplot(mpg, aes(displ, hwy)) 
  g + geom_point() +
  geom_smooth(method = "lm", se = TRUE) + # se : standard error
  labs(caption = "(based on data from ...)")
  
### Rcolor ###  
p <- ggplot(mydata,aes(type,price))
p + geom_boxplot(fill = "white", colour = cm.colors(13)) 

mydata$type %>% unique()
# cm.colors

### ztable ###
# library(ztable)
z = ztable(head(iris),align = "cccccc")
z

cgroup = c("Sepal","Petal","Species")
n.cgroup = c(2,2,1)
z = addcgroup(z,cgroup = cgroup,n.cgroup = n.cgroup)
z

### websites ###

# [http://ggplot2.tidyverse.org/index.html](http://ggplot2.tidyverse.org/index.html)  
# [https://cran.r-project.org/web/packages/ztable/vignettes/ztable.html](https://cran.r-project.org/web/packages/ztable/vignettes/ztable.html)
# [http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
# [https://cran.r-project.org/web/packages/ztable/vignettes/ztable.html](https://cran.r-project.org/web/packages/ztable/vignettes/ztable.html)
# [https://www.r-graph-gallery.com/](https://www.r-graph-gallery.com/)



