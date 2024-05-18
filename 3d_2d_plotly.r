rm(list=ls())

library(tidyverse)
library(tidyr)
library(lubridate)  #helps wrangle date attributes
library(data.table) #for loading and mapping data
library(htmlwidgets)
library(plotly)
library(patchwork)
library(stringr)


df_6.304 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.304.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)
df_6.354 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.354.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)
df_6.404 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.404.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)
df_6.454 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.454.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)
df_6.504 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.504.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)
df_6.554 <- read_csv("D:/Non_Documents/엑소/mla/MLA/axo/z_6.554.csv", col_types = cols(.default = "n"), skip = 35,  col_names = F)


df <- list(df_6.304, df_6.354)
class(df)


plot_ly(
  z = ~ scale(as.matrix(df)),
  x = ~ as.numeric(1:nrow(df)),
  y = ~ as.numeric(1:ncol(df)),
  type = "surface",
  colorbar = list(title = "My surface plot")
) %>% layout(scene = list(
  xaxis = list(title = "X-Axis"),
  yaxis = list(title = "Y-Axis"),
  zaxis = list(title = "Z-Axis")
))

dev.off()
fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~ as.matrix(df_6.304))
fig <- fig %>% add_surface(z = ~as.matrix(df_6.354+50), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(df_6.404+100), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(df_6.454+150), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(df_6.504+200), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(df_6.554+250), opacity = 0.7)

fig

fig <- plot_ly(z=as.matrix(df_6.554)) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)


fig



##########################################################


tbl <-
  list.files(path = path,
             pattern = "\\.csv$",
             full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = "n"),  skip = 35,  col_names = F))

class(tbl)
####################
raw.files <- data_frame(filename = list.files(path))

raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0(path, "/", filename))


raw.data <- raw.file.paths %>%
  # 'do' the function for each row in turn
  rowwise() %>%
  do(., read_csv(file=.$filepath, skip = 35, col_names=F))


#################
rm(list=ls())

## 여러 csv 파일 불러오기
setwd("D:\\Non_Documents\\엑소\\mla\\data\\")  ## Working directory setting
path <- getwd()
list.files() ## file list
file_list<-list.files() ## file list
file_list

for (i in 1:length(file_list)){
  print(i)
  assign(paste0("lens_",i),
         read_csv(file_list[i], skip = 35, col_names = F))

}



eval(parse(text=paste0("lens_", i)))




fig <- plot_ly(showscale = FALSE)

for (i in 1:9){
  fig_B <- fig %>%
    add_surface(z = ~ as.matrix(eval(parse(text=paste0("lens_B_", i)))+i*20), opacity = 0.7)
  print(i)
  }

for (i in 10:18){
  fig_G <- fig %>%
    add_surface(z = ~ as.matrix(eval(parse(text=paste0("lens_G_", i)))+i*20), opacity = 0.7)
  print(i)
}


for (i in 19:27){
  fig_R <- fig %>%
    add_surface(z = ~ as.matrix(eval(parse(text=paste0("lens_R_", i)))+i*20), opacity = 0.7)
  print(i)
}





dev.off()
fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~ as.matrix(lens_1))
fig <- fig %>% add_surface(z = ~as.matrix(lens_2+5), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_3+10), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_4+15), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_5+20), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_6+25), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_7+30), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_8+35), opacity = 0.7)
fig <- fig %>% add_surface(z = ~as.matrix(lens_9+40), opacity = 0.7)
gg <- fig



saveWidget(ggplotly(gg), file = "myplot.html")



dev.off()

fig_B <- plot_ly(showscale = FALSE)

for (i in 1:9){

fig_B <- fig_B %>% add_surface(z = ~ as.matrix(eval(parse(text=paste0("lens_B_", i)))+5*(i-1)), opacity = 0.7)
}

fig_B

gg <- fig

library(plotly)
library(tidyverse)
library(htmlwidgets)

saveWidget(ggplotly(gg), file = "myplot.html")

getwd()

################# spectrum 분석


rm(list=ls())

## 여러 csv 파일 불러오기 (z=5.6)
setwd("D:\\Non_Documents\\엑소\\mla\\data_sp2\\")  ## Working directory setting
path <- getwd()
list.files() ## file list
file_list<-list.files(pattern = "csv") ## file list
file_list

for (i in 1:length(file_list)){
  print(i)
  assign(paste0("lens_",430+i*20),
         read_csv(file_list[i], skip = 35, col_names = F))

}



fig_sp2 <- plot_ly(showscale = FALSE)

fig_sp2 <- fig_sp2 %>% add_surface(z = ~ as.matrix(lens_450))
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_470+5), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_490+10), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_510+15), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_530+20), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_550+25), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_570+30), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_590+35), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_610+40), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_630+45), opacity = 0.7)
fig_sp2 <- fig_sp2 %>% add_surface(z = ~as.matrix(lens_650+50), opacity = 0.7)

fig_sp2



require(akima)
require(rgl)

tdn<-c(1:nrow(z_50_650))
rdn<-c(1:ncol(z_50_650))

plot.new()
frame()
options(warn=-1) # 경고메세지 무시하기
getwd()
par("mar")
par(mar = c(1,1,1,1))
par(fg = NA,col="black")
#par(mfrow = c(1, 11))
dev.off()
for (i in 1:length(file_list)){

  png(paste0("lens_", 430+20*i, ".png"), pointsize=10, width=3800, height=3400, res=600)

  filled.contour(x=tdn,
                         y=rdn,
                         z=as.matrix(eval(parse(text=paste0("lens_", 430+20*i)))),
  color.palette=colorRampPalette(c("blue","yellow","red")),
  plot.title=title(main=paste0("Transmittance as wavelength, λ = ",430+i*20, "nm") , sub="Z = 5.4mm",
                   xlab="", ylab=""),
  nlevels=50,
  plot.axes = { axis(side = 1, at = tdn, labels = tdn, col.lab="white")
    axis(side = 2, at = rdn, labels = rdn, col.lab="white") },
  key.title=title(main="T(%)"),
  key.axes = axis(4, seq(0, 8, by = 0.1)))

  dev.off()

  print(i)
}

plot.new()
filled.contour(x=tdn,
               y=rdn,
               z=as.matrix(z_50_650),
               color.palette=colorRampPalette(c("blue","yellow","red")),
               plot.title=title(main=paste0("Transmittance as wavelength, λ = 650nm") , sub="Z = 5.0, 5.3, 5.6mm",
                                xlab="", ylab=""),
               nlevels=50,
               plot.axes = { axis(side = 1, at = tdn, labels = tdn, col.lab="white")
                 axis(side = 2, at = rdn, labels = rdn, col.lab="white") },
               key.title=title(main="T(%)"),
               key.axes = axis(4, seq(0, 8, by = 0.1)))



abline(h=25,v=6, col = "red", lty = 2)
library(tidyverse)

df <- a

for (i in 2:length(file_list)){

  a <- as.vector(eval(parse(text=paste0("lens_", 430+20*i)))[,25] )

  df <- cbind(df,a)


}

colnames(df) <- gsub(".csv", "", file_list)

row.names(df)

df %>% mutate(y=c(1:34)) %>%
  pivot_longer(-y, names_to = "wavelength", values_to = "tr") %>%
  ggplot(aes(x=y, y=tr, col=wavelength))+
  geom_point()+geom_line()

df %>% mutate(y=c(1:34)) %>%
  pivot_longer(-y, names_to = "wavelength", values_to = "tr") %>%
  filter(y == 9) %>%
  mutate(wavelength  = as.numeric(substr(wavelength,4,6))) %>%
  ggplot(aes(x=wavelength, y=tr))+
  geom_point()


############# 이미지 배열



#패키지를 사용하여 실루엣을 둘러싼 흰색 배경을 투명하게 만들고 이미지를 일반적인 크기(너비 기준)로 조정한 다음 's로 저장해야 합니다

needs(magick)
library(png)
path <- "D:\\Non_Documents\\엑소\\mla\\data_sp\\"
file.names <- dir(path, pattern = ".png")
setwd(path)





foo<-list()
for(j in 1:length(file.names)) foo[[j]]<-readPNG(file.names[j])

png("fit.png",width=720,height=480, res = 480)
grid.arrange(foo[[1]], foo[[2]], nrow=1, widths=c(1,2))
dev.off()
foo[[2]]






img <- magick::image_read("D:/Non_Documents/엑소/mla/data_sp/lens_450.png")
plot(img) # or print(img)



####


################# z축에 따른 MLA 분석


rm(list=ls())

## 여러 csv 파일 불러오기 (D 25um 반원구)
setwd("D:\\Non_Documents\\엑소\\mla\\data_depth\\")  ## Working directory setting
path <- getwd()
list.files() ## file list
file_list<-list.files(pattern = "csv") ## file list
file_list

for (i in 1:length(file_list)){
  print(i)
  assign(paste0(substr(file_list[i], 1, 8)),
         read_csv(file_list[i], skip = 35, col_names = F))

}



fig_650 <- plot_ly(showscale = FALSE)

fig_650 <- fig_650 %>% add_surface(z = ~ as.matrix(z_50_650))
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_51_650+10), opacity = 0.7)
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_52_650+20), opacity = 0.7)
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_53_650+30), opacity = 0.7)
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_54_650+40), opacity = 0.7)
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_55_650+50), opacity = 0.7)
fig_650 <- fig_650 %>% add_surface(z = ~as.matrix(z_56_650+60), opacity = 0.7)

fig_650



fig_550 <- plot_ly(showscale = FALSE)

fig_550 <- fig_550 %>% add_surface(z = ~ as.matrix(z_50_550))
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_51_550+10), opacity = 0.7)
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_52_550+20), opacity = 0.7)
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_53_550+30), opacity = 0.7)
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_54_550+40), opacity = 0.7)
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_55_550+50), opacity = 0.7)
fig_550 <- fig_550 %>% add_surface(z = ~as.matrix(z_56_550+60), opacity = 0.7)

fig_550



fig_450 <- plot_ly(showscale = FALSE)

fig_450 <- fig_450 %>% add_surface(z = ~ as.matrix(z_50_450))
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_51_450+10), opacity = 0.7)
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_52_450+20), opacity = 0.7)
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_53_450+30), opacity = 0.7)
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_54_450+40), opacity = 0.7)
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_55_450+50), opacity = 0.7)
fig_450 <- fig_450 %>% add_surface(z = ~as.matrix(z_56_450+60), opacity = 0.7)

fig_450



require(akima)
require(rgl)

rdn<-c(1:nrow(z_50_450))
tdn<-c(1:ncol(z_50_450))
rdn
tdn
plot.new()
frame()
options(warn=-1) # 경고메세지 무시하기
getwd()
par("mar")
par(mar = c(1,1,1,1))
par(fg = NA,col="black")
#par(mfrow = c(1, 11))
dev.off()
5%/%3

 
for (i in c(1:3)){

  png(paste0(substr(file_list[i], 1, 8), ".png"), pointsize=10, width=3800, height=3400, res=600)

  p[i] <- filled.contour(x=rdn,
                   y=tdn,
                   z=as.matrix(eval(parse(text=paste0("z_", 47+3*i, "_650")))),
  color.palette=colorRampPalette(c("blue","yellow","red")),
  plot.title=title(main=paste0("Transmittance as z, z=", substr(file_list[i], 3, 4), "mm") ,
                   sub= paste0("λ =",substr(file_list[i], 6, 8) ) ,
                   xlab="", ylab=""),
  nlevels=50,
  plot.axes = { axis(side = 1, at = tdn, labels = tdn, col.lab="white")
    axis(side = 2, at = rdn, labels = rdn, col.lab="white") },
  key.title=title(main="T(%)"),
  key.axes = axis(4, seq(0, 8, by = 0.1)))

  dev.off()

  print(i)
}


i
dev.off()
plot.new()
png(paste0(substr(file_list[i], 1, 8), ".png"), pointsize=10, width=3800, height=3400, res=600)

filled.contour(x=rdn,
               y=tdn,
               z=as.matrix(z_56_650),
               color.palette=colorRampPalette(c("blue","yellow","red")),
               plot.title=title(main=paste0("Transmittance as z, z=", substr(file_list[i], 3, 4), "mm") ,
                                sub= paste0("λ =",substr(file_list[i], 6, 8) ) ,
                                xlab="", ylab=""),
               nlevels=50,
               plot.axes = { axis(side = 1, at = tdn, labels = tdn, col.lab="white")
                 axis(side = 2, at = rdn, labels = rdn, col.lab="white") },
               key.title=title(main="T(%)"),
               key.axes = axis(4, seq(0, 8, by = 0.1)))
dev.off()



abline(h=25,v=6, col = "red", lty = 2)
library(tidyverse)

df <- a

for (i in 2:length(file_list)){

  a <- as.vector(eval(parse(text=paste0("lens_", 430+20*i)))[,25] )

  df <- cbind(df,a)


}

colnames(df) <- gsub(".csv", "", file_list)

row.names(df)

df %>% mutate(y=c(1:34)) %>%
  pivot_longer(-y, names_to = "wavelength", values_to = "tr") %>%
  ggplot(aes(x=y, y=tr, col=wavelength))+
  geom_point()+geom_line()

df %>% mutate(y=c(1:34)) %>%
  pivot_longer(-y, names_to = "wavelength", values_to = "tr") %>%
  filter(y == 9) %>%
  mutate(wavelength  = as.numeric(substr(wavelength,4,6))) %>%
  ggplot(aes(x=wavelength, y=tr))+
  geom_point()
