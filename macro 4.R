setwd("C:/Users/Nathan/Downloads")
library(readxl)
comp<-read_xlsx("empresas latin america.xlsx")
names<-comp[,1:3]

comp<-comp[-(1:9),-(1:10)]
head(comp)
i<-2
j<-1
while (i<nrow(comp)) {
  comp[1,i]<-names[j,2]
  if{ comp[1,i+1] == "t" }
  comp[1,i+1]<-names[j,2]
  comp[1,i+2]<-names[j,2]
  comp[1,i+3]<-names[j,2]
  i<-i+4
  j<-j+1
}
head(comp)
comp[1,nrow(comp)]
comp[1:10,1:10]
