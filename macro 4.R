setwd("C:/Users/Nathan/Downloads/macro 4")
library(readxl)
library(writexl)
library(dfms)
library(xtable)
comp<-read_xlsx("empresas latin america.xlsx")
names<-comp[,1:3]

comp<-comp[-(1:9),-(1:10)]



#comp2<-comp
comp<-comp2


i<-2
j<-1

comp<-comp[1:350,]
# Criando a série de tempo mensal
data_inicio <- as.Date("2023-07-31")
 data_fim <- as.Date("1994-07-31")
serie_tempo <- seq(data_inicio, data_fim, by = "-1 month")

comp[-1,1]<-as.character(serie_tempo)

complet_row<-vector("numeric", length = nrow(comp))
complet_col<-vector("numeric", length = ncol(comp)) 
pontos<-vector("numeric", length = nrow(names)) 
while (i<ncol(comp)) {
  
  if(is.na(comp[1,i] ) == T ){
    comp[2,i] <- names[j,2]
    complet_col[i]<-nrow(comp) - sum(is.na(comp[-(1:2),i]))
  pontos[j]<-i
        j<-j+1
    print(j)
    
      }
else if( comp[1,i] == "trade close" |  comp[1,i] == "close" ) {
    
    comp[2,i] <- names[j,2]
    pontos[j]<-i
    j<-j+1
    complet_col[i]<-sum(is.na(comp[-(1:2),i]))

  }
 
  i<-i+1

}
###ok
comp<-comp[,c(1,pontos[-1])]


comp<-comp[1:189,]  
complet_row<-vector("numeric", length = nrow(comp))
complet_col<-vector("numeric", length = ncol(comp)) 

for(i in 3:nrow(comp)) {
  complet_row[i]<- ncol(comp)- sum(is.na(comp[i, -1])) 
} 
for(i in 2:ncol(comp)) {
  complet_col[i]<- nrow(comp)- sum(is.na(comp[-(1:2),i])) 
} 


#plot(complet_row)

#plot(complet_col)

complet_comp100<-matrix(0, nrow = nrow(comp)  ,ncol = ncol(comp) -sum(complet_col < 100) )
complet_comp100<-as.data.frame(complet_comp100)
j<-1
for(i in 2:ncol(comp)) {
 if(complet_col[i]>100){
   complet_comp100[,j]<-comp[,i]
 j<-j+1
   }
} 
write_xlsx(complet_comp100, path = "complet_comp100.xlsx")

complet_comp70<-matrix(0, nrow = nrow(comp)  ,ncol = ncol(comp) -sum(complet_col < 70) )
complet_comp70<-as.data.frame(complet_comp70)
j<-1
for(i in 1:ncol(comp)) {
  if(complet_col[i]>70){
    complet_comp70[,j]<-comp[,i]
    j<-j+1
  }
} 

write_xlsx(complet_comp70, path = "complet_comp70.xlsx")
#complet_comp70<-read_excel("complet_comp70.xlsx")
library(zoo)
complet_comp70_interp<-as.data.frame(na.approx(complet_comp70[-(1:2),]))
complet_comp70_interp<-rbind(complet_comp70[(1:2),], complet_comp70_interp)
write_xlsx(complet_comp70_interp, path = "complet_comp70_int.xlsx")

###### raspagem final
i<-1
while (i<ncol(complet_comp70_interp)) {
  if(sum(is.na(complet_comp70_interp[-(1:2),i]) ) >7 ){
    complet_comp70_interp<-complet_comp70_interp[,-i]
    i<-i-1
  }
  i<-i+1
}

i<-1
while (i<ncol(complet_comp70_interp)) {
  if(sum(is.na(complet_comp70_interp[-(1:100),i]) ) >2 ){
    complet_comp70_interp<-complet_comp70_interp[,-i]
    i<-i-1
  }
  i<-i+1
}



complet_comp70_interp<-cbind(comp[,1],complet_comp70_interp)
indices_na <- which(is.na( complet_comp70_interp[-(1:9),]), arr.ind = T)# tudo nas duas ultimas
comp_final<-complet_comp70_interp[-(1:9),]
comp_final<-comp_final[-(179:180),-(550:555)]

sum(is.na( comp_final))#0
for (i in 2:ncol(complet_comp70_interp)) {
  comp_final[,i]<-(diff(as.numeric(comp_final[,i] )) /lag(as.numeric(comp_final[,i])) ) # yt - yt_1 / yt
  
}
comp_final[,2]

#View(complet_comp70_interp[,(550:555)])
sum(is.na( comp_final[-1,]))
indices_na <- which(is.na(comp_final ), arr.ind = T)# tudo nas duas ultimas



##comp_final<- comp_final[-1,]
comp_final<- rbind(complet_comp70_interp[1:2,1:549],comp_final )
scale(comp_final[,-1])
ic = ICr(scale(comp_final[,-1]))
plot(ic)
screeplot(ic, type = "pve")# primeiro 14.5, segundo 14
mod = DFM(scale(comp_final[,-1]), r = 2, p = 1)
#terei de fazer variancia explicada pelo auto valor? N, tem no screeplot

comp_final[3,4]
summary(mod)
mod
plot(mod)

# Gerar apenas 5 valores para exibir no eixo x

f_asset<-as.data.frame(mod[["F_pca"]])
f_asset<-cbind(as.Date(comp_final[,1]), f_asset)
colnames(f_asset)<-c("date", "PC1", "PC2")
f_asset[,1]
ggplot(data = f_asset, aes(x= date,   y = PC1, group = 1)) +
     geom_line(aes(col = 'firts asset factor')) +
     geom_line(aes(y = PC2 ,col = 'second factor')) +
   labs(title = "fatores de assets",
                   x = "meses",
                   y = "Fatores",
                   color = "") 
 
 ########## botar agora os negocios de vix


################### parte jp
fdi_assets_bp_mod <- read_excel("fdi_assets_bp.xlsx",
                                sheet = excel_sheets("fdi_assets_bp.xlsx")[2])
#View(fdi_assets_bp_mod)

fdi_assets_bp <- data.frame(t(fdi_assets_bp_mod))

fdi_assets_bp <- fdi_assets_bp[-1 , -c(1:4, 162:166)]

colnames(fdi_assets_bp) <- as.character(unlist(fdi_assets_bp_mod[c(5:161), 1]))
rownames(fdi_assets_bp) <- as.character(unlist(fdi_assets_bp_mod[4, c(2:ncol(fdi_assets_bp_mod))]))

# FDI (IIP)

fdi_assets_iip_mod <- read_excel("fdi_assets_iip.xlsx",
                                 sheet = excel_sheets("fdi_assets_iip.xlsx")[2])
#View(fdi_assets_iip_mod)

fdi_assets_iip <- data.frame(t(fdi_assets_iip_mod))

#View(fdi_assets_iip)

fdi_assets_iip <- fdi_assets_iip[-1 , -c(1:4, 213:218)]

colnames(fdi_assets_iip) <- as.character(unlist(fdi_assets_iip_mod[c(5:161), 1]))
rownames(fdi_assets_iip) <- as.character(unlist(fdi_assets_iip_mod[4, c(2:ncol(fdi_assets_iip_mod))]))

# Portfolio equity (BP)

equity_assets_bp_mod <- read_excel("portfolio_equity_assets_bp.xlsx",
                                   sheet = excel_sheets("portfolio_equity_assets_bp.xlsx")[2])
##View(equity_assets_bp_mod)

equity_assets_bp <- data.frame(t(equity_assets_bp_mod))

##View(equity_assets_bp)

equity_assets_bp <- equity_assets_bp[-1 , -c(1:4, 161:165)]

colnames(equity_assets_bp) <- as.character(unlist(equity_assets_bp_mod[c(5:160), 1]))
rownames(equity_assets_bp) <- as.character(unlist(equity_assets_bp_mod[4, c(2:ncol(equity_assets_bp_mod))]))

# Portfolio equity (IIP)


equity_assets_iip_mod <- read_excel("portfolio_equity_assets_iip.xlsx",
                                    sheet = excel_sheets("portfolio_equity_assets_iip.xlsx")[2])
##View(equity_assets_iip_mod)

equity_assets_iip <- data.frame(t(equity_assets_iip_mod))

##View(equity_assets_iip)

equity_assets_iip <- equity_assets_iip[-1 , -c(1:4, 213:218)]

colnames(equity_assets_iip) <- as.character(unlist(equity_assets_iip_mod[c(5:212), 1]))
rownames(equity_assets_iip) <- as.character(unlist(equity_assets_iip_mod[4, c(2:ncol(equity_assets_iip_mod))]))

# Portfolio debt (BP)

debt_assets_bp_mod <- read_excel("portfolio_debt_assets_bp.xlsx",
                                 sheet = excel_sheets("portfolio_debt_assets_bp.xlsx")[2])
#View(debt_assets_bp_mod)

debt_assets_bp <- data.frame(t(debt_assets_bp_mod))

#View(debt_assets_bp)

debt_assets_bp <- debt_assets_bp[-1 , -c(1:4, 159:163)]

colnames(debt_assets_bp) <- as.character(unlist(debt_assets_bp_mod[c(5:158), 1]))
rownames(debt_assets_bp) <- as.character(unlist(debt_assets_bp_mod[4, c(2:ncol(debt_assets_bp_mod))]))

# Portfolio debt (IIP)

debt_assets_iip_mod <- read_excel("portfolio_debt_assets_iip.xlsx",
                                  sheet = excel_sheets("portfolio_debt_assets_iip.xlsx")[2])
#View(debt_assets_iip_mod)

debt_assets_iip <- data.frame(t(debt_assets_iip_mod))

#View(debt_assets_iip)

debt_assets_iip <- debt_assets_iip[-1 , -c(1:4, 213:218)]

colnames(debt_assets_iip) <- as.character(unlist(debt_assets_iip_mod[c(5:212), 1]))
rownames(debt_assets_iip) <- as.character(unlist(debt_assets_iip_mod[4, c(2:ncol(debt_assets_iip_mod))]))

# Portfolio other (BP)

other_assets_bp_mod <- read_excel("other_assets_bp.xlsx",
                                  sheet = excel_sheets("other_assets_bp.xlsx")[2])
#View(other_assets_bp_mod)

other_assets_bp <- data.frame(t(other_assets_bp_mod))

#View(other_assets_bp)

other_assets_bp <- other_assets_bp[-1 , -c(1:4, 163:167)]

colnames(other_assets_bp) <- as.character(unlist(other_assets_bp_mod[c(5:162), 1]))
rownames(other_assets_bp) <- as.character(unlist(other_assets_bp_mod[4, c(2:ncol(other_assets_bp_mod))]))

# Portfolio other (IIP)

other_assets_iip_mod <- read_excel("other_assets_iip.xlsx",
                                   sheet = excel_sheets("other_assets_iip.xlsx")[2])
#View(other_assets_iip_mod)

other_assets_iip <- data.frame(t(other_assets_iip_mod))

#View(other_assets_iip)

other_assets_iip <- other_assets_iip[-1 , -c(1:4, 213:218)]

colnames(other_assets_iip) <- as.character(unlist(other_assets_iip_mod[c(5:212), 1]))
rownames(other_assets_iip) <- as.character(unlist(other_assets_iip_mod[4, c(2:ncol(other_assets_iip_mod))]))


## LIABILITIES

# FDI (BP)

fdi_liabilities_bp_mod <- read_excel("fdi_liabilities_bp.xlsx",
                                     sheet = excel_sheets("fdi_liabilities_bp.xlsx")[2])
#View(fdi_liabilities_bp_mod)

fdi_liabilities_bp <- data.frame(t(fdi_liabilities_bp_mod))

#View(fdi_liabilities_bp)

fdi_liabilities_bp <- fdi_liabilities_bp[-1 , -c(1:4, 163:167)]

colnames(fdi_liabilities_bp) <- as.character(unlist(fdi_liabilities_bp_mod[c(5:162), 1]))
rownames(fdi_liabilities_bp) <- as.character(unlist(fdi_liabilities_bp_mod[4, c(2:ncol(fdi_liabilities_bp_mod))]))

# FDI (IIP)

fdi_liabilities_iip_mod <- read_excel("fdi_liabilities_iip.xlsx",
                                      sheet = excel_sheets("fdi_liabilities_iip.xlsx")[2])
#View(fdi_liabilities_iip_mod)

fdi_liabilities_iip <- data.frame(t(fdi_liabilities_iip_mod))

#View(fdi_liabilities_iip)

fdi_liabilities_iip <- fdi_liabilities_iip[-1 , -c(1:4, 213:218)]

colnames(fdi_liabilities_iip) <- as.character(unlist(fdi_liabilities_iip_mod[c(5:212), 1]))
rownames(fdi_liabilities_iip) <- as.character(unlist(fdi_liabilities_iip_mod[4, c(2:ncol(fdi_liabilities_iip_mod))]))

# Portfolio Equity (BP)

equity_liabilities_bp_mod <- read_excel("portfolio_equity_liabilities_bp.xlsx",
                                        sheet = excel_sheets("portfolio_equity_liabilities_bp.xlsx")[2])

#View(equity_liabilities_bp_mod)

equity_liabilities_bp <- data.frame(t(equity_liabilities_bp_mod))

#View(equity_liabilities_bp)

equity_liabilities_bp <- equity_liabilities_bp[-1 , -c(1:4, 161:165)]

colnames(equity_liabilities_bp) <- as.character(unlist(equity_liabilities_bp_mod[c(5:160), 1]))
rownames(equity_liabilities_bp) <- as.character(unlist(equity_liabilities_bp_mod[4, c(2:ncol(equity_liabilities_bp_mod))]))

# Portfolio Equity (IIP)

equity_liabilities_iip_mod <- read_excel("portfolio_equity_liabilities_iip.xlsx",
                                         sheet = excel_sheets("portfolio_equity_liabilities_iip.xlsx")[2])

#View(equity_liabilities_iip_mod)

equity_liabilities_iip <- data.frame(t(equity_liabilities_iip_mod))

#View(equity_liabilities_iip)

equity_liabilities_iip <- equity_liabilities_iip[-1 , -c(1:4, 213:218)]

colnames(equity_liabilities_iip) <- as.character(unlist(equity_liabilities_iip_mod[c(5:212), 1]))
rownames(equity_liabilities_iip) <- as.character(unlist(equity_liabilities_iip_mod[4, c(2:ncol(equity_liabilities_iip_mod))]))

# Portfolio Debt (BP)

debt_liabilities_bp_mod <- read_excel("portfolio_debt_liabilities_bp.xlsx",
                                      sheet = excel_sheets("portfolio_debt_liabilities_bp.xlsx")[2])

##View(debt_liabilities_bp_mod)

debt_liabilities_bp <- data.frame(t(debt_liabilities_bp_mod))

#View(debt_liabilities_bp)

debt_liabilities_bp <- debt_liabilities_bp[-1 , -c(1:4, 158:162)]

colnames(debt_liabilities_bp) <- as.character(unlist(debt_liabilities_bp_mod[c(5:157), 1]))
rownames(debt_liabilities_bp) <- as.character(unlist(debt_liabilities_bp_mod[4, c(2:ncol(debt_liabilities_bp_mod))]))

# Portfolio Debt (IIP)

debt_liabilities_iip_mod <- read_excel("portfolio_debt_liabilities_iip.xlsx",
                                       sheet = excel_sheets("portfolio_debt_liabilities_iip.xlsx")[2])

#View(debt_liabilities_iip_mod)

debt_liabilities_iip <- data.frame(t(debt_liabilities_iip_mod))

#View(debt_liabilities_iip)

debt_liabilities_iip <- debt_liabilities_iip[-1 , -c(1:4, 213:218)]

colnames(debt_liabilities_iip) <- as.character(unlist(debt_liabilities_iip_mod[c(5:212), 1]))
rownames(debt_liabilities_iip) <- as.character(unlist(debt_liabilities_iip_mod[4, c(2:ncol(debt_liabilities_iip_mod))]))

# Portfolio other (BP)

other_liabilities_bp_mod <- read_excel("other_liabilities_bp.xlsx",
                                       sheet = excel_sheets("other_liabilities_bp.xlsx")[2])
#View(other_liabilities_bp_mod)

other_liabilities_bp <- data.frame(t(other_liabilities_bp_mod))

#View(other_liabilities_bp)

other_liabilities_bp <- other_liabilities_bp[-1 , -c(1:4, 163:167)]

colnames(other_liabilities_bp) <- as.character(unlist(other_liabilities_bp_mod[c(5:162), 1]))
rownames(other_liabilities_bp) <- as.character(unlist(other_liabilities_bp_mod[4, c(2:ncol(other_liabilities_bp_mod))]))

# Portfolio other (IIP)

other_liabilities_iip_mod <- read_excel("other_liabilities_iip.xlsx",
                                        sheet = excel_sheets("other_liabilities_iip.xlsx")[2])
#View(other_liabilities_iip_mod)

other_liabilities_iip <- data.frame(t(other_liabilities_iip_mod))

#View(other_liabilities_iip)

other_liabilities_iip <- other_liabilities_iip[-1 , -c(1:4, 213:218)]

colnames(other_liabilities_iip) <- as.character(unlist(other_liabilities_iip_mod[c(5:212), 1]))
rownames(other_liabilities_iip) <- as.character(unlist(other_liabilities_iip_mod[4, c(2:ncol(other_liabilities_iip_mod))]))


################################################################################

# Reserves assets (BP)

reserves_bp_mod <- read_excel("reserve_assets_bp.xlsx",
                              sheet = excel_sheets("reserve_assets_bp.xlsx")[2])
#View(reserves_bp_mod)

reserves_bp <- data.frame(t(reserves_bp_mod))

#View(reserves_bp)

reserves_bp <- reserves_bp[-1 , -c(1:4, 163:167)]

colnames(reserves_bp) <- as.character(unlist(reserves_bp_mod[c(5:162), 1]))
rownames(reserves_bp) <- as.character(unlist(reserves_bp_mod[4, c(2:ncol(reserves_bp_mod))]))

# Reserves assets (IIP)

reserves_iip_mod <- read_excel("reserve_assets_iip.xlsx",
                               sheet = excel_sheets("reserve_assets_iip.xlsx")[2])
#View(reserves_iip_mod)

reserves_iip <- data.frame(t(reserves_iip_mod))

#View(reserves_iip)

reserves_iip <- reserves_iip[-1 , -c(1:4, 213:218)]

colnames(reserves_iip) <- as.character(unlist(reserves_iip_mod[c(5:212), 1]))
rownames(reserves_iip) <- as.character(unlist(reserves_iip_mod[4, c(2:ncol(reserves_iip_mod))]))

# Claims private sector

claims_ps_mod <- read_excel("claims_private_sector.xlsx",
                            sheet = excel_sheets("claims_private_sector.xlsx")[2])
#View(claims_ps_mod)

claims_ps <- data.frame(t(claims_ps_mod))

#View(claims_ps)

claims_ps <- claims_ps[-c(1:3) , -c(1:5)]

colnames(claims_ps) <- as.character(unlist(claims_ps_mod[c(6:153), 1]))
rownames(claims_ps) <- as.character(unlist(claims_ps_mod[5, c(4:ncol(claims_ps_mod))]))


################################################################################

# Inicialmente, vamos selecionar apenas os países da América Latina em cada um
# dos dataframes

# Vetor com nomes de países da LatAm:
paises_latam <- c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica",
                  "El Salvador", "Mexico", "Paraguay", "Peru")

# Modificando cada um dos dataframes
library(dplyr)

fdi_assets_bp_latam <- select(fdi_assets_bp, all_of(paises_latam))
fdi_assets_iip_latam <- select(fdi_assets_iip, all_of(paises_latam))
equity_assets_bp_latam <- select(equity_assets_bp, all_of(paises_latam))
equity_assets_iip_latam <- select(equity_assets_iip, all_of(paises_latam))
debt_assets_bp_latam <- select(debt_assets_bp, all_of(paises_latam))
debt_assets_iip_latam <- select(debt_assets_iip, all_of(paises_latam))
other_assets_bp_latam <- select(other_assets_bp, all_of(paises_latam))
other_assets_iip_latam <- select(other_assets_iip, all_of(paises_latam))

fdi_liabilities_bp_latam <- select(fdi_liabilities_bp, all_of(paises_latam))
fdi_liabilities_iip_latam <- select(fdi_liabilities_iip, all_of(paises_latam))
equity_liabilities_bp_latam <- select(equity_liabilities_bp, all_of(paises_latam))
equity_liabilities_iip_latam <- select(equity_liabilities_iip, all_of(paises_latam))
debt_liabilities_bp_latam <- select(debt_liabilities_bp, all_of(paises_latam))
debt_liabilities_iip_latam <- select(debt_liabilities_iip, all_of(paises_latam))
other_liabilities_bp_latam <- select(other_liabilities_bp, all_of(paises_latam))
other_liabilities_iip_latam <- select(other_liabilities_iip, all_of(paises_latam))
reserves_bp_latam <- select(reserves_bp, all_of(paises_latam))
reserves_iip_latam <- select(reserves_iip, all_of(paises_latam))

# Substituindo entradas vazias (...) por NA:

fdi_assets_bp_latam[fdi_assets_bp_latam == "..."] <- NA
fdi_assets_iip_latam[fdi_assets_iip_latam == "..."] <- NA
equity_assets_bp_latam[equity_assets_bp_latam == "..."] <- NA
equity_assets_iip_latam[equity_assets_iip_latam == "..."] <- NA
debt_assets_bp_latam[debt_assets_bp_latam == "..."] <- NA
debt_assets_iip_latam[debt_assets_iip_latam == "..."] <- NA
other_assets_bp_latam[other_assets_bp_latam == "..."] <- NA
other_assets_iip_latam[other_assets_iip_latam == "..."] <- NA

fdi_liabilities_bp_latam[fdi_liabilities_bp_latam == "..."] <- NA
fdi_liabilities_iip_latam[fdi_liabilities_iip_latam == "..."] <- NA
equity_liabilities_bp_latam[equity_liabilities_bp_latam == "..."] <- NA
equity_liabilities_iip_latam[equity_liabilities_iip_latam == "..."] <- NA
debt_liabilities_bp_latam[debt_liabilities_bp_latam == "..."] <- NA
debt_liabilities_iip_latam[debt_liabilities_iip_latam == "..."] <- NA
other_liabilities_bp_latam[other_liabilities_bp_latam == "..."] <- NA
other_liabilities_iip_latam[other_liabilities_iip_latam == "..."] <- NA
reserves_bp_latam[reserves_bp_latam == "..."] <- NA
reserves_iip_latam[reserves_iip_latam == "..."] <- NA

# Uma ideia é tomar a amostra a partir de 2000

fdi_assets_bp_latam <- fdi_assets_bp_latam[-c(1:40), ]
fdi_assets_iip_latam <- fdi_assets_iip_latam[-c(1:40), ]
equity_assets_bp_latam <- equity_assets_bp_latam[-c(1:40), ]
equity_assets_iip_latam <- equity_assets_iip_latam[-c(1:40), ]
debt_assets_bp_latam <- debt_assets_bp_latam[-c(1:40), ]
debt_assets_iip_latam <- debt_assets_iip_latam[-c(1:40), ]
other_assets_bp_latam <- other_assets_bp_latam[-c(1:40), ]
other_assets_iip_latam <- other_assets_iip_latam[-c(1:40), ]

fdi_liabilities_bp_latam <- fdi_liabilities_bp_latam[-c(1:40), ]
fdi_liabilities_iip_latam <- fdi_liabilities_iip_latam[-c(1:40), ]
equity_liabilities_bp_latam <- equity_liabilities_bp_latam[-c(1:40), ]
equity_liabilities_iip_latam <- equity_liabilities_iip_latam[-c(1:40), ]
debt_liabilities_bp_latam <- debt_liabilities_bp_latam[-c(1:40), ]
debt_liabilities_iip_latam <- debt_liabilities_iip_latam[-c(1:40), ]
other_liabilities_bp_latam <- other_liabilities_bp_latam[-c(1:40), ]
other_liabilities_iip_latam <- other_liabilities_iip_latam[-c(1:40), ]
reserves_bp_latam <- reserves_bp_latam[-c(1:40), ]
reserves_iip_latam <- reserves_iip_latam[-c(1:40), ]


fdi_assets_bp_latam$Trimestre <- rownames(fdi_assets_bp_latam)
rownames(fdi_assets_bp_latam) <- NULL

fdi_assets_iip_latam$Trimestre <- rownames(fdi_assets_iip_latam)
rownames(fdi_assets_iip_latam) <- NULL

equity_assets_bp_latam$Trimestre <- rownames(equity_assets_bp_latam)
rownames(equity_assets_bp_latam) <- NULL

equity_assets_iip_latam$Trimestre <- rownames(equity_assets_iip_latam)
rownames(equity_assets_iip_latam) <- NULL

debt_assets_bp_latam$Trimestre <- rownames(debt_assets_bp_latam)
rownames(debt_assets_bp_latam) <- NULL

debt_assets_iip_latam$Trimestre <- rownames(debt_assets_iip_latam)
rownames(debt_assets_iip_latam) <- NULL

other_assets_bp_latam$Trimestre <- rownames(other_assets_bp_latam)
rownames(other_assets_bp_latam) <- NULL

other_assets_iip_latam$Trimestre <- rownames(other_assets_iip_latam)
rownames(other_assets_iip_latam) <- NULL

fdi_liabilities_bp_latam$Trimestre <- rownames(fdi_liabilities_bp_latam)
rownames(fdi_liabilities_bp_latam) <- NULL

fdi_liabilities_iip_latam$Trimestre <- rownames(fdi_liabilities_iip_latam)
rownames(fdi_liabilities_iip_latam) <- NULL

equity_liabilities_bp_latam$Trimestre <- rownames(equity_liabilities_bp_latam)
rownames(equity_liabilities_bp_latam) <- NULL

equity_liabilities_iip_latam$Trimestre <- rownames(equity_liabilities_iip_latam)
rownames(equity_liabilities_iip_latam) <- NULL

debt_liabilities_bp_latam$Trimestre <- rownames(debt_liabilities_bp_latam)
rownames(debt_liabilities_bp_latam) <- NULL

debt_liabilities_iip_latam$Trimestre <- rownames(debt_liabilities_iip_latam)
rownames(debt_liabilities_iip_latam) <- NULL

other_liabilities_bp_latam$Trimestre <- rownames(other_liabilities_bp_latam)
rownames(other_liabilities_bp_latam) <- NULL

other_liabilities_iip_latam$Trimestre <- rownames(other_liabilities_iip_latam)
rownames(other_liabilities_iip_latam) <- NULL

reserves_bp_latam$Trimestre <- rownames(reserves_bp_latam)
rownames(reserves_bp_latam) <- NULL

reserves_iip_latam$Trimestre <- rownames(reserves_iip_latam)
rownames(reserves_iip_latam) <- NULL

# Por enquanto, vamos trabalhar apenas com os dataframes BP, que não apresentam
# tantas entradas NA

# Vamos reunir os dataframes BP de inflows (liabilities) em um só dataframe

# Função para renomear as colunas, exceto "Trimestre"
rename_columns_except_trimestre <- function(df, suffix) {
  cols_to_rename <- names(df)[-match("Trimestre", names(df))]
  new_names <- paste(cols_to_rename, suffix, sep = "_")
  names(df)[names(df) %in% cols_to_rename] <- new_names
  return(df)
}

# Renomear as colunas para cada dataframe
fdi_liabilities_bp_latam <- rename_columns_except_trimestre(fdi_liabilities_bp_latam,
                                                            "fdi_inflows")
equity_liabilities_bp_latam <- rename_columns_except_trimestre(equity_liabilities_bp_latam,
                                                               "equity_inflows")
debt_liabilities_bp_latam <- rename_columns_except_trimestre(debt_liabilities_bp_latam,
                                                             "debt_inflows")
other_liabilities_bp_latam <- rename_columns_except_trimestre(other_liabilities_bp_latam,
                                                              "other_inflows")

# Reunir os dataframes usando merge com base na coluna "Trimestre"
inflows_bp <- merge(fdi_liabilities_bp_latam, equity_liabilities_bp_latam,
                    by = "Trimestre", all = TRUE)
inflows_bp <- merge(inflows_bp, debt_liabilities_bp_latam, by = "Trimestre", all = TRUE)
inflows_bp <- merge(inflows_bp, other_liabilities_bp_latam, by = "Trimestre", all = TRUE)

# Função para corrigir as entradas removendo os caracteres não-numéricos
clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

# Aplicar a função para todas as colunas numéricas
inflows_bp[, -1] <- sapply(inflows_bp[, -1], clean_numeric)


# Agora, vamos reunir os dataframes BP de outflows (assets) em um só dataframe

# Renomear as colunas para cada dataframe
fdi_assets_bp_latam <- rename_columns_except_trimestre(fdi_assets_bp_latam,
                                                       "fdi_outflows")
equity_assets_bp_latam <- rename_columns_except_trimestre(equity_assets_bp_latam,
                                                          "equity_outflows")
debt_assets_bp_latam <- rename_columns_except_trimestre(debt_assets_bp_latam,
                                                        "debt_outflows")
other_assets_bp_latam <- rename_columns_except_trimestre(other_assets_bp_latam,
                                                         "other_outflows")

# Reunir os dataframes usando merge com base na coluna "Trimestre"
outflows_bp <- merge(fdi_assets_bp_latam, equity_assets_bp_latam,
                     by = "Trimestre", all = TRUE)
outflows_bp <- merge(outflows_bp, debt_assets_bp_latam, by = "Trimestre", all = TRUE)
outflows_bp <- merge(outflows_bp, other_assets_bp_latam, by = "Trimestre", all = TRUE)

# Aplicar a função para todas as colunas numéricas
outflows_bp[, -1] <- sapply(outflows_bp[, -1], clean_numeric)

# Nossos dois dataframes estão ajustados

# Agora, precisamos estacionarizar as séries: iremos tomar as primeiras diferenças

# Calculando a primeira diferença para todas as colunas (exceto a primeira coluna com os meses)
inflows_bp <- inflows_bp %>%
  mutate(across(-1, ~ . - lag(.)))

outflows_bp <- outflows_bp %>%
  mutate(across(-1, ~ . - lag(.)))

# Determinando a estrutura do modelo

library(dfms)
library(xts)

# A função ICr( ) pode ser aplicada para determinar o número de fatores: ela 
# computa 3 information criteria propostos em Bai and NG (2002)

ic_inflows <- ICr(inflows_bp[, -1])

print(ic_inflows)

screeplot(ic_inflows)

# Pelo screeplot, podemos escolher 2 fatores para estimar o modelo, já que 
# aparentemente a maior queda do poder explicativo ocorre entre o segundo e terceiro fatores

# Agora, devemos a lag-order do VAR de fatores na equação de transição
# Isto é feito usando o pacote vars, na função VARselect(), com as
# estimativas PCA dos fatores reportadas por ICr()

# Utilizando vars::VARselect() com 2 componentes principais para estimar a
# ordem do lag do VAR

vars::VARselect(ic_inflows$F_pca[ , 1:2])

# Iremos utilizar 3 lags

# Estimando o modelo com 2 fatores e 3 lags

modelo_inflows <- DFM(inflows_bp[, -1], r = 2, p = 3)

print(modelo_inflows)

plot(modelo_inflows) # Plotando os dois fatores

# Podemos então obter um dataframe com as estimativas dos dois fatores (por PCR) para capital inflows

f1_pcr_inflows <- as.data.frame(modelo_inflows)[1:(nrow(inflows_bp)-1), "Value"] # Primeiro fator

f2_pcr_inflows <- as.data.frame(modelo_inflows)[(nrow(inflows_bp)):(2*nrow(inflows_bp)-2), "Value"] # Segundo fator

fatores_inflows <- cbind(f1_pcr_inflows, f2_pcr_inflows)

fatores_inflows <- rbind(NA, fatores_inflows)

fatores_inflows <- cbind(inflows_bp$Trimestre, fatores_inflows)


# Agora, para outflows

ic_outflows <- ICr(outflows_bp[, -1])

print(ic_outflows)

screeplot(ic_outflows)

# Vamos utilizar 1 fator para capital outflows

vars::VARselect(ic_outflows$F_pca[ , 1])

# Estimando o modelo com 1 fator de 7 lags

modelo_outflows <- DFM(outflows_bp[, -1], r = 1, p = 7)

print(modelo_outflows)

plot(modelo_outflows) # Plotando o fator

# Podemos então obter um dataframe com as estimativas do fator (por PCR) para capital outflows

#View(as.data.frame(modelo_outflows))

f1_pcr_outflows <- as.data.frame(modelo_outflows)[1:(nrow(outflows_bp)-1), "Value"]

fatores_outflows <- cbind(NA, f1_pcr_outflows)

fatores_outflows <- rbind(NA, fatores_outflows)

fatores_outflows <- cbind(outflows_bp$Trimestre, fatores_outflows)

fatores_outflows <- fatores_outflows[ , -2]

#View(fatores_outflows)

# Transformando em dataframes

fatores_inflows <- as.data.frame(fatores_inflows)
fatores_outflows <- as.data.frame(fatores_outflows)
fatores_inflows$f1_pcr_inflows <- as.numeric(fatores_inflows$f1_pcr_inflows)
fatores_inflows$f2_pcr_inflows <- as.numeric(fatores_inflows$f2_pcr_inflows)
fatores_outflows$f1_pcr_outflows <- as.numeric(fatores_outflows$f1_pcr_outflows)

cor(fatores_inflows$f1_pcr_inflows[-1], fatores_outflows$f1_pcr_outflows[-1])

# Correlação entre o primeiro fator de capital inflows e o primeiro fator de 
# capital outflows é de 0.58

# Let's plot the first inflow factor and outflow factor together

fatores <- merge(fatores_inflows, fatores_outflows, by = "V1", all = TRUE)

fatores <- na.omit(fatores)

trimestres <- seq(as.Date("2000-04-01"), by = "3 months", length.out = nrow(fatores))

fatores <- data.frame(V1 = trimestres, f1_pcr_inflows = f1_pcr_inflows, f1_pcr_outflows = f1_pcr_outflows)

# Gerar apenas 5 valores para exibir no eixo x
num_values <- 5
step <- ceiling(length(fatores$V1) / num_values)
trimestres_labels <- fatores$V1[seq(1, length(fatores$V1), step)]

# Plotando os dois fatores
library(ggplot2)

ggplot(data = fatores, aes(x = V1, y = f1_pcr_inflows, group = 1)) +
  geom_line(aes(col = 'Inflows factor')) +
  geom_line(aes(y = f1_pcr_outflows ,col = 'Outflows factor')) +
  labs(title = "Inflow e Outflow Factors",
       x = "Trimestres",
       y = "Fatores",
       color = "") +
  scale_x_date(breaks = trimestres_labels, date_labels = "%b %Y") +
  theme(legend.position = "bottom")



# Agora, vamos verificar a correlação entre os diferentes Capital Flows Factors

# FDI assets

# Selecionando colunas
fdi_outflows_colunas <- c("Trimestre", grep("fdi", names(outflows_bp), value = TRUE))

# Dataframe com FDI outflows
fdi_outflows_bp <- subset(outflows_bp, select = fdi_outflows_colunas)


# FDI liabilities

# Selecionando colunas
fdi_inflows_colunas <- c("Trimestre", grep("fdi", names(inflows_bp), value = TRUE))

# Dataframe com FDI outflows
fdi_inflows_bp <- subset(inflows_bp, select = fdi_inflows_colunas)


# Portfolio assets

# Selecionando colunas
equity_outflows_colunas <- c("Trimestre", grep("equity", names(outflows_bp), value = TRUE))
debt_outflows_colunas <- c("Trimestre", grep("debt", names(outflows_bp), value = TRUE))
portfolio_outflows_colunas <- c("Trimestre", equity_outflows_colunas, debt_outflows_colunas)

# Dataframe com portfolio outflows

portfolio_outflows_bp <- subset(outflows_bp, select = portfolio_outflows_colunas)
portfolio_outflows_bp <- portfolio_outflows_bp[, -c(2, 12)]

# Portfolio liabilities

# Selecionando colunas
equity_inflows_colunas <- c("Trimestre", grep("equity", names(inflows_bp), value = TRUE))
debt_inflows_colunas <- c("Trimestre", grep("debt", names(inflows_bp), value = TRUE))
portfolio_inflows_colunas <- c("Trimestre", equity_inflows_colunas, debt_inflows_colunas)

# Dataframe com portfolio outflows

portfolio_inflows_bp <- subset(inflows_bp, select = portfolio_inflows_colunas)
portfolio_inflows_bp <- portfolio_inflows_bp[, -c(2, 12)]


# Other liabilities

# Selecionando colunas
other_inflows_colunas <- c("Trimestre", grep("other", names(inflows_bp), value = TRUE))

# Dataframe com FDI outflows
other_inflows_bp <- subset(inflows_bp, select = other_inflows_colunas)


# Other assets

# Selecionando colunas
other_outflows_colunas <- c("Trimestre", grep("other", names(outflows_bp), value = TRUE))

# Dataframe com FDI outflows
other_outflows_bp <- subset(outflows_bp, select = other_outflows_colunas)



# Agora, vamos determinar o modelo de fatores para cada tipo de capital flows

# FDI inflows

ic_fdi_inflows <- ICr(fdi_inflows_bp[, -1])

print(ic_fdi_inflows)

screeplot(ic_fdi_inflows)

# Vamos pegar o primeiro fator.

vars::VARselect(ic_fdi_inflows$F_pca[ , 1])

# Iremos utilizar 3 lags

# Estimando o modelo com 1 fator e 3 lags

modelo_fdi_inflows <- DFM(fdi_inflows_bp[, -1], r = 1, p = 3)

print(modelo_fdi_inflows)

plot(modelo_fdi_inflows)

# Dataframe com as estimativas do fator (por PCR) para FDI inflows

f1_fdi_inflows <- as.data.frame(modelo_fdi_inflows)[1:(nrow(fdi_inflows_bp)-1), "Value"]

fatores_fdi_inflows <- cbind(NA, f1_fdi_inflows)

fatores_fdi_inflows <- rbind(NA, fatores_fdi_inflows)

fatores_fdi_inflows <- cbind(inflows_bp$Trimestre, fatores_fdi_inflows)

fatores_fdi_inflows <- fatores_fdi_inflows[ , -2]


# FDI outflows

ic_fdi_outflows <- ICr(fdi_outflows_bp[, -1])

print(ic_fdi_outflows)

screeplot(ic_fdi_outflows)

# Vamos usar 2 fatores.

vars::VARselect(ic_fdi_outflows$F_pca[ , 1:2])

# Iremos utilizar 3 lags

# Estimando o modelo com 2 fatores e 3 lags

modelo_fdi_outflows <- DFM(fdi_outflows_bp[, -1], r = 2, p = 3)

print(modelo_fdi_outflows)

plot(modelo_fdi_outflows)

# Dataframe com as estimativas do primeiro fator (por PCR) para FDI outflows

f1_fdi_outflows <- as.data.frame(modelo_fdi_outflows)[1:(nrow(fdi_outflows_bp)-1), "Value"]

fatores_fdi_outflows <- cbind(NA, f1_fdi_outflows)

fatores_fdi_outflows <- rbind(NA, fatores_fdi_outflows)

fatores_fdi_outflows <- cbind(outflows_bp$Trimestre, fatores_fdi_outflows)

fatores_fdi_outflows <- fatores_fdi_outflows[ , -2]



# Portfolio inflows

ic_portfolio_inflows <- ICr(portfolio_inflows_bp[, -1])

print(ic_portfolio_inflows)

screeplot(ic_portfolio_inflows)

# Vamos pegar dois fatores.

vars::VARselect(ic_portfolio_inflows$F_pca[ , 1:2])

# Iremos utilizar 3 lags

# Estimando o modelo com 2 fatores e 3 lags

modelo_portfolio_inflows <- DFM(portfolio_inflows_bp[, -1], r = 2, p = 3)

print(modelo_portfolio_inflows)

plot(modelo_portfolio_inflows)

# Dataframe com as estimativas do fator (por PCR) para portfolio inflows

f1_portfolio_inflows <- as.data.frame(modelo_portfolio_inflows)[1:(nrow(portfolio_inflows_bp)-1), "Value"]

fatores_portfolio_inflows <- cbind(NA, f1_portfolio_inflows)

fatores_portfolio_inflows <- rbind(NA, fatores_portfolio_inflows)

fatores_portfolio_inflows <- cbind(inflows_bp$Trimestre, fatores_portfolio_inflows)

fatores_portfolio_inflows <- fatores_portfolio_inflows[ , -2]



# Portfolio outflows

ic_portfolio_outflows <- ICr(portfolio_outflows_bp[, -1])

print(ic_portfolio_outflows)

screeplot(ic_portfolio_outflows)

# Vamos pegar um fator.

vars::VARselect(ic_portfolio_outflows$F_pca[ , 1])

# Iremos utilizar 4 lags

# Estimando o modelo com 1 fator e 4 lags

modelo_portfolio_outflows <- DFM(portfolio_outflows_bp[, -1], r = 1, p = 4)

print(modelo_portfolio_outflows)

plot(modelo_portfolio_outflows)

# Dataframe com as estimativas do fator (por PCR) para portfolio outflows

f1_portfolio_outflows <- as.data.frame(modelo_portfolio_outflows)[1:(nrow(portfolio_outflows_bp)-1), "Value"]

fatores_portfolio_outflows <- cbind(NA, f1_portfolio_outflows)

fatores_portfolio_outflows <- rbind(NA, fatores_portfolio_outflows)

fatores_portfolio_outflows <- cbind(outflows_bp$Trimestre, fatores_portfolio_outflows)

fatores_portfolio_outflows <- fatores_portfolio_outflows[ , -2]



# Other inflows

ic_other_inflows <- ICr(other_inflows_bp[, -1])

print(ic_other_inflows)

screeplot(ic_other_inflows)

# Vamos pegar dois fatores.

vars::VARselect(ic_other_inflows$F_pca[ , 1:2])

# Iremos utilizar 3 lags

# Estimando o modelo com 2 fatores e 3 lags

modelo_other_inflows <- DFM(other_inflows_bp[, -1], r = 2, p = 3)

print(modelo_other_inflows)

plot(modelo_other_inflows)

# Dataframe com as estimativas do fator (por PCR) para other inflows

f1_other_inflows <- as.data.frame(modelo_other_inflows)[1:(nrow(other_inflows_bp)-1), "Value"]

fatores_other_inflows <- cbind(NA, f1_other_inflows)

fatores_other_inflows <- rbind(NA, fatores_other_inflows)

fatores_other_inflows <- cbind(inflows_bp$Trimestre, fatores_other_inflows)

fatores_other_inflows <- fatores_other_inflows[ , -2]



# Other outflows

ic_other_outflows <- ICr(other_outflows_bp[, -1])

print(ic_other_outflows)

screeplot(ic_other_outflows)

# Vamos pegar dois fatores.

vars::VARselect(ic_other_outflows$F_pca[ , 1:2])

# Iremos utilizar 4 lags

# Estimando o modelo com 2 fatores e 4 lags

modelo_other_outflows <- DFM(other_outflows_bp[, -1], r = 2, p = 4)

print(modelo_other_outflows)

plot(modelo_other_outflows)

# Dataframe com as estimativas do fator (por PCR) para other inflows

f1_other_outflows <- as.data.frame(modelo_other_outflows)[1:(nrow(other_outflows_bp)-1), "Value"]

fatores_other_outflows <- cbind(NA, f1_other_outflows)

fatores_other_outflows <- rbind(NA, fatores_other_outflows)

fatores_other_outflows <- cbind(outflows_bp$Trimestre, fatores_other_outflows)

fatores_other_outflows <- fatores_other_outflows[ , -2]



# Reunindo todos os fatores em um só dataframe

fatores_total <- merge(fatores_fdi_inflows, fatores_fdi_outflows, by = "V1")
fatores_total <- merge(fatores_total, fatores_portfolio_inflows, by = "V1")
fatores_total <- merge(fatores_total, fatores_portfolio_outflows, by = "V1")
fatores_total <- merge(fatores_total, fatores_other_inflows, by = "V1")
fatores_total <- merge(fatores_total, fatores_other_outflows, by = "V1")

fatores_total <- na.omit(fatores_total)

fatores_total <- data.frame(V1 = trimestres, fatores_total[ , -1])

fatores_total <- merge(fatores, fatores_total, by = "V1")


# Montando a matriz de correlação

fatores_total$f1_fdi_inflows <- as.numeric(fatores_total$f1_fdi_inflows)
fatores_total$f1_fdi_outflows <- as.numeric(fatores_total$f1_fdi_outflows)
fatores_total$f1_portfolio_inflows <- as.numeric(fatores_total$f1_portfolio_inflows)
fatores_total$f1_portfolio_outflows <- as.numeric(fatores_total$f1_portfolio_outflows)
fatores_total$f1_other_inflows <- as.numeric(fatores_total$f1_other_inflows)
fatores_total$f1_other_outflows <- as.numeric(fatores_total$f1_other_outflows)

matriz_correlacao <- cor(fatores_total[ , -1])

library(corrplot)

corrplot(matriz_correlacao, method = 'color')

