library(readxl)
library(agricolae)
library(dplyr)
base_final <- read_excel("Prop Mec Raquis PE.xlsx", 
                         sheet = '1', range = "B1:E58")
base_final <- na.omit(base_final)
base_final <- base_final[-nrow(base_final),]
base_final <- cbind(base_final,Cont_fibra=rep(10,nrow(base_final)))
base_final <- cbind(base_final,Tamiz=rep('ST',nrow(base_final)))
base_final <- cbind(base_final,Formulacion=rep('PEADr',nrow(base_final)))
for (i in 2:48) {
  a<-as.character(i)
  Prop_Mec_Raquis_PE <- read_excel("Prop Mec Raquis PE.xlsx", 
                                   sheet = a, range = "B1:E58")
  Prop_Mec_Raquis_PE <- na.omit(Prop_Mec_Raquis_PE)
  Prop_Mec_Raquis_PE <- Prop_Mec_Raquis_PE[-nrow(Prop_Mec_Raquis_PE),]
  if (i%%3 == 1){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Cont_fibra=rep(10,nrow(Prop_Mec_Raquis_PE)))
  } else if (i%%3 == 2){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Cont_fibra=rep(20,nrow(Prop_Mec_Raquis_PE)))
  } else if(i%%3==0)
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Cont_fibra=rep(30,nrow(Prop_Mec_Raquis_PE)))
  if (i %in% c(1,2,3,13,14,15,25,26,27,37,38,39)){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Tamiz=rep('ST',nrow(Prop_Mec_Raquis_PE)))
  }else if (i %in% c(4,5,6,16,17,18,28,29,30,40,41,42)){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Tamiz=rep('T20',nrow(Prop_Mec_Raquis_PE)))
  }else if (i %in% c(7,8,9,19,20,21,31,32,33,43,44,45)){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Tamiz=rep('T30',nrow(Prop_Mec_Raquis_PE)))
  }else if (i %in% c(10,11,12,22,23,24,34,35,36,46,47,48)){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE,Tamiz=rep('T40',nrow(Prop_Mec_Raquis_PE)))
  }
  if (i<=12) {
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE, Formulacion = rep('PEADr',nrow(Prop_Mec_Raquis_PE)))
  } else if(i>=13 && i<=24){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE, Formulacion = rep('PEADr_4',nrow(Prop_Mec_Raquis_PE)))
  } else if (i>24 && i<=36){
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE, Formulacion = rep('PEr',nrow(Prop_Mec_Raquis_PE)))
  }else {
    Prop_Mec_Raquis_PE <- cbind(Prop_Mec_Raquis_PE, Formulacion = rep('PEr_4',nrow(Prop_Mec_Raquis_PE)))
  }
  base_final <- rbind(base_final,Prop_Mec_Raquis_PE)
}
colnames(base_final)[colnames(base_final) == "Módulo elástico (MPa)"] <- "Mod_elas"
colnames(base_final)[colnames(base_final) == "Resistencia a la tensión (MPa)"] <- "Resistencia"
colnames(base_final)[colnames(base_final) == "Elongación a la rotura (%)"] <- "Elongacion"
colnames(base_final)[colnames(base_final) == "Tenacidad (MPa)"] <- "Tenacidad"

# Diseño de experimento módulo de elasticidad
modelo_modu <- aov(Mod_elas ~ Cont_fibra * Tamiz * Formulacion, data = base_final)
summary(modelo_modu)

duncan.test(modelo_modu,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_modu,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_modu,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_modu,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_modu,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

HSD.test(modelo_modu,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_modu,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_modu,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_modu,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_modu,"Cont_fibra",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,"Tamiz",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,"Formulacion",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_modu,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")


# Diseño de experimento Resistencia a la tensión
modelo_resis <- aov(Resistencia ~ Cont_fibra * Tamiz * Formulacion, data = base_final)
summary(modelo_resis)
duncan.test(modelo_resis,"Cont_fibra",group = FALSE)$comparison%>% filter(signif. == " ")
duncan.test(modelo_resis,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_resis,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_resis,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison  %>% filter(signif. == " ")

LSD.test(modelo_resis,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

HSD.test(modelo_resis,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_resis,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_resis,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_resis,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_resis,"Cont_fibra",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,"Tamiz",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,"Formulacion",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_resis,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")

# Diseño de experimento Elongación a la rotura
modelo_elon <- aov(Elongacion ~ Cont_fibra * Tamiz * Formulacion, data = base_final)
summary(modelo_elon)
duncan.test(modelo_elon,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_elon,"Tamiz",group = FALSE)$comparison%>% filter(signif. == " ")
duncan.test(modelo_elon,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_elon,c("Cont_fibra","Tamiz", "Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_elon,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_elon,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_elon,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_elon,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

HSD.test(modelo_elon,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_elon,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_elon,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_elon,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison

LSD.test(modelo_elon,"Cont_fibra",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_elon,"Tamiz",group = FALSE, p.adj = 'bon')$comparison%>% filter(signif. == " ")
LSD.test(modelo_elon,"Formulacion",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_elon,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")

# Diseño de experimento Tenacidad
modelo_tenac <- aov(Tenacidad ~ Cont_fibra * Tamiz * Formulacion, data = base_final)
summary(modelo_tenac)
duncan.test(modelo_tenac,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_tenac,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_tenac,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
duncan.test(modelo_tenac,c("Cont_fibra","Tamiz", "Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_tenac,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

HSD.test(modelo_tenac,"Cont_fibra",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_tenac,"Tamiz",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_tenac,"Formulacion",group = FALSE)$comparison %>% filter(signif. == " ")
HSD.test(modelo_tenac,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE)$comparison %>% filter(signif. == " ")

LSD.test(modelo_tenac,"Cont_fibra",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,"Tamiz",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,"Formulacion",group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")
LSD.test(modelo_tenac,c("Cont_fibra","Tamiz","Formulacion"),group = FALSE, p.adj = 'bon')$comparison %>% filter(signif. == " ")

