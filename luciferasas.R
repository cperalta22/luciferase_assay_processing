# MANEJADOR DE DATOS DE LUCIFERASA A PARTIR DE UN ARCHIVO .CSV MAESTRO 
# Ejemplo del formato requerido del archivo maestro:
#
#   Experimento	Celulas	Plasmido	Luciferasa	Renilla
#   PILOTO    	K562	  POS_CNTRL	1.307	      1099
#   PILOTO    	K562	  VV	      0.328	      952.1
#
# NOTA : EL CONTROL POSITIVO SIEMPRE SE DEBE LLAMAR POS_CNTRL <--- OJO!!!
library(hash)
library(lattice)
library(plyr)
library(ggplot2)

luci <- read.csv("luciferasa_master_file.csv", stringsAsFactors = FALSE)
luci$RLA_Value <-luci$Luciferasa/luci$Renilla # crea columna de valores: luciferasa/renilla

# hash para reemplazar nombres de celulas y condiciones con indices para ser ordenados posteriormente
table_hash <-
  read.table(
    "luciferasa_master_hash.tsv" , # el hash es un indice de valores y sus reemplazos
    header = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("NA")
  )
hash_codenames <- hash(keys = table_hash$V1, values = table_hash$V2)

for (i in 1:(nrow(luci))) { # asigna nombres con un indice para que se muestren en el orden correcto
  luci$Plasmido[i] <- values(hash_codenames, luci$Plasmido[i])
  luci$Celulas[i] <- values(hash_codenames, luci$Celulas[i])
}
celulas <- unique(luci$Celulas)# se usa mas adelante para relativizar vs el control positivo "POS_CNTRL"
condiciones <- unique(luci$Plasmido)
experimento <- unique(luci$Experimento)  

##### CAMBIOS DEL 2 DE JUNIO DEL 2018 ######
for (i in 1:length(experimento)){ # establece valores relativos al control positivo de cada experimento
  temp_df <- subset(luci, luci$Experimento==experimento[i])
  temp_df <- subset(temp_df, temp_df$Plasmido==values(hash_codenames,"POS_CNTRL"))
  relativizador <- mean(temp_df$RLA_Value)
  for (j in 1:nrow(luci)){
    if (luci$Experimento[j]==experimento[i]){
      luci$RLA_Value[j]<- luci$RLA_Value[j]/relativizador
    }
  }
}
# boxplot con lattice
bwplot(Plasmido~RLA_Value | Celulas, data= luci, 
       notch = T,
       layout=c(length(celulas),1),
       par.settings = list( box.umbrella=list(col= c("gold2", "red")),
                            plot.symbol=list(col= c("gold2", "red")),
                            box.dot=list(col= c("gold2", "red")),
                            box.rectangle = list(col= c("gold2", "red"))
                            ),
       main= "Distribucion global de los datos"
       )

# Generar grafica de barras con FUNCION DEFINIDA POR EL USUARIO
barras_con_error <- function(X, TITULO){
bar_data <- ddply(X, c("Celulas", "Plasmido"), summarise, # nuevo df usando plyr!!.
                  n= length(RLA_Value),
                  mean= mean(RLA_Value),
                  sd= sd(RLA_Value),
                  se= sd/sqrt(n))

limits <- aes(ymax = bar_data$mean + bar_data$se, 
              ymin = bar_data$mean - bar_data$se)# delimitaciones de tamaño de la grafica ggplot2

info_plot <- ggplot(data = bar_data, aes(x = Plasmido, y = mean,
                               fill = Celulas))# info del objeto ggplot2 incluye
# el df a usar , y lo que se va a plotear

info_plot + geom_bar(stat = "identity", # parametros de ploteo de barras
                     position = position_dodge(0.9),colour="black") +
  geom_errorbar(limits, position = position_dodge(0.9),# parametro de barras de error
                width = 0.25) +
  labs(x = "Construcciones", y = "Unidades Relativas de Luciferasa") +# etiquetas
  ggtitle(TITULO) +
  scale_fill_discrete(name = "Lineas Celulares")+
  coord_flip()# grafica horizontal

}

# Barplot global
barras_con_error(luci, "Actividad de luciferasa")

# Barplot por linea celular
# for (i in 1:length(celulas)){
#   sub_luci <- luci[luci$Celulas==celulas[i],]
#   sub_titulo <- paste0("Actividad de luciferasa ",celulas[i])
#   print(barras_con_error(sub_luci,sub_titulo))
# }

# Barplot global construcciones grandes
orig_luci<- luci[!(luci$Plasmido %in% c("09-INI-F","08-INI-R","05-FIN-F","03-FIN-X-R",
                                "02-MYC-MT-F","01-MYC-MT-R", "209-ENH_CNTRL",
                                "207-ENH_LARGE","208-ENH_SMALL",  "07-MED-F"  ,    "06-MED-R")),]
sub_titulo <- "Actividad de luciferasa\nconstrucciones originales"
barras_con_error(orig_luci,sub_titulo)

# Barplot por linea celular construcciones originales
# for (i in 1:length(celulas)){
#   sub_luci <- orig_luci[orig_luci$Celulas==celulas[i],]
#   sub_titulo <- paste0("Actividad de luciferasa ",celulas[i],"\nconstrucciones originales")
#   print(barras_con_error(sub_luci,sub_titulo))
# }

# Barplot global construcciones cortas
cortas_luci<- luci[!(luci$Plasmido %in% c("11-LARGE-F","13-CP-F","12-CP-R","15-MR-F",
                                          "14-MR-R","10-LARGE-R","02-MYC-MT-F","01-MYC-MT-R","209-ENH_CNTRL",
                                          "207-ENH_LARGE","208-ENH_SMALL")),]
sub_titulo <- "Actividad de luciferasa\nconstrucciones cortas"
barras_con_error(cortas_luci,sub_titulo)

# Barplot por linea celular construcciones originales
# for (i in 1:length(celulas)){
#   sub_luci <- cortas_luci[cortas_luci$Celulas==celulas[i],]
#   sub_titulo <- paste0("Actividad de luciferasa ",celulas[i],"\nconstrucciones cortas")
#   print(barras_con_error(sub_luci,sub_titulo))
# }

# Barplot global construcciones MYC
MYC_luci<- luci[!(luci$Plasmido %in% c("11-LARGE-F","13-CP-F","12-CP-R","15-MR-F",
                                          "14-MR-R","10-LARGE-R","09-INI-F","08-INI-R", 
                                       "05-FIN-F","03-FIN-X-R", "06-MED-R","07-MED-F",
                                       "209-ENH_CNTRL",
                                       "207-ENH_LARGE","208-ENH_SMALL"
                                      )),]
sub_titulo <- "Actividad de luciferasa\nsitio mutado MYC"
barras_con_error(MYC_luci,sub_titulo)

# Barplot por linea celular construcciones MYC
# for (i in 1:length(celulas)){
#   sub_luci <- MYC_luci[MYC_luci$Celulas==celulas[i],]
#   sub_titulo <- paste0("Actividad de luciferasa ",celulas[i],"\nsitio mutado MYC")
#   print(barras_con_error(sub_luci,sub_titulo))
# }

########### GRAFICAS SEMESTRE 4 #############

# # Barplot global construcciones MYC K562 MCF7
# k5m7<- MYC_luci[MYC_luci$Celulas!=celulas[2],]
# sub_titulo <- "Actividad de luciferasa\nsitio mutado MYC"
# barras_con_error(k5m7, sub_titulo)
# 
# # Barplot global construcciones cortas K562 MCF7
# 
# # Barplot global construcciones cortas
# cortas_luci<- luci[!(luci$Plasmido %in% c("11-LARGE-F","13-CP-F","12-CP-R","15-MR-F",
#                                           "14-MR-R","10-LARGE-R","02-MYC-MT-F","01-MYC-MT-R",
#                                           "209-ENH_CNTRL",
#                                           "207-ENH_LARGE","208-ENH_SMALL")),]
# cortas_luci_k5m7<- cortas_luci[cortas_luci$Celulas!=celulas[2],]
# sub_titulo <- "Actividad de luciferasa\nconstrucciones cortas"
# barras_con_error(cortas_luci_k5m7,sub_titulo)

# elementos regulatorios
reg_luci <- luci[luci$Celulas==celulas[1],]
reg_luci<- reg_luci[!(reg_luci$Plasmido %in% c("11-LARGE-F",    "13-CP-F",       "12-CP-R",       "19-SMALL-F"   ,
                                       "16-SMALL-R" ,   "15-MR-F"    ,   "14-MR-R"   ,    "10-LARGE-R" ,   "09-INI-F" ,     "08-INI-R"     ,
                                       "05-FIN-F"   ,   "03-FIN-X-R"  ,  "02-MYC-MT-F",   "01-MYC-MT-R" ,  "07-MED-F"  ,    "06-MED-R"     
                                      )),]
sub_titulo <- "Actividad de luciferasa\nFragmentos con enriquecimiento de H3K4me3"
barras_con_error(reg_luci,sub_titulo)

