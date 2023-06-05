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

# Generar grafica de barras con FUNCION DEFINIDA POR EL USUARIO
barras_con_error <- function(X, TITULO){
bar_data <- ddply(X, c("Experimento", "Plasmido"), summarise, # nuevo df usando plyr!!.
                  n= length(RLA_Value),
                  mean= mean(RLA_Value),
                  sd= sd(RLA_Value),
                  se= sd/sqrt(n))

limits <- aes(ymax = bar_data$mean + bar_data$se, 
              ymin = bar_data$mean - bar_data$se)# delimitaciones de tamaño de la grafica ggplot2

info_plot <- ggplot(data = bar_data, aes(x = Plasmido, y = mean,
                               fill = Experimento))# info del objeto ggplot2 incluye
# el df a usar , y lo que se va a plotear .... aparentemente

info_plot + geom_bar(stat = "identity", # parametros de ploteo de barras
                     position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),# parametro de barras de error
                width = 0.25) +
  labs(x = "Construcciones", y = "Unidades Relativas de Luciferasa") +# etiquetas
  ggtitle(TITULO) +
  scale_fill_discrete(name = "Experimento")+
  coord_flip()# grafica horizontal
}

# Barplot global
barras_con_error(luci, "Actividad de luciferasa")

# Barplot por experimento
for (i in 1:length(experimento)){
  sub_luci <- luci[luci$Experimento==experimento[i],]
  sub_titulo <- paste0("Actividad de luciferasa ",experimento[i])
  print(barras_con_error(sub_luci,sub_titulo))
}