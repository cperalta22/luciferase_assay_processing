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
library(outliers)


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


for (i in 1:length(celulas)){ # establece valores relativos al control positivo de cada linea celular
  temp_df <- subset(luci, luci$Celulas==celulas[i])
  temp_df <- subset(temp_df, temp_df$Plasmido==values(hash_codenames,"POS_CNTRL"))
  relativizador <- mean(temp_df$RLA_Value)
  for (j in 1:nrow(luci)){
    if (luci$Celulas[j]==celulas[i]){
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

# dotplot con lattice
dotplot(Plasmido~RLA_Value | Celulas, data= luci, 
       notch = T,
       layout=c(length(celulas),1),
       par.settings = list( box.umbrella=list(col= c("gold2", "red")),
                            plot.symbol=list(col= c("gold2", "red")),
                            box.dot=list(col= c("gold2", "red")),
                            box.rectangle = list(col= c("gold2", "red"))
       ),
       main= "Distribucion global de los datos"
)


# busqueda de outliers
bar_data <- ddply(luci, c("Celulas", "Plasmido"), summarise, # nuevo df usando plyr!!.
                  n= length(RLA_Value),
                  mean= mean(RLA_Value),
                  sd= sd(RLA_Value),
                  se= sd/sqrt(n),
                  out_up_lim= mean +(sd*3),
                  out_lw_lim= mean -(sd*3))


for (i in 1:length(celulas)){
  print("=============================================================================================")
  print(celulas[i])
  for (j in 1:length(condiciones)){
    if (condiciones[j] %in% luci[luci$Celulas==celulas[i],]$Plasmido){
      vector_luci <- luci[luci$Celulas==celulas[i] & luci$Plasmido==condiciones[j],]$RLA_Value
      out_vector_luci <- vector_luci[
        vector_luci<=bar_data[bar_data$Celulas==celulas[i] & bar_data$Plasmido==condiciones[j],]$out_lw_lim
        |
        vector_luci>=bar_data[bar_data$Celulas==celulas[i] & bar_data$Plasmido==condiciones[j],]$out_up_lim  
          ]
      print(condiciones[j])
      print(vector_luci)
      print(outlier(vector_luci))
      print(out_vector_luci)
    }
  }
}
