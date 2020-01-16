library(data.table)
library(readxl)
library(ggplot2)
library(WriteXLS)
library(plyr)
library(dplyr)
library(glmnet)

###PARTE 1
aprender = fread("aprender.csv", 
           na.strings = c(""), 
           dec=",")

diccionario = read_excel("aprender2018-diccionario-primaria-6.xlsx", range = cell_cols("A:B"))

# reemplazo NAs por mediana
na_columns = colSums(is.na(aprender))
na_columns = na_columns[na_columns > 0]
num_nas = names(na_columns)

for(j in num_nas){
  set(aprender, j=j, value=ifelse(is.na(aprender[[j]]), 
                                  median(aprender[[j]], na.rm=T), 
                                  aprender[[j]]))
}

ggplot(data = aprender, aes(x = mpuntaje)) + geom_histogram()

diccionario = na.omit(diccionario)

#4.c
#WriteXLS(diccionario, ExcelFileName = "Diccionario.xls")
#2.d)
names(aprender) <- diccionario$Etiqueta[match(names(aprender), diccionario$Variable)]
# 3)
aprender1 <- aprender
aprender1 <- aprender1[aprender1$Sexo!=-6 , ]
aprender1 <- aprender1[aprender1$Sexo!=-9 , ]
Sexo.labs <- c("Varones", "Mujeres")
names(Sexo.labs) <- c("1", "2")
ggplot(data = aprender1, aes(x = `Puntaje en Lengua`)) + 
  geom_histogram() + 
  facet_grid(Sexo ~ ., labeller = labeller(Sexo = Sexo.labs)) + 
  ggtitle("Distribución en Puntaje de Lengua por Sexo")  

ggplot(data = aprender1, aes(x = `Puntaje en Matemática`)) + 
  geom_histogram() +
  facet_grid(Sexo ~ ., labeller = labeller(Sexo = Sexo.labs)) + 
  ggtitle("Distribución en Puntaje de Matemática por Sexo")  

#4
cor(aprender$`Puntaje en Lengua`,aprender$`Puntaje en Matemática`)

#5
aprender1 <- aprender1[aprender1$`Indice socioeconómico del alumno`!=-1 , ]
Tabla_frecuencia <- as.data.frame(table(aprender1$`Indice socioeconómico del alumno`))
Tabla_frecuencia$Porcentaje <- round(Tabla_frecuencia$Freq/(sum(Tabla_frecuencia$Freq))*100, 1)
Tabla_frecuencia$Var1 <- c("Bajo", "Medio", "Alto")
ggplot(data= Tabla_frecuencia, aes(x="", y=Porcentaje, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(Porcentaje, "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Agrupación por Nivel Socioeconomico")

#6
Tabla_Provincias <- as.data.frame(table(aprender1$`Indice socioeconómico del alumno`, aprender1$`Número de jurisdicción`))
Tabla_Provincias <- Tabla_Provincias [c(2,1,3)]
diccionario=read_excel("aprender2018-diccionario-primaria-6.xlsx", range = "C529:D552", col_names = FALSE)
Tabla_Provincias$Var2 <- diccionario$"...2"[match(Tabla_Provincias$Var2, diccionario$"...1")]
setDT(Tabla_Provincias)
Total_provincia= Tabla_Provincias[, .(total = sum(Freq)), by=.(Var2)]
Tabla_Provincias$TOTAL <- Total_provincia$total[match(Tabla_Provincias$Var2, Total_provincia$Var2)]
Tabla_Provincias$Porcentaje <- round((Tabla_Provincias$Freq)/(Tabla_Provincias$TOTAL)*100, 1)
Tabla_Provincias <- subset(Tabla_Provincias, select = -c(Freq, TOTAL))
colnames(Tabla_Provincias)[colnames(Tabla_Provincias) == 'Var2'] <- 'Provincia'
colnames(Tabla_Provincias)[colnames(Tabla_Provincias) == 'Var1'] <- 'Nivel Socioeconómico'
Tabla_Provincias

### PARTE 2
#1
aprender2 <- subset(aprender, select = -c(`Nivel de desempeño en Matemática`))

#2 
aprender1
target_var = "Indice socioeconómico del alumno"
features = setdiff(names(aprender2), target_var)

X = aprender2[, features, with=F]
for(j in names(X)) set(X, j=j, value=as.numeric(X[[j]]))

y = aprender2[[target_var]]
X = data.matrix(X)
cv_ridge = cv.glmnet(X, y, alpha=0)
plot(cv_ridge)

mse_minimo = which.min(cv_ridge$cvm)
cv_ridge$lambda[mse_minimo]

# coincide con el grafico, log(lambda) = -1
log(cv_ridge$lambda[mse_minimo])

