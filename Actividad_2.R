#ACTIVIDAD 2

# Preparación del entorno de trabajo (importación de datos y librerías):
estudio1<-read.csv("/Users/emmettdiez/Desktop/estudio1.csv", na.strings = "", fileEncoding = "UTF-8-BOM") 

#library(tidyverse)
install.packages("lawstat")
install.packages("car")
install.packages("nortest")
install.packages("gtsummary")
library(ggplot2)
library(tidyverse)
library(car)
library(lawstat)
library(gtsummary)


# En primer lugar, comprobamos la homogeneidad de varianzas mediante el Test de Levene.
#Para los valores significativos <0.05 no existen diferencias significativas (hay homogeneidad) y viceversa si el valor es >0.05.
with (estudio1Res, leveneTest(alimento1, sexo))
with (estudio1Res, leveneTest(alimento2, sexo))
with (estudio1Res, leveneTest(alimento3, sexo))
with (estudio1Res, leveneTest(alimento4, sexo))
with (estudio1Res, leveneTest(alimento5, sexo))
with (estudio1Res, leveneTest(alimento6, sexo))
with (estudio1Res, leveneTest(alimento7, sexo))
with (estudio1Res, leveneTest(alimento8, sexo))
with (estudio1Res, leveneTest(alimento9, sexo))
with (estudio1Res, leveneTest(alimento10, sexo))
with (estudio1Res, leveneTest(alimento11, sexo))
with (estudio1Res, leveneTest(alimento12, sexo))
with (estudio1Res, leveneTest(alimento13, sexo))
with (estudio1Res, leveneTest(alimento14, sexo))
with (estudio1Res, leveneTest(alimento15, sexo))
with (estudio1Res, leveneTest(alimento16, sexo))
with (estudio1Res, leveneTest(alimento17, sexo))
with (estudio1Res, leveneTest(alimento18, sexo))
with (estudio1Res, leveneTest(alimento19, sexo))
with (estudio1Res, leveneTest(alimento20, sexo))

with (estudio1Res, leveneTest(nutriente1, sexo))
with (estudio1Res, leveneTest(nutriente2, sexo))
with (estudio1Res, leveneTest(nutriente3, sexo))
with (estudio1Res, leveneTest(nutriente4, sexo))
with (estudio1Res, leveneTest(nutriente5, sexo))
with (estudio1Res, leveneTest(nutriente6, sexo))
with (estudio1Res, leveneTest(nutriente7, sexo))
with (estudio1Res, leveneTest(nutriente8, sexo))
with (estudio1Res, leveneTest(nutriente9, sexo))
with (estudio1Res, leveneTest(nutriente10, sexo))
with (estudio1Res, leveneTest(nutriente11, sexo))
with (estudio1Res, leveneTest(nutriente12, sexo))
with (estudio1Res, leveneTest(nutriente13, sexo))
with (estudio1Res, leveneTest(nutriente14, sexo))
with (estudio1Res, leveneTest(nutriente15, sexo))
with (estudio1Res, leveneTest(nutriente16, sexo))
with (estudio1Res, leveneTest(nutriente17, sexo))
with (estudio1Res, leveneTest(nutriente18, sexo))
with (estudio1Res, leveneTest(nutriente19, sexo))

# EJERCICIO 1: Comprobacion de la normalidad con el test de Anderson Darling (pvalor >0,05 para aceptar H0):
# H0: los datos analizados de los alimentos siguen una distribución normal.
# H1: los daos analizados de los alimentos no siguen una distribución normal.

library(nortest)
ad.test(estudio1$alimento1)
ad.test(estudio1$alimento2)
ad.test(estudio1$alimento3)
ad.test(estudio1$alimento4)
ad.test(estudio1$alimento5)
ad.test(estudio1$alimento6)
ad.test(estudio1$alimento7)
ad.test(estudio1$alimento8)
ad.test(estudio1$alimento9)
ad.test(estudio1$alimento10)
ad.test(estudio1$alimento11)
ad.test(estudio1$alimento12)
ad.test(estudio1$alimento13)
ad.test(estudio1$alimento14)
ad.test(estudio1$alimento15)
ad.test(estudio1$alimento16)
ad.test(estudio1$alimento17)
ad.test(estudio1$alimento18)
ad.test(estudio1$alimento19)
ad.test(estudio1$alimento20)

#El P valor es menor que el nivel de significancia 0.05 por lo que rechazamos la H0 (para aceptarla p-valor >0,05). Los alimentos no siguen una distribucion normal.

ad.test(estudio1$nutriente1)
ad.test(estudio1$nutriente2)
ad.test(estudio1$nutriente3)
ad.test(estudio1$nutriente4)
ad.test(estudio1$nutriente5)
ad.test(estudio1$nutriente6)
ad.test(estudio1$nutriente7)
ad.test(estudio1$nutriente8)
ad.test(estudio1$nutriente9)
ad.test(estudio1$nutriente10)
ad.test(estudio1$nutriente11)
ad.test(estudio1$nutriente12)
ad.test(estudio1$nutriente13)
ad.test(estudio1$nutriente14)
ad.test(estudio1$nutriente15)
ad.test(estudio1$nutriente16)
ad.test(estudio1$nutriente17)
ad.test(estudio1$nutriente18)
ad.test(estudio1$nutriente19)

#Los p valor de los nutrientes también son menores que 0.05 por lo que tampoco siguen una distribución normal.

# Como comprobación adicional, los siguientes histogramas corroboran que los datos no siguen una distribución normal. 
#A este fenómeno en que la mayor parte d elos datos se agrupan a la izquierda de la campana se le conoce como curtosis.
#Todos los histogramas que hemos probado presentan leptocurtosis (los datos se concentran de forma putiaguda y elevada).
ggplot(estudio1Res, aes(x=alimento1)) +
  geom_histogram(fill="aquamarine", color="black", binwidth=30) +
  labs(title= "Distribución del alimento1",
       subtitle="Histograma de distribución de alimentos",
       x="Alimento1",
       y="Frecuencia") +
  theme_minimal()
ggplot(estudio1Res, aes(x=alimento17)) +
  geom_histogram(fill="aquamarine", color="black", binwidth=30) +
  labs(title= "Distribución del alimento17",
       subtitle="Histograma de distribución de alimentos",
       x="Alimento17",
       y="Frecuencia") +
  theme_minimal()
ggplot(estudio1Res, aes(x=nutriente5)) +
  geom_histogram(fill="aquamarine", color="black", binwidth=10) +
  labs(title= "Distribución del nutriente5",
       subtitle="Histograma de distribución de nutrientes",
       x="Nutriente5",
       y="Frecuencia") +
  theme_minimal()
ggplot(estudio1Res, aes(x=nutriente15)) +
  geom_histogram(fill="aquamarine", color="black", binwidth=10) +
  labs(title= "Distribución del nutriente15",
       subtitle="Histograma de distribución de nutrientes",
       x="Nutriente15",
       y="Frecuencia") +
  theme_minimal()


# EJERCICIO 2: Calcular y analizar estadísticas descriptivas de los valores sin transformar de los alimentos y nutrientes seleccionados en cuatro categorías definidas por sexo y rango de IMC.

estudio1$categorias<-cut(estudio1$IMC, breaks = c(-Inf, 30, Inf), labels = c("bajo", "alto"))
estudio1$categoria_final<- with(estudio1,ifelse(sexo==1 & categorias=="bajo", 1, 
                                                ifelse(sexo==1 & categorias=="alto", 2, 
                                                       ifelse(sexo==2 & categorias=="bajo", 3, 4))))
estudio1$categoriasdos<-with(estudio1, ifelse (sexo==1, "hombre", "mujer"))

# Reordeno mi dataframe estudio1 para limpiarlo de datos que no necesito.
estudio1Res <- estudio1%>% 
  select(categorias, categoria_final, IMC, sexo, alimento1:alimento20, nutriente1:nutriente19)

# Calculamos la mediana. Puede hacerse con summary como las dos priemras líneas, con cada alimento y nutriente, o bien condensarse con apply:
summary(estudio1Res$alimento1)
summary(estudio1Res$nutriente1)

apply(estudio1Res[, 5:24], 2, median)
apply(estudio1Res[, 25:43], 2, median)

# El rango intercuartilico es el tercer cuartil menos el primer cuartil:
# Puede calcularse con las siguientes dos líneas de código individualmente para cada alimento y nutriente, o condensarse con el comando apply.
quantile(estudio1Res$alimento1, probs= c(0, 0.25, 0.75, 1))
quantile(estudio1Res$nutriente1, probs= c(0, 0.25, 0.75, 1))

apply(estudio1Res[, 5:24], 2, quantile, probs=c(0, 0.25, 0.75, 1))
apply(estudio1Res[, 25:43], 2, quantile, probs=c(0, 0.25, 0.75, 1))


# EJERCICIO 3: en la Tabla 2 aparece un p-value correspondiente al t-test. En este, las hipótesis son:
# HO: Para cada alimento, existen diferencias entre las categorías creadas con respecto al IMC y el Sexo.
# H1: Para cada alimento, no existen diferencias entre las categorías creadas con respecto al IMC y al Sexo.

# TABLA RESUMEN

select_gtsummary <- estudio1Res %>% select(c("categorias", "sexo", "alimento1", "alimento2", "alimento3", "alimento4", "alimento5", "alimento6", "alimento7", "alimento8", "alimento9", "alimento10", "alimento11", "alimento12", "alimento13", "alimento14", "alimento15", "alimento16", "alimento17", "alimento18", "alimento19", "alimento20",
                                     "nutriente1", "nutriente2", "nutriente3", "nutriente4", "nutriente5", "nutriente6", "nutriente7", "nutriente8", "nutriente9", "nutriente10", "nutriente11", "nutriente12", "nutriente13", "nutriente14", "nutriente15", "nutriente16", "nutriente17", "nutriente18", "nutriente19"))

tabla <- select_gtsummary %>%
  tbl_strata(strata=sexo,
             .tbl_fun = ~.x %>%
               tbl_summary(by=categorias,
                           type=starts_with("alimento")~"continuous",
                           statistic = all_continuous() ~ "{median} ({p25}-{p75})") %>%
               add_p(test = list(all_continuous() ~ "t.test"),
                     pvalue_fun = ~ style_pvalue(.x, digits = 3)))

tabla


