#---------------------------------------------------------------------------------------------------#

# Eugenio Alamillos Jimenez – eugenio.alamillos@gmail.com
# TFM – Master Data Science – Kschool

# En este TFM se pretende hacer un “cluster” de usuarios de Google Analytics. Para ello, se utilizará la API disponible 
# para extraer los datos directamente de las bases de datos de Google Analytics. Una vez extraídos los datos, se trataran y 
# limpiarán para, posteriormente, hacer un “k-means” y extraer los resultados en un documento .csv. 
# Para finalizar, ese documento .csv se debería utilizar para enriquecer las listas de remarketing, retargeting… 
# de Google Ads para optimizar la inversión en este tipo de publicaciones pagadas.

#---------------------------------------------------------------------------------------------------#

# Limpiamos todos los datos por precaución

rm(list=ls())

# Esta función te llevará al navegador para que aceptes el acceso a tus cuentas de Google Analytics
# IMPORTANTE: si tu correo de gmail tiene doble autentificación, tendrás que desactivar estar opción
# Para corregir este TFM, os daré acceso a mi cuenta personal de Google Analytics
# Si en la sesión tienes activos varios correos, tendrás que seleccionar el correspondiente en la consola 
# (al correo que tiene acceso al Google Analytics)
ga_auth()

# Listar todas las cuentas de Google Analytics asociadas al correo
# IMPORTANTE -- YA TENÉIS ACCESO A LA CUENTA DE ANALYTICS. SI NO ES ASÍ, AVISADME.
# Para que el código funcione correctamente, se tiene que desactivar temporalmente la verificación en dos pasos de las cuentas de GMAIL
account_list <- ga_account_list()
account_list$viewId

#Seleccionar la vista correspondiente "Sin Spam - Sin IP internas"
ga_id <- 136886270

#Crear varias consultas simples a la API para comprobar que retorna valores
google_analytics(ga_id, 
                 date_range = c("2019-12-01", "2019-12-07"), 
                 metrics = "sessions", 
                 dimensions = "date")

#Añadir configuración para evitar el muestreo de datos (cuando retornemos grandes cantidades de datos)
google_analytics(ga_id, 
                 date_range = c("2019-12-01", "2019-12-07"), 
                 metrics = "pageviews",
                 dimensions = "pageTitle",
                 anti_sample = TRUE)
# Prueba 2 de datos
google_analytics(ga_id, 
                 date_range = c("2019-01-01", "2019-12-07"), 
                 metrics = "sessions", 
                 dimensions = "month")
# Prueba 3 de datos
google_analytics(ga_id, 
                 date_range = c("2019-01-01", "2019-12-07"), 
                 metrics = "sessions", 
                 dimensions = "month",
                 anti_sample = TRUE)

# Patrones de ordenación
delta_sess <- order_type("sessions","DESCENDING")

google_analytics(ga_id, 
                 date_range = c("2019-01-01", "2019-12-31"), 
                 metrics = c("sessions","timeOnPage","avgTimeOnPage","pageviews","entranceRate","pageviewsPerSession"),
                 dimensions = "dimension3",
                 anti_sample = TRUE,
                 order = delta_sess)

# Una vez ejecutada la consulta, vemos que hay un valor "false" que hay que eliminar para el análisis. Este valor "false"
# es debido a una mala implementación de esta dimensión implementada que luego se corrigió.

sin_comprar_entradas <- google_analytics(ga_id, 
                 date_range = c("2019-01-01", "2019-12-31"), 
                 metrics = c("sessions","timeOnPage","avgTimeOnPage","pageviews","entranceRate","pageviewsPerSession"),
                 dimensions = "dimension3",
                 anti_sample = TRUE,
                 order = delta_sess,
                 filtersExpression = "ga:dimension3!=false")


# Una vez limpiado estos datos, hacemos otra consulta para extraer los eventos de tipo "Comprar entradas"

solo_comprar_entradas<- google_analytics(ga_id, 
                 date_range = c("2019-01-01", "2019-12-31"), 
                 metrics = c("totalEvents"),
                 dimensions = "dimension3",
                 anti_sample = TRUE,
                 filtersExpression = "ga:eventCategory=@Compra")

# Unimos los dos dataset dónde la dimension3 será el nexo de unión

dataset_final <- merge(sin_comprar_entradas, solo_comprar_entradas, by="dimension3")

# Comprobamos que la unión es correcta y nos ponemos con los cluster
my_data <- dataset_final
my_data


# Extraemos solo una parte de los datos y comprobamos el tipo
head(my_data)
typeof(my_data)


# Comprobamos el tipo de la dimension3
typeof(my_data$dimension3)

# Extraemos los nombres de filas
rownames(my_data)

# Para que funcione correctamente la aplicación, tenemos que sustituir el nombre de las filas por dimension3
row.names(my_data) <- my_data$dimension3
my_data

# Una vez cambiado el nombre a la primera columna, eliminamos la columna duplicada
my_data$dimension3 <- NULL

head(my_data)

# Creamos una variable "limpia" para las conclusiones finales
limpia <- my_data
limpia2 <- my_data

#Verificamos si el dataset tiene NA's
sapply(my_data,function(x) any(is.na(x)))

#No queremos que el algoritmo dependa de una unidad variable arbitraria
my_data<-scale(my_data)
head(my_data)


# Veamos las distancias entre cada estado
distance <- get_dist(my_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# El plot que se muestra no es muy representativo, se valorará incluir menos usuarios para siguientes pruebas

# "Entrenamos" el kmeans para ver la primera visualización
my_kmeans_2<-kmeans(my_data,centers = 2,nstart = 10)
str(my_kmeans_2)
my_kmeans_2


# Visualizamos el cluster
fviz_cluster(my_kmeans_2, data = my_data)

# Como podemos apreciar en la visualización, hay varios outliers que trataremos
hist(my_data)

# Hacer cluster sobre 2 agrupaciones
kmeans.result <- kmeans(my_data,centers = 2,nstart = 10)
# centro de los cluster
kmeans.result$centers
# calcular la distancia sobre los objetos y centros de los cluster
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((my_data - centers)^2))
# seleccionar las 5 grandes distancias (se podría ampliar a 10 llegado el caso)
# En nuestro ejemplo, con eliminar los 5 más lejanos es suficiente
outliers <- order(distances, decreasing=T)[1:5]
# identificar outliers
outliers

#una vez identificados los outliers, los eliminamos de nuestros datos
test_remove <- my_data[-c(outliers),] 
test_remove
hist(test_remove)

# Volvemos a hacer el mismo proceso con la variable "limpia" para sacar conclusiones finales
# Hacer cluster sobre 2 agrupaciones
kmeans.result <- kmeans(limpia,centers = 2,nstart = 10)
# centro de los cluster
kmeans.result$centers
# calcular la distancia sobre los objetos y centros de los cluster
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((my_data - centers)^2))
# seleccionar las 5 grandes distancias (se podría ampliar a 10 llegado el caso)
# En nuestro ejemplo, con eliminar los 5 más lejanos es suficiente
outliers <- order(distances, decreasing=T)[1:5]
# identificar outliers
outliers

#una vez identificados los outliers, los eliminamos de nuestros datos
limpia <- my_data[-c(outliers),] 
limpia



# "Entrenamos" el kmeans para ver la primera visualización con los datos limpios
my_kmeans_2_clean<-kmeans(test_remove,centers = 2,nstart = 10)
str(my_kmeans__clean)
my_kmeans_2_clean


# Visualizamos el cluster con datos limpios
fviz_cluster(my_kmeans_2_clean, data = test_remove)


#--------------------- Inicio: Visualizacion con outliers------------------------------#

#"entrenamos" otros kmeans
my_kmeans_3 <- kmeans(my_data, centers = 3, nstart = 10)
my_kmeans_4 <- kmeans(my_data, centers = 4, nstart = 10)
my_kmeans_5 <- kmeans(my_data, centers = 5, nstart = 10)

# plots para comparar
plot_1 <- fviz_cluster(my_kmeans_2, geom = "point", data = my_data) + ggtitle("k = 2")
plot_2 <- fviz_cluster(my_kmeans_3, geom = "point",  data = my_data) + ggtitle("k = 3")
plot_3 <- fviz_cluster(my_kmeans_4, geom = "point",  data = my_data) + ggtitle("k = 4")
plot_4 <- fviz_cluster(my_kmeans_5, geom = "point",  data = my_data) + ggtitle("k = 5")


grid.arrange(plot_1, plot_2, plot_3, plot_4, nrow = 2)


#--------------------- Fin: Visualizacion con outliers------------------------------#

#--------------------- Inicio: Visualizacion sin outliers------------------------------#

#"entrenamos" otros kmeans
my_kmeans_3_clean <- kmeans(test_remove, centers = 3, nstart = 10)
my_kmeans_4_clean <- kmeans(test_remove, centers = 4, nstart = 10)
my_kmeans_5_clean <- kmeans(test_remove, centers = 5, nstart = 10)

# plots para comparar
plot_1_clean <- fviz_cluster(my_kmeans_2_clean, geom = "point", data = test_remove) + ggtitle("k = 2")
plot_2_clean <- fviz_cluster(my_kmeans_3_clean, geom = "point",  data = test_remove) + ggtitle("k = 3")
plot_3_clean <- fviz_cluster(my_kmeans_4_clean, geom = "point",  data = test_remove) + ggtitle("k = 4")
plot_4_clean <- fviz_cluster(my_kmeans_5_clean, geom = "point",  data = test_remove) + ggtitle("k = 5")


grid.arrange(plot_1_clean, plot_2_clean, plot_3_clean, plot_4_clean, nrow = 2)


#--------------------- Fin: Visualizacion sin outliers------------------------------#



#---------------- Inicio: Numero optimo de clusters (Elbow method) --------------------#
# En esta apartado detectaremos el número aproximado de cluster que son necesarios
# El número ideal de cluster se identificará cuándo el error empiece a normalizarse
# Para estar seguros, utilizaremos varias funciones diferentes para detectar el número idóneo



#1º función - Resultado = 4-5

fviz_nbclust(test_remove, kmeans, method = "wss")


#2ª función - Resultado = 5
fviz_nbclust(test_remove, kmeans, method = "silhouette")

#3ª función  Resultado = 5-6

# Errores sin datos y número máximos de cluster
Errores <-NULL
K_Max   <-10

# Ejecuta kmeans con diferentes cluster, desde 1 hasta 10
# Luego guarda el error de cada ejecucion en el vector "Errores"
for (i in 1:K_Max)
{
  Errores[i] <- sum(kmeans(test_remove[-1], centers=i)$withinss)
}

# Grafica el vector "Errores"
plot(1:K_Max, Errores, type="b", 
     xlab="Cantidad de Cluster", 
     ylab="Suma de error")

# 4ª función  Resultado = 5-6
# Me gusta esta funcionalidad que he encontrado donde te sugiere varios números óptimos. 
# Entre los sugeridos: 5,6,8 y 10

cal_fit2 <- cascadeKM(test_remove, 1, 10, iter = 1000)
plot(cal_fit2, sortg = TRUE, grpmts.plot = TRUE)

# 5ª función Resultado = 5

gap_stat <- clusGap(test_remove, FUN = kmeans, nstart = 25,
                    K.max = 9, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)



#---------------- Fin: Numero optimo de clusters nos quedamos con 5 --------------------#



#---------------- Inicio: Resultados finales --------------------#

my_final_kmeans <- kmeans(test_remove, 5, nstart = 10)
print(my_final_kmeans)

fviz_cluster(my_final_kmeans, data = test_remove)

#centros de cada cluster
limpia2  %>%
  mutate(Cluster = my_final_kmeans$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")



#---------------- Fin: Resultados finales --------------------#
