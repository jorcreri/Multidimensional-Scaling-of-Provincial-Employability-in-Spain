# TRABAJO EMPLOYMENT. 
# Modelización Estadística de Datos de Alta Dimensión.
# Escalado multidimensional de la empleabilidad provincial española.

# Jorge Crespo Rivas (j.crespo.rivas@udc.es) 
# Jesús Estévez Amoedo (j.esteveza@udc.es)

################################################################################
# EJERCICIO 1. ESTUDIO DESCRIPTIVO.

head(employment)
summary(employment) 

attach(employment)
windows()
par(mfrow=c(3,2))
barplot(inm.1000, main = 'Inmigración por cada mil')
barplot(em.1000, main = 'Emigración por cada mil')
boxplot(activos, main = 'Activos', col = 'orange')
boxplot(empleados, main = 'Empleados', col = 'lightblue')
boxplot(en.paro, main = 'En paro', col = 'forestgreen')

par(mfrow=c(2,2))

diferencia <- inm.1000 - em.1000
plot(diferencia, en.paro, main = "Comparación entre Diferencia de Inmigrantes/Emigrantes y Tasa de Activos",
     xlab = "Diferencia Inmigrantes - Emigrantes", ylab = "Tasa de Paro (%)", 
     pch = 19, col = "blue")


diferencia <- inm.1000 - em.1000
plot(diferencia, empleados , main = "Comparación entre Diferencia de Inmigrantes/Emigrantes y Tasa de Empleados",
     xlab = "Diferencia Inmigrantes - Emigrantes", ylab = "Tasa de Empleados (%)", 
     pch = 19, col = "blue")

# Análisis del PIB frente a Tasa de Empleados
plot(empleados, pib, main = "PIB vs. Tasa de Empleados (%)",
     xlab = "Tasa de Empleados (%)", ylab = "PIB", 
     pch = 19, col = "blue")
abline(lm(pib ~ empleados), col = "red")

# Análisis del PIB frente a Tasa de Paro
plot(en.paro, pib, main = "PIB vs. Tasa de Paro (%)",
     xlab = "Tasa de Paro (%)", ylab = "PIB", 
     pch = 19, col = "blue")
abline(lm(pib ~ en.paro), col = "red")

# El IPC debido a su baja variabilidad no lo vamos a usar como parámetro para evaluar 
# su influencia en otras variables.

################################################################################
# EJERCICIO 2. MATRIZ DE DISTANCIAS.

library(StatMatch)

muestras.emp <- employment[,]

Distancias.emp <- mahalanobis.dist(muestras.emp)

diag(Distancias.emp) <- NA

min_distancia <- min(Distancias.emp, na.rm = TRUE)
max_distancia <- max(Distancias.emp, na.rm = TRUE)

min_distancia
max_distancia

which(Distancias.emp == min_distancia, arr.ind = TRUE)
which(Distancias.emp == max_distancia, arr.ind = TRUE)

################################################################################
# EJERCICIO 5. MDS.

par(mfrow=c(1, 1))
emp<- scale(employment)
Distancia <- dist(emp)

D <- as.matrix(Distancia)
D
n <- 52
A <-(-1/2)*D^2
A
H <-diag(rep((n-1)/n,length.out=n))
H[which(H==0)]<--1/n
H
B<-H %*% A %*% H
B

################################################################################
# EJERCICIO 6. DIMENSIONES NECESARIAS.
# Autovalores
lambda <- eigen(B)$values
lambda
# Proporción de varianza explicada
var_exp <- lambda / sum(lambda)

# Varianza acumulada
var_exp_acum <- cumsum(var_exp)

# Ver proporciones
print(var_exp_acum)

################################################################################
# EJERCICIO 7. COORDENADAS

autovec <- eigen(B)$vectors # Autovectores
autovec
vec12 <- autovec[,c(1:3)]
lambda123 <- lambda[1:3]
result <- vec12*sqrt(lambda123)
result

################################################################################
# EJERCICIO 8. REPRESENTACIÓN.

names.c <- c(
  "Albacete", "Alicante", "Almería", "Álava", "Asturias", "Ávila", "Badajoz", 
  "Barcelona", "Burgos", "Cáceres", "Cádiz", "Cantabria", "Castellón", 
  "Ceuta", "Ciudad Real", "Córdoba", "Cuenca", "Girona", "Granada", "Guadalajara", 
  "Guipúzcoa", "Huelva", "Huesca", "Illes Balears", "Jaén", "La Coruña", 
  "La Rioja", "Las Palmas", "León", "Lleida", "Lugo", "Madrid", "Málaga", 
  "Melilla", "Murcia", "Navarra", "Ourense", "Palencia", "Pontevedra", 
  "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria", 
  "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya", 
  "Zamora", "Zaragoza"
)

library(scatterplot3d)

# Crear el gráfico 3D
s3d <- scatterplot3d(result,
  main = "MDS Resultados en 3D",
  xlab = "Dim 1",
  ylab = "Dim 2",
  zlab = "Dim 3",
  pch = 19,      
  color = "blue"
)

# Agregar etiquetas usando las coordenadas proyectadas
coords_2d <- s3d$xyz.convert(result[,1], result[,2], result[,3])  
text(
  x = coords_2d$x, 
  y = coords_2d$y, 
  labels = names.c,  # Etiquetas para las provincias
  pos = 3, cex = 0.7, col = "darkred"
)

################################################################################
# EJERCICIO 9. ROTACIÓN. 
result.rot <- cbind(result[,1],result[,3],-result[,2])
result.rot


# Crear el nuevo gráfico 3D con las coordenadas rotadas
s3d_rot <- scatterplot3d(result.rot,
                         main = "MDS Resultados Rotados en 3D",
                         xlab = "Dim 1",
                         ylab = "Dim 2",
                         zlab = "Dim 3",
                         pch = 19,
                         color = "blue"
)

# Agregar etiquetas usando las coordenadas proyectadas del gráfico rotado
coords_2d_rot <- s3d_rot$xyz.convert(result.rot[,1], result.rot[,2], result.rot[,3])
text(
  x = coords_2d_rot$x,
  y = coords_2d_rot$y,
  labels = names.c, 
  pos = 3, cex = 0.7, col = "darkred"
)

################################################################################
# EJERCICIO 10. APLICAR MÉTODO DE R. 

cmdscale(D, k=3)
# Observamos que los resultados son los mismos

################################################################################
# EJERCICIO 11. MISMO PROCEDIMIENTO -> VARIABLES, NO OBSERVACIONES.

library(scatterplot3d)

mat_cor <- cor(employment)

dist_var <- as.dist(1 - mat_cor) # Usamos 1 - correlación como medida de distancia

eigen_values <- eigen(mat_cor)$values

var_exp <- eigen_values / sum(eigen_values)

var_exp_acum <- cumsum(var_exp)

print(var_exp)        # Proporción de varianza explicada por dimensión
print(var_exp_acum)   # Varianza acumulada

plot(var_exp_acum, type = "b", 
     main = "Varianza Acumulada por Dimensión", 
     xlab = "Número de Dimensiones", 
     ylab = "Varianza Acumulada", 
     pch = 19, col = "black", ylim = c(0, 1))

# Aplicar MDS a las variables
mds_var <- cmdscale(dist_var, k = 3)  # Reducimos a 2 dimensiones

# Crear el gráfico 3D y guardar el objeto
s3d <- scatterplot3d(
  x = mds_var[, 1],  # Coordenada de la primera dimensión (eje X)
  y = mds_var[, 2],  # Coordenada de la segunda dimensión (eje Y)
  z = mds_var[, 3],  # Coordenada de la tercera dimensión (eje Z)
  main = "MDS de las Variables", 
  xlab = "Dim 1", 
  ylab = "Dim 2", 
  zlab = "Dim 3",  # Etiqueta del eje Z
  pch = 19,        # Puntos sólidos
  color = "black"  # Color azul para los puntos
)

coords_2d <- s3d$xyz.convert(mds_var[, 1], mds_var[, 2], mds_var[, 3])

text(
  x = coords_2d$x, 
  y = coords_2d$y, 
  labels = colnames(employment),  
  pos = 3, cex = 0.7, col = "red"
)