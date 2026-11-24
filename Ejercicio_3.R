#Ejercicio 3: Considere el experimento de lanzar dos dados justos y analizar la distribución de la suma de sus 
#resultados.

#Dado 1 (solo representación)
dado_1 <- c(1:6)

#Dado 2 (solo representación)
dado_2 <- c(1:6)

#literal a)
set.seed(123)
n <- 50000

#suma
suma <- numeric(n)

for (i in 1:n){
    suma[i] <- sum(sample(1:6, size = 2, replace = TRUE))
}  

cat("Suma primera tirada:", suma[1], "\n")

cat("Suma quinta tirada:", suma[5], "\n")

#b)

frecuencia_ab <- table(suma)
frecuencia_ac <- cumsum(frecuencia_ab)
frecuencia_rel <- frecuencia_ab / n
frecuencia_porcentual <- frecuencia_rel * 100
frecuencia_rel_acum <- cumsum(frecuencia_rel)

tabla_frecuencias <- data.frame(
    Suma = as.numeric(names(frecuencia_ab)),
    Frecuencia = as.numeric(frecuencia_ab),
    Frecuencia_Acumulada = as.numeric(frecuencia_ac),
    Frecuencia_Relativa = as.numeric(frecuencia_rel),
    Frecuencia_Porcentual = as.numeric(frecuencia_porcentual),
    Frecuencia_Relativa_Acumulada = as.numeric(frecuencia_rel_acum)
)

print (tabla_frecuencias, row.names = FALSE)

#c)

barplot(
    frecuencia_ab,
    main = "Sumas observadas",
    xlab = "Suma de los dados",
    ylab = "Frecuencia"
)

#d)
prob <- numeric(12)
for (i in 1:6){
    for (j in 1:6){
        prob[i + j] <- prob[i + j] + 1
    }  
}  

for (i in 2:12){
    prob[i] <- prob[i] / 36
}  

tabla_comparativa <- data.frame(
  Suma = 2:12,
  Observada = as.numeric(table(factor(suma, levels = 2:12))) / n,
  Teorica = prob[2:12]
)

print(tabla_comparativa, row.names = FALSE)

