rm(list=ls()) # Lo primero que hago es borrar el environment

# Lo primero que tenemos que tener en cuenta es que hay 3 variables.
# Debido a ello, vamos a tener 9 funciones de pertenencia (3 funciones * variable)

# Hay 3 rangos, T, H y W. Vamos a definirlos ahora en R

W <- seq(0, 1000, by=0.1)
H <- seq(0, 100,  by=0.1)
T <- seq(40, 120, by=0.1)

# Ahora que los tengo definidos, tengo que definir la variable temperatura.
# Esta variable seguirÃ¡ una estrctura similar al ejercicio anteriormente entregado.
# Lo que voy a hacer es definir las funciones y entonces sacarlas mediante plots y lines.

# Por lo tanto, vamos a comenzar con la TEMPERATURA.

#Init mu_x

mu_x <- seq(0, length(T))

#FunciÃ³n TRIANGULAR
mu_x <- rep(0, length(T))

mu_x[ T > 80] <- 1
mu_x[ T > 60 & T <= 80 ] <- (T[ T > 60 & T <= 80 ] - 60) / (80 - 60)
T_Baja <- mu_x
plot(T, mu_x, type = "l", col="green")

mu_x <- rep(0, length(T))

mu_x[ T <= 80 ] <- 1
mu_x[ T > 80 & T < 100 ] <- (100 - T[ T > 80 & T < 100 ]) / (100-80)
T_Media <- mu_x
lines(T, mu_x, type="l", col="red")

# Ahora que tengo cada cosa sacada, debo de pasar a hacer la otra funciÃ³n, la TRAPEZIODAL

mu_x <- rep(0, length(T))

mu_x[ T > 80 ] <- 1
mu_x[ T > 60 & T <= 80 ] <- (T[ T > 60 & T <= 80 ] - 60) / (80 - 60)
T_Media <- mu_x
lines(T, T_Media, type="l", col="blue")

mu_x <- rep(0, length(T))

mu_x[ T < 80] <- 1
mu_x[ T >= 80 & T < 100 ] <- (100 - T[ T >= 80 & T < 100 ]) / (100 - 80)
T_Alta <- mu_x
lines(T, T_Alta, type="l", col="pink")

# Ya tenemos hechas las dos funciones.

#Ahora ya hemos definido las funciones sobre temperatura, tenemos que pasar a las funciones sobre velocidad angular (w)

mu_x <- seq(0, length(W))

#Hacemos exactamente el mismo procedimiento que antes, primero la TRIANGULAR

mu_x <- rep(0, length(W))

mu_x[ W > 500 ] <- 1
mu_x[ W > 250 & W <= 500 ] <- (W[ W > 250 & W <= 500 ] - 250) / (500 - 250)
Vel_Baja <- mu_x
plot(W, Vel_Baja, type="l", col="red")

mu_x <- rep(0, length(W))

mu_x[ W < 500 ] <- 1
mu_x[ W >= 500 & W < 750 ] <- (750 - W[ W >= 500 & W < 750 ]) / (750 - 500)
Vel_Media <- mu_x
lines(W, Vel_Media, type="l", col="green")

# Ahora vamos con la TRAPEZOIDAL

mu_x <- rep(0, length(W))

mu_x[ W >= 500] <- 1
mu_x[ W > 250 & W < 500 ] <- (W[ W > 250 & W < 500 ] - 250) / (500 - 250)
Vel_Media <- mu_x
lines(W, Vel_Media, type="l", col="pink")

mu_x <- rep(0, length(W))

mu_x[ W < 500 ] <- 1
mu_x[ W >= 500 & W < 750 ] <- (750 - W[ W >= 500 & W < 750 ]) / (750 - 500)
Vel_Alta <- mu_x
lines(W, Vel_Alta, type="l", col="orange")

v_angular <- mu_x

#Finalmente, tenemos que hacer la variable de Humedad (H). Seguiremos el mismo procedimiento que con las dos anteriores.

mu_x <- seq(0, length(H))

#Tal y como hemos hecho antes, empezamos por la funciÃ³n TRIANGULAR

mu_x <- rep(0, length(H))

mu_x[ H >= 50 ] <- 1
mu_x[ H > 25 & H < 50 ] <- (H[ H > 25 & H < 50 ] - 25) / (50 - 25)
Hum_Baja <- mu_x
plot(H, Hum_Baja, type="l", col="red")

mu_x <- rep(0, length(H))

mu_x[ H <= 50 ] <- 1
mu_x[ H >= 50 & H < 75 ] <- (75 - H[ H >= 50 & H < 75 ]) / (75-50)
Hum_Media <- mu_x
lines(H, Hum_Media, type="l", col="black")

# Ahora vamos con la funciÃ³n TRAPEZIODAL

mu_x <- rep(0, length(H))

mu_x[ H >= 50 ] <- 1
mu_x[ H > 25 & H < 50 ] <- (H[ H > 25 & H < 50 ] - 25) / (50 - 25)
Hum_Media <- mu_x
lines(H, Hum_Media, type="l", col="green")

mu_x <- rep(0, length(H))

mu_x[ H <= 50 ] <- 1
mu_x[ H >= 50 & H < 75 ] <- (75 - H[ H >= 50 & H < 75 ]) / (75 - 50)
Hum_Alta <- mu_x
lines(H, Hum_Alta, type="l", col="orange")


# Ahora ya hemos definido todas las reglas difusas.
# Una vez que hemos hecho esto, debemos ahora de definir las que pone en el enunciado como "reglas de control".
# Para ello, vamos a combinarlas en un array cada una, a partir de los elementos que las forman.
# Y nos fijamos en el cuadro del enunciado.

# Vamos a definirlas:

## Regla 1: Humedad baja y temperatura baja --> Velocidad Angular Baja

regla1 <- c(T_Baja, Hum_Baja, Vel_Baja)

## Regla 2: Humedad baja,  temperatura media --> Velocidad Angular Media

regla2 <- c(T_Media, Hum_Baja, Vel_Media)

## Regla 3: Humedad baja, temperatura alta --> Velocidad Angular Media

regla3 <- c(T_Alta, Hum_Baja, Vel_Media)

## Regla 4: Humedad media, temperatura baja --> Velocidad Angular baja

regla4 <- c(T_Baja, Hum_Media, Vel_Baja)

## Regla 5: Humedad Alta, Temperatura baja --> Velocidad Angular Media

regla5 <-- c(T_Baja, Hum_Alta, Vel_Media)

## Regla 6: Humedad media y temperatura alta --> Velocidad Angular Alta

regla6 <-- c(T_Alta, Hum_Media, Vel_Alta)

## Regla 7: Humedad alta y temperatura media --> Velocidad Angular Media

regla7 <-- c(T_Media, Hum_Alta, Vel_Media)

## Regla 8: Humedad media y temperatura media --> Velocidad Angular Media

regla8 <-- c(T_Media, Hum_Media, Vel_Media)

## Regla 9: Humedad alta y temperatura alta --> Velocidad Angular Alta

regla9 <-- c(T_Alta, Hum_Alta, Vel_Alta)


# Pues ya tenemos definidas las 9 reglas (El cuadrito estÃ¡ implementado en R)

H <- rbind(regla1, regla2, regla3, regla4, regla5, regla6, regla7, regla8, regla9)

### Cálculo de la función de centro gravitatorio

defus_cGrav <- function(v_angular)
{
  
  num <- sum(W * v_angular)
  denom <- sum (v_angular)
  w.needed <- num/denom
  
  w.needed 		
  
}

# Devuelvo el valor de w para temp=60 y humedad=40
defus_cGrav(v_angular)

