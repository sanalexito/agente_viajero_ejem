#---- Parámetros ---------------------------------------------------------------
#Se ocupan las distancias entre las ciudades
#para el ejemplo fijo la semilla
set.seed(1234)
x = 1:10 #ciudades
n = 10   #número de ciudades a visitar

#Se crea una sucesión de números aleatorios que representan las distancias 
#entre ciudades
d <- sample(20:100, 100, replace = T)

#Metemos estas distancias en una matriz
D <- matrix(d, nrow = 10, ncol = 10, byrow = T)

tampob <- 10
tamaño_muestra <- 3
#Tm <- tasa de mutación
#Se hacen cero los elementos de la diagonal
for(i in 1:n) for(j in 1:n){ 
  if(i==j){
    D[i, j] = 0
  }else{
    D[i, j] <- D[i, j]
  }
}

#Hacemos ahora la función de adaptación
#Se inicializa con una ruta inicial y se le da una matriz
#devuelve lo que vamos a llamar el costo total

funcion_adaptacion <- function(vector, distancias){
  total = 0 #es la suma de distancias
  for( i in 1:(n-1)){
    #pongo la ciudad donde estoy primero y luego la ciudad 
    #a la que debo llegar
    posicion1 <- i
    posicion2 <- i+1
    
    total = total + D[posicion1, posicion2]
  }
  
  primera <- vector[1]
  ultima <- vector[n]
  
  distancia_total <- total + D[ultima, primera]
  
  return(distancia_total)
}


#Población inicial #######################################################
#se crea matriz con ceros y luego se va rellenando
poblacion <- matrix(0, nrow = tampob, ncol = n)
FEV <- 0
for(i in 1:tampob){
  poblacion[i,] <- sample(x,n)
  FEV[i] = funcion_adaptacion(poblacion[i,]) 
}

#Seleccipon torneo #######################################################

seleccion_torneo <- function(){
  posibles <- sample(1:tampob, tamaño_muestra)
  minimo <- min(FEV[posibles])
  a  = which(FEV[posibles]== minimo)
  b = a[1]
  
  k = posibles[b]
  return(poblacion[k,])
}

seleccion_torneo()


#función cruzamiento #####################################################
funcion_cruzamiento <- function(padre, madre){
  n = length(padre)
  a = sample(1:n, 1)
  prohibidos <- abs(c(2-a, 1-a, a, a+1, a+2))
  co = 1:n
  b = sample(co[-prohibidos])
  pc1 = min(a,b)
  pc2 = max(a,b)
  
  x = c(padre[1:pc1], padre[pc2:n])
  
  n3 = length(x)
  y = madre
  
  for(i in 1:n3){
    y = y[!y%in%x[i]]
  }
  H1 <- c(padre[1:pc1], y, padre[pc2:n])
  
 #para el hijo 2
  n = length(madre)
  a = sample(1:n, 1)
  prohibidos <- abs(c(2-a, 1-a, a, a+1, a+2))
  co = 1:n
  b = sample(co[-prohibidos])
  pc1 = min(a,b)
  pc2 = max(a,b)
  
  x = c(madre[1:pc1], madre[pc2:n])
  
  n3 = length(x)
  y = padre
  
  for(i in 1:n3){
    y = y[!y%in%x[i]]
  }
  H2 <- c(madre[1:pc1], y, madre[pc2:n])
  
  hijos <- list(hijo_1 = H1, hijo_2 = H2)
  
  return(hijos)
}


pa = seleccion_torneo()
ma = seleccion_torneo()
pa
ma

funcion_cruzamiento(pa, ma)


#FUNCION MUTACION ############################################################
funcion_mutacion <- function(hijo){
  pm = sample(1:n, 2)
  pm1 = min(pm)
  pm2 = max(pm)
  
  posicion <- x[pm1]
  x[pm1] = x[pm2]
  x[pm2] = posicion
  
  return(x)
}
hhh <- seleccion_torneo()
hhh
funcion_mutacion(hhh)

#iterativo #################################################################

#poblacion inicial 
poblacion <- matrix(0, nrow = tampob, ncol = n)
FEV <- 0
for(i in 1:tampob){
  poblacion[i,] <- sample(x,n)
  FEV[i] = funcion_adaptacion(poblacion[i,]) 
}


#iteraciones----
maxi_iteraciones =3
TC = 0.3
Tm = 0.2
for(i in 1:maxi_iteraciones){
  #seleccion de padres#
  Padre = seleccion_torneo()
  Madre = seleccion_torneo()
  
  #cruzamiento y obtención de hijos# 
  if(runif(1)<TC){
    cruz <- funcion_cruzamiento(Padre, Madre)
    Hijo_1 <- cruz[1]
    Hijo_2 <- cruz[2]
  }else{
    Hijo_1 <- Padre
    Hijo_2 <- Madre
  } 

  #seleccion de un hijo para mutación
  if(runif(1)<0.5){
    Hijo <- Hijo_1
  }else{
    Hijo <- Hijo_2
  }
  
  #MUTACION
  if(runif(1)<Tm){ #Tm = tasa de mutación
    Hijo_mutado = funcion_mutacion(Hijo)
  }else{
    Hijo_mutado = Hijo
  }
    
  #Ingreso del hijo mutado a la población:
  existe = 0

  for(k in 1:tampob){
    if( sum( abs( unlist(Hijo_mutado) - poblacion[k,]))==0){
      existe = 1
      break
    }
  }
  
  if(existe == 0 & funcion_adaptacion(Hijo_mutado)>max(FEV)){
    pos = which( FEV == min( FEV))[1]
    poblacion[pos, ] = Hijo_mutado
    FEV[pos] = funcion_adaptacion(Hijo_mutado)
  }
  
}


FEV_incumbente = min(FEV); FEV_incumbente
d = which(FEV == min(FEV))[1]
incumbente = poblacion[d, ];incumbente

funcion_adaptacion(incumbente, D)
