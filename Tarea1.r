usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, repos = "https://cran.itam.mx/")
  suppressPackageStartupMessages(require(p, character.only = TRUE, quietly  = TRUE))
}


#Funcion amnesia, recibe un vector x numerico
#Dos numeros t,s>0
#Regresa |P(X>s)-P(X>t+s|X>t)|

amnesia <- function(x,t,s){
    p_s <- length(x[x>s])/length(x) # P(X>s)
    n_s <- length(x[x>t])           # Número de elementos en X tal que X>t
    p_ts <- length(x[x>t+s])/n_s    #P(X>t+s|X>t)
    return (abs(p_s-p_ts))
    
}


X <- rexp(100,1)#Muestra aleatoria de 100 observaciones
Y <- rexp(1000000,1)#Muestra aleatoria de 1000 observaciones
t <- 0.5
s <- 2

#Para una muestra con 100 observaciones
amnesia(X,t,s)

#Para una muestra con 1000,000 observaciones
amnesia(Y,t,s)

#Probabilidad real
1-pexp(s,1)

#Probabilidad dada por la muestra de tamaño 100
length(X[X>s])/length(X)

#Probabilidad dada por la muestra tamaño 1000,000
length(Y[Y>s])/length(Y)

usePackage('Rlab')

#Generamos 500 observaciones bernoulli con p = 1/2
#Asociamos un 1 al éximo "cae sol en el lanzamiento de la moneda"
N <- 500
x <- rbern(N, 0.5)
#Calculamos ahora la proporcion de soles en cada tiro
proporcion_soles <- cumsum(x)/1:N

#Realizamos la gráfica
plot(1:N,proporcion_soles,type = 'l',ylim =c(0,1),xlab = 'No. de lanzamientos',
     ylab = 'Proporción de Soles/Lanzamientos',col = 'blue')


