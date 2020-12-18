library(readxl)
file.choose()
ruta="C:\\Users\\alber\\OneDrive\\Documentos\\Máster MII\\1º\\1º Cuatri\\Fuentes de energía\\Trabajo\\Buena Base.xlsx"

base_datos=read_excel(ruta)
names(base_datos)
base_datos2=select(base_datos,"NAME","DATE","AÑO","MES","WND_Direction_Angle",
                   "WND_Direction_Quality_Code","WND_Type_Code",
                   "WND_Speed_Rate","WND_Speed_Quality_Code")

filter_data_base2=base_datos2[base_datos2$WND_Direction_Angle!=999, ]
filter_data_base2=filter_data_base2[filter_data_base2$WND_Speed_Rate!=9999, ]
filter_data_base2$WND_Speed_Rate=filter_data_base2$WND_Speed_Rate/10

nrow(base_datos2)-nrow(filter_data_base2)
1-(nrow(filter_data_base2)/nrow(base_datos2))


#Contabilización de datos

meses=c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
num_datos=c(1,2,3,4,5,6,7,8,9,10,11,12)
j=1
for (i in 1:12) {
  num_datos[j]=nrow(filter(filter_data_base2,filter_data_base2$MES==i))
  j=j+1
}
datos_meses=data.frame(meses,num_datos)
write.table(datos_meses,file='Contabilización_Meses.csv',sep=",")


años=c(2013,2014,2015,2016,2017,2018,2019)
num_datos2=c(1,2,3,4,5,6,7)
z=1
i=2013
for (i in 2013:2019) {
  num_datos2[z]=nrow(filter(filter_data_base2,filter_data_base2$AÑO==i))
  z=z+1
}
datos_años=data.frame(años,num_datos2)
write.table(datos_años,file="Contabilización_Años.csv",sep=",")


#Diagramas de cajas y bigotes

summary(filter_data_base2)

boxplot(filter_data_base2$WND_Speed_Rate,horizontal = T,xlab="Speed_Rate")
boxplot(filter_data_base2$WND_Direction_Angle,horizontal = T,xlab="Direction_Angle")


#Cálculo de medias y varianzas
#MES
mes=c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre","TOTAL")
media_mes=c(1,2,3,4,5,6,7,8,9,10,11,12,13)
varianza_mes=c(1,2,3,4,5,6,7,8,9,10,11,12,13)
j=1
for (i in 1:12){
  media_mes[j]=mean(filter_data_base2[filter_data_base2$MES==i,]$WND_Speed_Rate)
  varianza_mes[j]=var(filter_data_base2[filter_data_base2$MES==i,]$WND_Speed_Rate)
  j=j+1
}
media_mes[13]=mean(filter_data_base2$WND_Speed_Rate)
varianza_mes[13]=var(filter_data_base2$WND_Speed_Rate)
Medias_Varianzas_Mes=data.frame(mes,media_mes,varianza_mes)

write.table(Medias_Varianzas_Mes,file="MyVMes.csv",sep=",")


#AÑO
año= c(2013,2014,2015,2016,2017,2018,2019,"TOTAL")
media_año=c(1,2,3,4,5,6,7,8)
varianza_año=c(1,2,3,4,5,6,7,8)
k=1
for (i in 2013:2019) {
  media_año[k]=mean(filter_data_base2[filter_data_base2$AÑO==i,]$WND_Speed_Rate)
  varianza_año[k]=var(filter_data_base2[filter_data_base2$AÑO==i,]$WND_Speed_Rate)
  k=k+1
}

media_año[8]=mean(filter_data_base2$WND_Speed_Rate)
varianza_año[8]=var(filter_data_base2$WND_Speed_Rate)

Medias_Varianzas_Año=data.frame(año,media_año,varianza_año)

write.table(Medias_Varianzas_Año,file="MyVAño.csv",sep=",")



#Histogramas
#Anuales

par(mfrow=c(1,3))
m=1
for (i in 2013:2019) {
  hist(filter_data_base2[filter_data_base2$AÑO==i,]$WND_Speed_Rate,main=año[m],xlab="Velocidad Viento")
  m=m+1
  }

#Mensuales
par(mfrow=c(1,2))
b=1
for (i in 1:12) {
  hist(filter_data_base2[filter_data_base2$MES==i,]$WND_Speed_Rate,main=mes[b],xlab="Velocidad Viento")
  b=b+1
}


#Rosa de los vientos
install.packages("openair")
library(openair)
names(filter_data_base2)

par(mfrow=c(1,1))
windRose(filter_data_base2,ws = "WND_Speed_Rate", wd = "WND_Direction_Angle", breaks= c(0,2,4,8,10,12,50),
         auto.text=FALSE,
         paddle=FALSE,
         annotate=FALSE,
         key= list(labels= c("0-2",
                             "2-4",
                             "4-8",
                             "8-10",
                             "10-12",
                             "12-50")),
         key.footer = "Velocidad del viento (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col=c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a", "#E69F00"))

par(mfrow=c(2,3))
x=1
for (i in 1:12) {
  windRose(filter_data_base2[filter_data_base2$MES==i,],ws = "WND_Speed_Rate", wd = "WND_Direction_Angle",main=mes[x],breaks= c(0,2,4,8,10,12,50),
           auto.text=FALSE,
           paddle=FALSE,
           annotate=FALSE,
           key= list(labels= c("0-2",
                               "2-4",
                               "4-8",
                               "8-10",
                               "10-12",
                               "12-50")),
           key.footer = "Velocidad del viento (m/s)",
           key.position = "bottom",
           par.settings=list(axis.line=list(col="lightgray")),
           col=c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a", "#E69F00"))
  x=x+1
}


#Diagrama de Weibull

suma_velocidades=cumsum(table(filter_data_base2$WND_Speed_Rate))
funcion_suma_velocidades=suma_velocidades/6700
s_v=data.frame(table(filter_data_base2$WND_Speed_Rate))

dist_Weibull=data.frame(s_v$Var1,suma_velocidades,funcion_suma_velocidades)
names(dist_Weibull)=c("Velocidad_Obs","Num_Acumulado","Frec_Acum")


dist_Weibull$Valor_Obs=as.numeric(dist_Weibull$Valor_Obs)
dist_Weibull2=dist_Weibull[1:18,]

modelo=lm(log(-log(1-dist_Weibull2$Frec_Acum))~log(dist_Weibull2$Velocidad_Obs))
plot(modelo)
summary(modelo)

ggplot(dist_Weibull2,aes(x=log(Velocidad_Obs),log(-log(1-Frec_Acum)),))+geom_point(col="red")+geom_smooth(method ="lm",se=F)

#Extraemos k(a partir de la pendiente) y c(a partir de la ordenada origen)
p=2.0385
m=-3.2246

k=p
c=exp(-m/k)

mediaw= c *gamma(1+1/k)
desvw=c*sqrt(gamma(1+2/k)-gamma(1+1/k)^2)
varw=desvw^2

install.packages("fitdistrplus")
library(fitdistrplus)

velocidad=filter_data_base2[filter_data_base2$WND_Speed_Rate!=0,]$WND_Speed_Rate
fw=fitdist(velocidad, "weibull")
par(mfrow=c(1,1))
plot(fw)

metodo=c("real","weibull")
media=c(mean(filter_data_base2$WND_Speed_Rate),mediaw)
varianza=c(var(filter_data_base2$WND_Speed_Rate),varw)

comparacion_weibull=data.frame(metodo,media,varianza)
write.table(comparacion_weibull,file="comparacion_weibull.csv",sep = ",")














#Datos por mes
datos_enero=filter(filter_data_base2,filter_data_base2$MES==1)
datos_febrero=filter(filter_data_base2,filter_data_base2$MES==2)
datos_marzo=filter(filter_data_base2,filter_data_base2$MES==3)
datos_abril=filter(filter_data_base2,filter_data_base2$MES==4)
datos_mayo=filter(filter_data_base2,filter_data_base2$MES==5)
datos_junio=filter(filter_data_base2,filter_data_base2$MES==6)
datos_julio=filter(filter_data_base2,filter_data_base2$MES==7)
datos_agosto=filter(filter_data_base2,filter_data_base2$MES==8)
datos_septiembre=filter(filter_data_base2,filter_data_base2$MES==9)
datos_octubre=filter(filter_data_base2,filter_data_base2$MES==10)
datos_noviembre=filter(filter_data_base2,filter_data_base2$MES==11)
datos_diciembre=filter(filter_data_base2,filter_data_base2$MES==12)


#Datos por año
datos_2013=filter(filter_data_base2,filter_data_base2$AÑO==2013)
datos_2014=filter(filter_data_base2,filter_data_base2$AÑO==2014)
datos_2015=filter(filter_data_base2,filter_data_base2$AÑO==2015)
datos_2016=filter(filter_data_base2,filter_data_base2$AÑO==2016)
datos_2017=filter(filter_data_base2,filter_data_base2$AÑO==2017)
datos_2018=filter(filter_data_base2,filter_data_base2$AÑO==2018)
datos_2019=filter(filter_data_base2,filter_data_base2$AÑO==2019)


filter_data_base2[filter_data_base2$AÑO==2013,filter_data_base2$velocidad]


