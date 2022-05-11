#===================================================================================================
#cargar_datos: Esta función emplea el nombre de un alumno para generar una semilla aleatoria
#y así asignar un establecimiento de salud y sus datos
#Entrada: nombres de alumnos
#Salida: datos
#===================================================================================================

cargar_datos=function(nombre1,nombre2)
{
  #Codificar nombres y definir semilla
  entrada = c(nombre1,nombre2)
  entrada=c(strsplit(nombre1,"")[[1]],strsplit(nombre2,"")[[1]])
  diccionario = c(LETTERS[1:26],letters[1:26],"ñ","Ñ")
  conversion=match(entrada,diccionario)
  conversion[which(is.na(conversion))]=0
  conversion=sum(conversion)
  
  #Lectura de archivos y creación de tablas
  tabla=read.csv("datos.csv",header=T,sep=";")
  set.seed(conversion)

  #Nombre de hospital
  ind=which(tabla$Nombre.hospital!="")
  nombre_hospital=paste(sample(tabla$Nombre.hospital[ind],1),sample(tabla$Frutas,1),sep=" ")
  
  #Datos
  largo=20000
  sexo=sample(c("Femenino","Masculino","Otro"),largo,replace=T,prob = c(0.45,0.45,0.1))
  ind=which(tabla$Enfermedades!="")
  diagnostico=sample(tabla$Enfermedades[ind],largo,replace=T)
  ind=which(tabla$Comorbilidades!="")
  comorbilidades=sample(tabla$Comorbilidades[ind],largo,replace=T)
  probabilidad=runif(1,0,0.3)
  egresos=sample(c("Fallecido(a)","Alta"),largo,replace=T,prob = c(probabilidad,1-probabilidad))
  datos=data.frame(nombre_hospital=nombre_hospital,diagnostico,
                   comorbilidad=comorbilidades,sexo,egreso=egresos)
  datos=count(datos,c('diagnostico','comorbilidad','sexo','egreso'))
  datos$DE=sample(seq(1:200),nrow(datos),replace=T)
  datos$prom_edad=sample(seq(0:120),nrow(datos),replace=T)
  datos$ds_edad=round(runif(nrow(datos),0,5),2)
  datos$nombre_hospital=rep(nombre_hospital,nrow(datos))
  datos=datos[,c(9,1,2,3,7,8,4,6,5)]
  return(datos)
}
  
    
  
  