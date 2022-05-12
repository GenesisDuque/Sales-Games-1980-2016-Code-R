#contraste de ventas
#H0: Medias de NA y Japon son iguales
#H1: Medias son distintas

#varianza desconocida
#Bilateral

#añadimos alpha
alpha = 0.05

#variables
x1 = Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales
x2 = Analisis_de_genero_VideoGames_Sales_1980_2016$JP_sales
n = 12

#No conocemos  si las varianzas son iguales o distintas
#Apliicamos Fisher 
#bilateral

#hallamos la region critica
print(c(-qf(alpha/2,n-1,n-1,lower.tail = FALSE),qf(alpha/2,n-1,n-1,lower.tail = FALSE)))
#hallamos el estadistico
var.test(x1,x2,alternative='two.sided', conf.level = 1-alpha)

#resultado
#El p-value es bastante bajo y F= 8,06 dentro de la regón critica. por tanto las
#varianzas son distintas . Rechamos H0

#hallamos ahora el contraste de igualdad de medias
#Aplicamos t student
print(c(-qt(alpha/2,n-1,lower.tail = FALSE),qt(alpha/2,n-1,lower.tail = FALSE)))
t.test(x1,x2, alternative='two.sided', conf.level = 1-alpha, var.equal =FALSE)

#resultados
#p-value < 1 y t= 3.36 esta en la zona critica  por tanto rechazamos H0
#Por tanto la medias y varianzas sobre las ventas de America del norte y Japon 
#difieren

#-------------------------------------------------------------------------------

#variables
x3= Analisis_de_genero_VideoGames_Sales_1980_2016$EU_sales

#Hipotesis 
#H0: la media de america es mayor que la de Europa
#H1: la media de europa es mayor a America 

#No conocemos  si las varianzas son iguales o distintas
#Apliicamos Fisher 
#unilateral

#H0 : varianzad iguales
#H1 : varianzas distintas

#hallamos la region critica
print(c(-qf(alpha/2,n-1,n-1,lower.tail = FALSE),qf(alpha/2,n-1,n-1,lower.tail = FALSE)))
#hallamos el estadistico
var.test(x1,x3,alternative='less', conf.level = 1-alpha)

#resultados
# el p_valor es bastante alto por tanto Aceptamos H0 = varianzas iguales

#hallamos ahora el contraste de igualdad de medias
#Aplicamos t student
print(c(-qt(alpha/2,n-1,lower.tail = FALSE),qt(alpha/2,n-1,lower.tail = FALSE)))
t.test(x1,x3, alternative='less', conf.level = 1-alpha, var.equal =TRUE)

#resultados
#el p-value es cerca del 90% por tanto aceptamos H0 media America es mas grande 
#que la europea

#-------------------------------------------------------------------------------

#variable
x4 = Analisis_de_genero_VideoGames_Sales_1980_2016$Others_sales

#Hipotesis
#H0: Medias de NA y Otras regiones son iguales
#H1: Medias son distintas

#No conocemos  si las varianzas son iguales o distintas
#Apliicamos Fisher 
#Bilateral

#H0 : varianzad iguales
#H1 : varianzas distintas

#hallamos la region critica
print(c(-qf(alpha/2,n-1,n-1,lower.tail = FALSE),qf(alpha/2,n-1,n-1,lower.tail = FALSE)))
#hallamos el estadistico
var.test(x1,x4,alternative='two.sided', conf.level = 1-alpha)

#resultados
#P-value es muy pequeño 1.2e-05 . Rechazamos H0 , las varianzas son distintas


#hallamos ahora el contraste de igualdad de medias
#Aplicamos t student
print(c(-qt(alpha/2,n-1,lower.tail = FALSE),qt(alpha/2,n-1,lower.tail = FALSE)))
t.test(x1,x4, alternative='two.sided', conf.level = 1-alpha, var.equal =FALSE)

#resultados
#p-value por debajo de 1 valor muy pequeño y valor t dentro de la region critica
#por tanto rechazamos H0 medias son diferentes para sobre las ventas de America y 
#otras regiones

