#######################################################################
#ESTUDIO DE LAS VENTAS DE VIDEOJUEGOS.R                               #
#######################################################################

#Documentos necesarios
#Avg_Sales
#Abs_data_per_region
#All_Data (all sales data)


#Grafico de líneas. Absolutos
year = Abs_data_per_region$Year_of_Release
na_s = Abs_data_per_region$NA_Sales
eu_s = Abs_data_per_region$EU_Sales
jp_s = Abs_data_per_region$JP_Sales
ot_s = Abs_data_per_region$Other_Sales
Global = Abs_data_per_region$Global_Sales

plot(year, na_s, type='l', main="Ventas por Año", xlab="Año",
     ylab="Unidades vendidas (por millón)", col="Red", ylim=c(0,700), lwd=3)
lines(year, eu_s, col="Blue", type="l", lwd=3)
lines(year, jp_s, col="Orange", type="l", lwd=3)
lines(year, ot_s, col="Brown", type="l", lwd=3)
lines(year, Global, col="Black", type="l", lwd=3)


#Gráfico de líneas. Medias
avg_year = Avg_Sales$Year_of_Release
avg_na_s = Avg_Sales$NA_Avg_Sales
avg_eu_s = Avg_Sales$EU_Avg_Sales
avg_jp_s = Avg_Sales$JP_Avg_Sales
avg_ot_s = Avg_Sales$Other_Avg_Sales
avg_Global = Avg_Sales$Global_Avg_Sales

plot(avg_year, avg_na_s, type='l', main="Ventas medias por Año", xlab="Año",
     ylab="Media de unidades vendidas (por millón)", col="Red", ylim=c(0,5), lwd=3)
lines(avg_year, avg_eu_s, col="Blue", type="l", lwd=3)
lines(avg_year, avg_jp_s, col="Orange", type="l", lwd=3)
lines(avg_year, avg_ot_s, col="Brown", type="l", lwd=3)
lines(avg_year, avg_Global, col="Black", type="l", lwd=3)

#Grafico de Barras. Cantidad de Publicaciones
year_published = Abs_data_per_region$Year_of_Release
count_published = Abs_data_per_region$COUNT_PUBLISHED

barplot(count_published, names.arg=year_published, main="Publicaciones por Año", xlab="Año",
        ylab="Publicaciones totales")



#######################################################################
############Funciones_utiles
#horiz= TRUE       #Sirve para poner las tablas de barras horizontales
                   #como parámetro en barplot(..., horiz=TRUE)
#######################################################################




#Datos útiles para el análisis de los datos separado en Regiones.
################################GLOBAL################################
#Media
global_mean_80_16 = mean(All_Data$Global_Sales)

#Varianza
global_var_80_16 = var(All_Data$Global_Sales)

#Desviación típica
global_sd_80_16 = sd(All_Data$Global_Sales)

#Correlación de Pearson
global_coef_80_16 = global_sd_80_16 / global_mean_80_16

######################################################################

##################################NA##################################
#Media
na_mean_80_16 = mean(All_Data$NA_Sales)

#Varianza
na_var_80_16 = var(All_Data$NA_Sales)

#Desviación típica
na_sd_80_16 = sd(All_Data$NA_Sales)

#Correlación de Pearson
na_coef_80_16 = na_sd_80_16 / na_mean_80_16

######################################################################

################################EUROPE################################
#Media
eu_mean_80_16 = mean(All_Data$EU_Sales)

#Varianza
eu_var_80_16 = var(All_Data$EU_Sales)

#Desviación típica
eu_sd_80_16 = sd(All_Data$EU_Sales)

#Correlación de Pearson
eu_coef_80_16 = eu_sd_80_16 / eu_mean_80_16

######################################################################

#################################JAPAN################################
#Media
jp_mean_80_16 = mean(All_Data$JP_Sales)

#Varianza
jp_var_80_16 = var(All_Data$JP_Sales)

#Desviación típica
jp_sd_80_16 = sd(All_Data$JP_Sales)

#Correlación de Pearson
jp_coef_80_16 = jp_sd_80_16 / jp_mean_80_16

######################################################################

#################################OTHER################################
#Media
other_mean_80_16 = mean(All_Data$Other_Sales)

#Varianza
other_var_80_16 = var(All_Data$Other_Sales)

#Desviación típica
other_sd_80_16 = sd(All_Data$Other_Sales)

#Correlación de Pearson
other_coef_80_16 = other_sd_80_16 / other_mean_80_16

######################################################################

######################################################################
######################################################################

######################################################################
#Datos útiles para entender tendencias anormales.
#Filtro los datos que me interesan. En este caso necesito filtrar la columna year y global_sales
#x(filas,columnas)           si no hay nada coge todo y si pones c() puedes seleccionar cuales c(1:10) de 1 a 10; c(1,2,3,...) deaseadas.
year_n_globalsales <- All_Data[,c(3,10)]

#Separo la primera tendencia
first_tend <- subset(year_n_globalsales, year_n_globalsales$Year_of_Release < 2002)

#Separo la segunda tendencia
second_tend <- subset(year_n_globalsales, year_n_globalsales$Year_of_Release >= 2002)

#Realizo sus medias, varianzas, desviaciones tipicas y coeficientes de Pearson
#Media
FT_mean_80_01 = mean(first_tend$Global_Sales)
ST_mean_02_16 = mean(second_tend$Global_Sales)

#Varianza
FT_var_80_01 = var(first_tend$Global_Sales)
ST_var_02_16 = var(second_tend$Global_Sales)

#Desviación típica
FT_sd_80_01 = sd(first_tend$Global_Sales)
ST_sd_02_16 = sd(second_tend$Global_Sales)

#Correlación de Pearson
FT_coef_80_01 = FT_sd_80_01 / FT_mean_80_01
ST_coef_02_16 = ST_sd_02_16 / ST_mean_02_16
######################################################################
