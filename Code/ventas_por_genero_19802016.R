#Gráficos - Pórcion de videos juegos 1980-2016

count_gender = Analisis_de_genero_VideoGames_Sales_1980_2016$Cantidad
type_gender = Analisis_de_genero_VideoGames_Sales_1980_2016$Gender

#conversión a porcentaje
Porcentaje_gender = round(count_gender*100 / 16448)

#gráfico por porcentaje
pie(Porcentaje_gender, labels = paste0(Porcentaje_gender,"%"), col =brewer.pal(n=12, name="Paired"))
legend("bottomleft",legend=type_gender, fill = brewer.pal(n=12, name="Paired"))

#Sales NA

sales_na = Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales
sales_ja = Analisis_de_genero_VideoGames_Sales_1980_2016$JA_sales
sales_eu = Analisis_de_genero_VideoGames_Sales_1980_2016$EU_sales

#barplot
barplot(count_gender, col = brewer.pal(n=12, name="Paired"), names.arg =type_gender, main="Gráfico Videosjuegos por género 1980-2016")


#Gráfico de lineas para Ventas en Norte America, Japón , Europa y otros
x = 1:12
plot(x, Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales, type="l", main="Número de ventas según el género 1980-2016", ylab= "Unidades vendidas (M)", xlab = "Género")
lines(x, Analisis_de_genero_VideoGames_Sales_1980_2016$EU_sales, col=2, type = "l")
lines(x, Analisis_de_genero_VideoGames_Sales_1980_2016$JA_sales, col=3, type="l")
lines(x,Analisis_de_genero_VideoGames_Sales_1980_2016$Others_sales, col=4, type="l")


#calcular la moda unidimensional -funtion
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
rm(Freq)
#Tabla de frecuencias para 1980 a 2016 por género NA
Freq <- as.data.frame(table(dataset_sales_games_1980_2016_NA$gender_number))
Tabla_na_gender_80_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda
mean_na_80_16 = mean(dataset_sales_games_1980_2016_NA$gender_number)
mode_na_80_16 = mode(dataset_sales_games_1980_2016_NA$gender_number)

#calculamos la varianza
var_na_80_16 = var(dataset_sales_games_1980_2016_NA$gender_number)

#calculamos la desviación típica
sd_na_80_16 = sd(dataset_sales_games_1980_2016_NA$gender_number)

#calculamos correlación de Pearson
cor_na_80_16 = sd_na_80_16 / mean_na_80_16

#historigrama
hist(dataset_sales_games_1980_2016_NA$gender_number, main="Género Norte Ámerica desde 1980 al 2016",ylab="Frecuencia", xlab="Género")

rm(Freq)
#--------------------------------------------------------------------------------

#Tabla de frecuencias para 1980 a 2016 por género EU
Freq <- as.data.frame(table(dataset_sales_games_1980_2016_EU$gender_number))
Tabla_eu_gender_80_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))
#calculamos la media y la moda
mean_eu_80_16 = mean(dataset_sales_games_1980_2016_EU$gender_number)
mode_eu_80_16 = mode(dataset_sales_games_1980_2016_EU$gender_number)

#calculamos la varianza
var_eu_80_16 = var(dataset_sales_games_1980_2016_EU$gender_number)

#calculamos la desviación típica
sd_eu_80_16 = sd(dataset_sales_games_1980_2016_EU$gender_number)

#calculamos correlación de Pearson
cor_eu_80_16 = sd_eu_80_16 / mean_eu_80_16

#historigrama
hist(dataset_sales_games_1980_2016_EU$gender_number, main="Género Europa desde 1980 al 2016",ylab="Frecuencia", xlab="Género")

#--------------------------------------------------------------------------------
rm(Freq)
#Tabla de frecuencias para 1980 a 2016 por género JP
Freq <- as.data.frame(table(dataset_sales_games_1980_2016_JP$gender_number))
Tabla_jp_gender_80_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda
mean_jp_80_16 = mean(dataset_sales_games_1980_2016_JP$gender_number)
mode_jp_80_16 = mode(dataset_sales_games_1980_2016_JP$gender_number)

#calculamos la varianza
var_jp_80_16 = var(dataset_sales_games_1980_2016_JP$gender_number)

#calculamos la desviación típica
sd_jp_80_16 = sd(dataset_sales_games_1980_2016_JP$gender_number)

#calculamos correlación de Pearson
cor_jp_80_16 = sd_jp_80_16 / mean_jp_80_16

#historigrama
hist(dataset_sales_games_1980_2016_JP$gender_number, main="Género Jápon desde 1980 al 2016" ,ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------

rm(Freq)

#Tabla de frecuencias para 1980 a 2016 por género Others
Freq <- as.data.frame(table(dataset_sales_games_1980_2016_Others$gender_number))
Tabla_others_gender_80_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda
mean_others_80_16 = mean(dataset_sales_games_1980_2016_Others$gender_number)
mode_others_80_16 = mode(dataset_sales_games_1980_2016_Others$gender_number)

#calculamos la varianza
var_others_80_16 = var(dataset_sales_games_1980_2016_Others$gender_number)

#calculamos la desviación típica
sd_others_80_16 = sd(dataset_sales_games_1980_2016_Others$gender_number)

#calculamos correlación de Pearson
cor_others_80_16 = sd_others_80_16 / mean_others_80_16  

#historigrama
hist(dataset_sales_games_1980_2016_Others$gender_number, main="Género otros paises desde 1980 al 2016",ylab="Frecuencia", xlab="Género")  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1980 a 1989 por género Others
Freq <- as.data.frame(table(dataset_gender_games_1980_1989_Others$gender_number))
Tabla_others_gender_80_89 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))
#calculamos la media y la moda 
mean_others_80_89 = mean(dataset_gender_games_1980_1989_Others$gender_number)

mode_others_80_89 = mode(dataset_gender_games_1980_1989_Others$gender_number)

#calculamos la varianza
var_others_80_89 = var(dataset_gender_games_1980_1989_Others$gender_number)

#calculamos la desviación típica
sd_others_80_89 = sd(dataset_gender_games_1980_1989_Others$gender_number)

#calculamos correlación de Pearson
cor_others_80_89 = sd_others_80_89 / mean_others_80_89

#historigrama
hist(dataset_gender_games_1980_1989_Others$gender_number, main="Género otros paises del 80 al 89",ylab="Frecuencia", xlab="Género")


rm(Freq)
#-------------------------------------------------------------------------------

#Tabla de frecuencias para 1980 a 1989 por género NA
Freq <- as.data.frame(table(dataset_gender_games_80_89_NA$Genre))
Tabla_na_gender_80_89 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                         prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_na_80_89 = mean(dataset_gender_games_80_89_NA$Genre)

mode_na_80_89 = mode(dataset_gender_games_80_89_NA$Genre)

#calculamos la varianza
var_na_80_89 = var(dataset_gender_games_80_89_NA$Genre)

#calculamos la desviación típica
sd_na_80_89 = sd(dataset_gender_games_80_89_NA$gender_number)

#calculamos correlación de Pearson
cor_na_80_89 = sd_na_80_89 / mean_na_80_89

hist(dataset_gender_games_80_89_NA$Genre, main="Género Norte Ámerica del 80 al 89",ylab="Frecuencia", xlab="Género")

#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1980 a 1989 por género EU
Freq <- as.data.frame(table(dataset_gender_games_80_89_EU$code_gender))
Tabla_eu_gender_80_89 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_eu_80_89 = mean(dataset_gender_games_80_89_EU$code_gender)

mode_eu_80_89 = mode(dataset_gender_games_80_89_EU$code_gender)

#calculamos la varianza
var_eu_80_89 = var(dataset_gender_games_80_89_EU$code_gender)

#calculamos la desviación típica
sd_eu_80_89 = sd(dataset_gender_games_80_89_EU$gender_number)

#calculamos correlación de Pearson
cor_eu_80_89 = sd_eu_80_89 / mean_eu_80_89

hist(dataset_gender_games_80_89_EU$code_gender, main="Género Europa del 80 al 89",ylab="Frecuencia", xlab="Género")

#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1980 a 1989 por género JP
Freq <- as.data.frame(table(dataset_gender_games_80_89_JP$code_gender))
Tabla_jp_gender_80_89 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_jp_80_89 = mean(dataset_gender_games_80_89_JP$code_gender)

mode_jp_80_89 = mode(dataset_gender_games_80_89_JP$code_gender)

#calculamos la varianza
var_jp_80_89 = var(dataset_gender_games_80_89_JP$code_gender)

#calculamos la desviación típica
sd_jp_80_89 = sd(dataset_gender_games_80_89_JP$code_gender)

#calculamos correlación de Pearson
cor_jp_80_89 = sd_jp_80_89 / mean_jp_80_89

hist(dataset_gender_games_80_89_JP$code_gender, main="Género Jápon del 80 al 89",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

rm(Freq)

#Tabla de frecuencias para 1990 a 1999 por género NA
Freq <- as.data.frame(table(dataset_gender_games_90_99_NA$code_gender))
Tabla_na_gender_90_99 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_na_90_99 = mean(dataset_gender_games_90_99_NA$code_gender)

mode_na_90_99 = mode(dataset_gender_games_90_99_NA$code_gender)

#calculamos la varianza
var_na_90_99 = var(dataset_gender_games_90_99_NA$code_gender)

#calculamos la desviación típica
sd_na_90_99 = sd(dataset_gender_games_90_99_NA$code_gender)

#calculamos correlación de Pearson
cor_na_90_99 = sd_na_90_99 / mean_na_90_99

hist(dataset_gender_games_90_99_NA$code_gender, main="Género Norte Ámerica del 90 al 99",ylab="Frecuencia", xlab="Género")


#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1990 a 1999 por género EU
Freq <- as.data.frame(table(dataset_gender_games_90_99_EU$code_gender))
Tabla_eu_gender_90_99 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_eu_90_99 = mean(dataset_gender_games_90_99_EU$code_gender)

mode_eu_90_99 = mode(dataset_gender_games_90_99_EU$code_gender)

#calculamos la varianza
var_eu_90_99 = var(dataset_gender_games_90_99_EU$code_gender)

#calculamos la desviación típica
sd_eu_90_99 = sd(dataset_gender_games_90_99_EU$code_gender)

#calculamos correlación de Pearson
cor_eu_90_99 = sd_eu_90_99 / mean_eu_90_99

hist(dataset_gender_games_90_99_EU$code_gender, main="Género Europa del 90 al 99",ylab="Frecuencia", xlab="Género")


#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1990 a 1999 por género JP
Freq <- as.data.frame(table(dataset_gender_games_90_99_JP$code_gender))
Tabla_jp_gender_90_99 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_jp_90_99 = mean(dataset_gender_games_90_99_JP$code_gender)

mode_jp_90_99 = mode(dataset_gender_games_90_99_JP$code_gender)

#calculamos la varianza
var_jp_90_99 = var(dataset_gender_games_90_99_JP$code_gender)

#calculamos la desviación típica
sd_jp_90_99 = sd(dataset_gender_games_90_99_JP$code_gender)

#calculamos correlación de Pearson
cor_jp_90_99 = sd_jp_90_99 / mean_jp_90_99

hist(dataset_gender_games_90_99_JP$code_gender, main="Género Japón del 90 al 99",ylab="Frecuencia", xlab="Género")


#-------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 1990 a 1999 por género Others
Freq <- as.data.frame(table(dataset_gender_games_90_99_Others$code_gender))
Tabla_ot_gender_90_99 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_ot_90_99 = mean(dataset_gender_games_90_99_Others$code_gender)

mode_ot_90_99 = mode(dataset_gender_games_90_99_Others$code_gender)

#calculamos la varianza
var_ot_90_99 = var(dataset_gender_games_90_99_Others$code_gender)

#calculamos la desviación típica
sd_ot_90_99 = sd(dataset_gender_games_90_99_Others$code_gender)

#calculamos correlación de Pearson
cor_ot_90_99 = sd_ot_90_99 / mean_ot_90_99


hist(dataset_gender_games_90_99_Others$code_gender, main="Género Japón del 90 al 99",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 2000 a 2009 por género NA

Freq <- as.data.frame(table(dataset_gender_games_00_09_NA$code_gender))
Tabla_na_gender_00_09 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_na_00_09 = mean(dataset_gender_games_00_09_NA$code_gender)

mode_na_00_09 = mode(dataset_gender_games_00_09_NA$code_gender)

#calculamos la varianza
var_na_00_09 = var(dataset_gender_games_00_09_NA$code_gender)

#calculamos la desviación típica
sd_na_00_09 = sd(dataset_gender_games_00_09_NA$code_gender)

#calculamos correlación de Pearson
cor_na_00_09 = sd_na_00_09 / mean_na_00_09


hist(dataset_gender_games_00_09_NA$code_gender, main="Género Norte America del 2000 al 2009",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 2000 a 2009 por género EU

Freq <- as.data.frame(table(dataset_gender_games_00_09_EU$code_gender))
Tabla_eu_gender_00_09 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_eu_00_09 = mean(dataset_gender_games_00_09_EU$code_gender)

mode_eu_00_09 = mode(dataset_gender_games_00_09_EU$code_gender)

#calculamos la varianza
var_eu_00_09 = var(dataset_gender_games_00_09_EU$code_gender)

#calculamos la desviación típica
sd_eu_00_09 = sd(dataset_gender_games_00_09_EU$code_gender)

#calculamos correlación de Pearson
cor_eu_00_09 = sd_eu_00_09 / mean_eu_00_09

hist(dataset_gender_games_00_09_EU$code_gender, main="Género Europa del 2000 al 2009",ylab="Frecuencia", xlab="Género")

#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 2000 a 2009 por género JP

Freq <- as.data.frame(table(dataset_gender_games_00_09_JP$code_gender))
Tabla_jp_gender_00_09 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_jp_00_09 = mean(dataset_gender_games_00_09_JP$code_gender)

mode_jp_00_09 = mode(dataset_gender_games_00_09_JP$code_gender)

#calculamos la varianza
var_jp_00_09 = var(dataset_gender_games_00_09_JP$code_gender)

#calculamos la desviación típica
sd_jp_00_09 = sd(dataset_gender_games_00_09_JP$code_gender)

#calculamos correlación de Pearson
cor_jp_00_09 = sd_jp_00_09 / mean_jp_00_09

hist(dataset_gender_games_00_09_JP$code_gender, main="Género Japón del 2000 al 2009",ylab="Frecuencia", xlab="Género")


#------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 2000 a 2009 por género Others

Freq <- as.data.frame(table(dataset_gender_games_00_09_Others$code_gender))
Tabla_ot_gender_00_09 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_ot_00_09 = mean(dataset_gender_games_00_09_Others$code_gender)

mode_ot_00_09 = mode(dataset_gender_games_00_09_Others$code_gender)

#calculamos la varianza
var_ot_00_09 = var(dataset_gender_games_00_09_Others$code_gender)

#calculamos la desviación típica
sd_ot_00_09 = sd(dataset_gender_games_00_09_Others$code_gender)

#calculamos correlación de Pearson
cor_ot_00_09 = sd_ot_00_09 / mean_ot_00_09

hist(dataset_gender_games_00_09_Others$code_gender, main="Género otros paises del 2000 al 2009",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
rm(Freq)

#Tabla de frecuencias para 2010 a 2016 por género NA

Freq <- as.data.frame(table(dataset_gender_games_10_16_NA$code_gender))
Tabla_na_gender_10_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

#calculamos la media y la moda 
mean_na_10_16 = mean(dataset_gender_games_10_16_NA$code_gender)

mode_na_10_16 = mode(dataset_gender_games_10_16_NA$code_gender)

#calculamos la varianza
var_na_10_16 = var(dataset_gender_games_10_16_NA$code_gender)

#calculamos la desviación típica
sd_na_10_16 = sd(dataset_gender_games_10_16_NA$code_gender)

#calculamos correlación de Pearson
cor_na_10_16 = sd_na_10_16 / mean_na_10_16

hist(dataset_gender_games_10_16_NA$code_gender, main="Género Norte America del 2010 al 2016",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
rm(Freq)
#Tabla de frecuencias para 2010 a 2016 por género EU
Freq <- as.data.frame(table(dataset_gender_games_10_16_EU$code_gender))
Tabla_eu_gender_10_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

mean_eu_10_16 = mean(dataset_gender_games_10_16_EU$code_gender)

mode_eu_10_16 = mode(dataset_gender_games_10_16_EU$code_gender)

#calculamos la varianza
var_eu_10_16 = var(dataset_gender_games_10_16_EU$code_gender)

#calculamos la desviación típica
sd_eu_10_16 = sd(dataset_gender_games_10_16_EU$code_gender)

#calculamos correlación de Pearson
cor_eu_10_16 = sd_eu_10_16 / mean_eu_10_16

hist(dataset_gender_games_10_16_EU$code_gender, main="Género Europa del 2010 al 2016",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
rm(Freq)
#Tabla de frecuencias para 2010 a 2016 por género JP
Freq <- as.data.frame(table(dataset_gender_games_10_16_JP$code_gender))
Tabla_jp_gender_10_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

mean_jp_10_16 = mean(dataset_gender_games_10_16_JP$code_gender)

mode_jp_10_16 = mode(dataset_gender_games_10_16_JP$code_gender)

#calculamos la varianza
var_jp_10_16 = var(dataset_gender_games_10_16_JP$code_gender)

#calculamos la desviación típica
sd_jp_10_16 = sd(dataset_gender_games_10_16_JP$code_gender)

#calculamos correlación de Pearson
cor_jp_10_16 = sd_jp_10_16 / mean_jp_10_16

hist(dataset_gender_games_10_16_JP$code_gender, main="Género Japón del 2010 al 2016",ylab="Frecuencia", xlab="Género")

#-------------------------------------------------------------------------------
rm(Freq)
#Tabla de frecuencias para 2010 a 2016 por género Others

Freq <- as.data.frame(table(dataset_gender_games_10_16_Others$code_gender))
Tabla_ot_gender_10_16 <- transform(Freq, CumFreq = cumsum(Freq), Rel =
                                     prop.table(Freq), CumRel=cumsum(prop.table(Freq)))

mean_ot_10_16 = mean(dataset_gender_games_10_16_Others$code_gender)

mode_ot_10_16 = mode(dataset_gender_games_10_16_Others$code_gender)

#calculamos la varianza
var_ot_10_16 = var(dataset_gender_games_10_16_Others$code_gender)

#calculamos la desviación típica
sd_ot_10_16 = sd(dataset_gender_games_10_16_Others$code_gender)

#calculamos correlación de Pearson
cor_ot_10_16 = sd_ot_10_16 / mean_ot_10_16

hist(dataset_gender_games_10_16_Others$code_gender, main="Género Otros paises 2010 al 2016",ylab="Frecuencia", xlab="Género")




