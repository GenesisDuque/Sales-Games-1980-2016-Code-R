#Gráficos - Pórcion de videos juegos 1980-2016


#conversión a porcentaje
Porcentaje_gender = round(count_gender*100 / 16448)


#gráfico por porcentaje
pie(Porcentaje_gender, labels = paste0(Porcentaje_gender,"%"), col =brewer.pal(n=12, name="Paired"))
legend("bottomleft",legend=type_gender, fill = brewer.pal(n=12, name="Paired"))

#Sales NA

sales_na = Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales

#barplot
barplot(count_gender, col = brewer.pal(n=12, name="Paired"), names.arg =type_gender, main="Gráfico Videosjuegos por género 1980-2016")


#linea
x = 1:12
plot(x, Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales, type="l", main="Número de ventas según el género 1980-2016", ylab= "Unidades vendidas (M)", xlab = "Género")
lines(x, Analisis_de_genero_VideoGames_Sales_1980_2016$EU_sales, col=2, type = "l")
lines(x, Analisis_de_genero_VideoGames_Sales_1980_2016$JA_sales, col=3, type="l")
lines(x,Analisis_de_genero_VideoGames_Sales_1980_2016$Others_sales, col=4, type="l")
legend("bottomleft", legend = type_gender)