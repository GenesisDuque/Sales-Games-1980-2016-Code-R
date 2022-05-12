#Estadistica bidimensiona 1980 a 2016
#varible xi=cantidad de juegos de cada genero
#varible yi=ventas de unidades de juegos por cada genero

# Norte America
x1_na_quantity <- Analisis_de_genero_VideoGames_Sales_1980_2016$`Cantidad de juegos NA` 
y1_na_sales <- Analisis_de_genero_VideoGames_Sales_1980_2016$NA_sales

x1_y1_na <- data.frame("Quantity" = x1_na_quantity, "Sales"=y1_na_sales)

covarianza_na <- cov(x1_na_quantity,y1_na_sales)
correlacion_na <- cor(x1_na_quantity,y1_na_sales)
reg_lin_na <- lm(y1_na_sales~x1_na_quantity)
summary(reg_lin_na)

plot(x1_na_quantity,y1_na_sales, xlab = "Cantidad por género", ylab = "Unidades vendidas (M)", col="blue", pch=19)
abline(reg_lin_na)

# Europa 

x2_eu_quantity <-Analisis_de_genero_VideoGames_Sales_1980_2016$`Cantidad de juego EU`
y2_eu_sales <-Analisis_de_genero_VideoGames_Sales_1980_2016$EU_sales

x2_y2_eu <- data.frame("Quantity" = x2_eu_quantity, "Sales"=y2_eu_sales)

covarianza_eu <- cov(x2_eu_quantity,y2_eu_sales)
correlacion_eu <- cor(x2_eu_quantity,y2_eu_sales)
reg_lin_eu <- lm(y2_eu_sales~x2_eu_quantity)
summary(reg_lin_eu)

plot(x2_eu_quantity,y2_eu_sales, xlab = "Cantidad por género", ylab = "Unidades vendidas (M)", col="blue", pch=19)
abline(reg_lin_eu)

#Japón

x3_jp_quantity <-Analisis_de_genero_VideoGames_Sales_1980_2016$`Cantidad de juego JP`
y3_jp_sales <-Analisis_de_genero_VideoGames_Sales_1980_2016$JP_sales

x3_y3_jp <- data.frame("Quantity" = x3_jp_quantity, "Sales"=y3_jp_sales, col="black")


covarianza_jp <- cov(x3_jp_quantity,y3_jp_sales)
correlacion_jp <- cor(x3_jp_quantity,y3_jp_sales)
reg_lin_jp <- lm(y3_jp_sales~x3_jp_quantity)
summary(reg_lin_jp)

plot(x3_jp_quantity,y3_jp_sales,xlab = "Cantidad por género", ylab = "Unidades vendidas (M)", col="blue", pch=19 )
abline(reg_lin_jp)

#Others

x4_ot_quantity <-Analisis_de_genero_VideoGames_Sales_1980_2016$`Cantidad de juego Others`
y4_ot_sales <-Analisis_de_genero_VideoGames_Sales_1980_2016$Others_sales

x4_y4_ot <- data.frame("Quantity" = x4_ot_quantity, "Sales"=y4_ot_sales)

covarianza_ot <- cov(x4_ot_quantity,y4_ot_sales)
correlacion_ot <- cor(x4_ot_quantity,y4_ot_sales)
reg_lin_ot <- lm(y4_ot_sales~x4_ot_quantity)
summary(reg_lin_ot)

plot(x4_ot_quantity,y4_ot_sales, xlab = "Cantidad por género", ylab = "Unidades vendidas (M)", col="blue", pch=19 )
abline(reg_lin_ot)


################################################
###############################################

#Predición unidades de copias vendidas para just dance Wii 

x_year_just_dance <- just_dance$Year_of_Release
y_sales_just_dance <- just_dance$Global_Sales

just_dance_xy <- data.frame("Quantity" = x_year_just_dance, "Sales"=y_sales_just_dance)

covarianza_just_dance <- cov(x_year_just_dance,y_sales_just_dance)
correlacion_just_dance <- cor(x_year_just_dance,y_sales_just_dance)
reg_lin_just_dance <- lm(y_sales_just_dance~x_year_just_dance)
summary(reg_lin_just_dance)

plot(x_year_just_dance,y_sales_just_dance, xlab = "Cantidad por género", ylab = "Unidades vendidas (M)", col="blue", pch=19 )
abline(reg_lin_just_dance)


var_just_x <- var(x_year_just_dance)
var_just_y <- var(y_sales_just_dance)

#resultado : La ecuación de regresión es negativa y por tanto representa un desencso de las ventas
