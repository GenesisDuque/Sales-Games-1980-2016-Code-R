ggplot(Nde_videojuegos_por_genero_by_Publicador_and_Genero, aes(x = Nde_videojuegos_por_genero_by_Publicador_and_Genero$`ï»¿Publicador`, y = Nde_videojuegos_por_genero_by_Publicador_and_Genero$`NÂº de videojuegos por gÃ©nero`, fill=Nde_videojuegos_por_genero_by_Publicador_and_Genero$`GÃ©nero`)) + 
  geom_bar(stat = "identity")

