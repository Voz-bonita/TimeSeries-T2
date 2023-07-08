library('pacman')

pacman::p_load(tidyr,dplyr,stringr,Mcomp,forecast,ggpubr,tseries)

###########################################################################################

data(M3)

id=2539 # id da série escolhida

horizonte <- M3[[id]]$h

serie_temp <- M3[[id]]$x

out_sample <- M3[[id]]$xx

M3[[id]]$description # descrição da série

############### Séries Arima ###############

Arima_p = Arima(serie_temp, order=c(1,1,3), seasonal=c(1,0,1))

Arima_box_cox = Arima(serie_temp, order=c(1,1,2), seasonal=c(0,0,2),
                      lambda = "auto")

############### Séries ETS ###############

ets_p <- ets(serie_temp, model = "MAM")

ets_boxcox <- ets(serie_temp, model = "AAA", lambda = -.24)


#######

## Estudo de desempenho preditivo por janela deslizante;

#######

# Tamanho da série

n = length(serie_temp)

########################################################

### Funções para a função tsCV

f_arima = function(x,h){
  
  forecast(Arima(x,order = c(1,1,3), seasonal=c(1,0,1)),h=h)
}

cv_arima = tsCV(serie_temp,f_arima, h=5,initial=n-14)

##############

f_arima_box_cox = function(x,h){
  
  forecast(Arima(x,order=c(1,1,2), seasonal=c(0,0,2), lambda = 'auto'),h=h)
  
}

cv_arima_box_cox = tsCV(serie_temp,f_arima_box_cox, h=5,initial=n-14)

##############

f_ets = function(x,h){
  
  forecast(ets(serie_temp, model = "MAM"),h=h)
  
}

cv_ets = tsCV(serie_temp,f_ets, h=5,initial=n-14)


##############

f_ets_box_cox = function(x,h){
  
  forecast(ets(serie_temp, model = "AAA", lambda = -.24),h=h)
  
}

cv_ets_box_cox = tsCV(serie_temp,f_ets_box_cox, h=5,initial=n-14)

##############

## Cálculo do erro absoluto médio para cada horizonte de previsão

##############

mae_arima = cv_arima %>% abs() %>% colMeans(na.rm=T)

mae_arima_box_cox = cv_arima_box_cox %>% abs() %>% colMeans(na.rm=T)

mae_ets = cv_ets %>% abs() %>% colMeans(na.rm=T)

mae_ets_box_cox = cv_ets_box_cox %>% abs() %>% colMeans(na.rm=T)

tabela_p = cbind(as.numeric(mae_arima), as.numeric(mae_ets))

tabela_box = cbind(mae_arima_box_cox, mae_ets_box_cox)

tabela_mae = data.frame(mae_arima, mae_ets, mae_arima_box_cox, mae_ets_box_cox)

knitr::kable(tabela_mae)


### ERRO ABSOLUTO MÉDIO - MAE

colnames(tabela_mae) <- c('ARIMA', 'ETS',
                            'ARIMA Transformada', 'ETS Transformada')


##### Gráficos de linhas

tf_tidy_dt = function(dt){
  
  colnames(dt) = c('Arima','ETS')
  
  data = pivot_longer(as.data.frame(dt),cols = c('Arima','ETS')) %>% mutate(rep(1:5,each = 2))
  
  colnames(data) = c('Modelo','MAE','ID')
  
  return(data)
}

tabela_p = tf_tidy_dt(tabela_p) 

tabela_box = tf_tidy_dt(tabela_box) #mudar nome



ggplot(tabela_p, aes (x = ID,y = MAE, group = Modelo, colour = Modelo))+
  geom_line(size = 1) +geom_point(size = 2)+
  labs(x = 'h', y = 'MAE')+
  theme_bw()+
  theme(axis.title.y =element_text(colour='black' , size=14),
        axis.title.x =element_text(colour='black' , size=14),
        axis.text =element_text(colour='black',size=13.5),
        panel.border = element_blank(),
        plot.title = element_text(size = 24, face = "bold"),
        axis.line =element_line(colour='black'))+
  ggtitle('Erros de previsão para cada horizonte dos modelos sem tranformação Box cox')+
  scale_colour_manual(values = c("#009999",'#B2182B'))


ggplot(tabela_box, aes (x = ID,y = MAE, group = Modelo, colour = Modelo))+
  geom_line(size = 1) +geom_point(size = 2)+
  labs(x = 'h', y = 'MAE')+
  theme_bw()+
  theme(axis.title.y =element_text(colour='black' , size=14),
        axis.title.x =element_text(colour='black' , size=14),
        axis.text =element_text(colour='black',size=13.5),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'),
        plot.title = element_text(size = 24, face = "bold"))+
  ggtitle('Erros de previsão para cada horizonte dos modelos transformados')+
  scale_colour_manual(values = c("#009999",'#B2182B'))
