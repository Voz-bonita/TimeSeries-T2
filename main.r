if (!require("pacman")) {
    install.packages("pacman")
}
pacman::p_load("forecast", "Mcomp", "tseries", "ggplot2", "ggpubr")
source("auxiliar_functions.r", encoding = "UTF-8")

LAG_MAP <- c("MONTHLY" = 12)

ids <- c(2119, 2539)
id_escolhido <- ids[2]
serie_escolhida <- M3[[id_escolhido]]
serie_escolhida %>% plot()
treinamento <- serie_escolhida$x
teste <- serie_escolhida$xx
lag <- LAG_MAP[[serie_escolhida$period]]
horizonte <- serie_escolhida$h

# Item a)
mstl(treinamento) %>% plot()

# Item b)
kpss.test(treinamento, null = "Trend")
out_diff_trein <- treinamento %>% diff_series(lag = lag)
c(out_diff_trein$diff_simples, out_diff_trein$diff_sasonal)
treinamento_estacionario <- out_diff_trein$ts

treinamento_estacionario %>%
    autoplot() +
    geom_line(linewidth = 1) +
    theme_bw()

par(mfrow = c(1, 2))
acf(treinamento_estacionario)
pacf(treinamento_estacionario)

box_cox(treinamento,
    lambda = seq(-3, 3, 1)[-5],
    path_to_plot = "assets/box_cox_escolha.png"
)
treinamento_transformado <- box_cox__(treinamento, lambda = 0)
treinamento_transformado %>% kpss.test(treinamento, null = "Trend")
out_diff_trein_trans <- treinamento_transformado %>% diff_series(lag = lag)
c(out_diff_trein_trans$diff_simples, out_diff_trein_trans$diff_sasonal)
trein_trans_estac <- out_diff_trein_trans$ts

par(mfrow = c(1, 2))
acf(trein_trans_estac)
pacf(trein_trans_estac)

# A FAZER: Escolher os modelos baseados nos gráficos ou rodar loops para valores pequenos de p, q, P, Q

# Item c)
ets(treinamento, model = "AAA")
ets(treinamento, model = "MAA")
ets(treinamento, model = "MAM")
ets_mod <- ets(treinamento, model = "MMM")
ets_mod_boxcox <- ets(treinamento, model = "AAA", lambda = 0) # AAA é o unico modelo possível por causa do lambda e modelos instaveis

# Item d)

residuals_analysis(ets_mod, "assets/ets_mod.png") %>% format_tab("", digits = 3, "latex")
residuals_analysis(ets_mod_boxcox, "assets/ets_mod_boxcox.png") %>% format_tab("", digits = 3, "latex")
# A FAZER: Incluir os modelos ARIMA

# Item e)

# item f)

p3 <- forecast(ets_mod, h = horizonte, level = 95) %>%
    prediction_plot()
p4 <- forecast(ets_mod_boxcox, h = horizonte, level = 95) %>%
    prediction_plot()

ggarrange(p3, p4) %>%
    ggsave(filename = "assets/predictions.png", .)

# item g)
models <- c(
    "auto.arima" = auto.arima,
    # ses, holt,
    "ets" = ets, "stlf" = stlf,
    "bats" = bats, "tbats" = tbats
)
MAEs <- map_dbl(models, ~ MAE(model = .x, train = treinamento, test = teste, h = horizonte))
data.frame("Implementação" = names(MAEs), "MAE" = MAEs) %>%
    format_tab("", digits = 4, format = "latex")