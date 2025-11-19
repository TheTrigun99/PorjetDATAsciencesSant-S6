library(RGCCA)
library(gliomaData)
fit_sg <- readRDS('modelrgcca.rds')

library(ggplot2)
top20 <- head(coef_ordered(fit_dipg, s = "lambda.1se")[, c("view_col", "coef")], 20)
top20$couleur <- ifelse(top20$view_col == 'A_23_P205428', "rouge", "bleu")
ggplot(top20, aes(x = reorder(view_col, coef), y = coef)) +
  geom_col(fill = "steelblue") +
  coord_flip() +scale_fill_manual(values = c("rouge" = "red", "bleu" = "steelblue"))+
  labs(title = "Top 20 variables pour prédire 'dipg'",
       x = "Variable", y = "Coefficient") +
  theme_minimal()+theme(legend.position = "none")
block2_df <- subset(boot_out$stats, block == "GE")
block2_sorted_abs <- block2_df[order(-abs(block2_df$mean)), ]
top20_block2 <- head(block2_sorted_abs[,c("mean", "var")], 20)
library(ggplot2)

# 1. Garder les 20 premières lignes et colonnes utiles
top20 <- block2_sorted_abs[, c("mean", "var")]
print(top20)
top20$couleur <- ifelse(top20$var == 'A_23_P205428', "rouge", "bleu")
