#######
ggplot(df, aes(x = Plot, y = X_mean, colour = Group, group = Plot)) +
  geom_boxplot()
ggplot(df, aes(x = Group, y = X_mean, colour = Group)) +
  geom_boxplot()
