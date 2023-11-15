# Make data frame with data (2x2 design)
data <- data.frame(
  "Klippa" = c(7, 0),
  "Vegetation" = c(2, 5),
  row.names = c("Kamoflagoue", "Vit"),
  stringsAsFactors = FALSE
)

fisher.test(data)
chisq.test(data)


# White eggs and camouflagued eggs
Klippa <- c(5,13)
Vegetation <- c(6,15)
matris <- rbind(Klippa, Vegetation)
names <- c("Klippa", "Vegetation", "Klippa", "Vegetation")
colnames(matris) <- c("Vit", "Kamoflagoue")
medel <- matris

p1 <- barplot(medel, ylim=c(0,15), 
            beside=T, col=c("white","grey"), names.arg = names, xlab = "Habitat", ylab = "Prederade ägg")
arrows(p1, medel, y1=c(medel-sem,
                       medel+sem), angle=90)

chisq.test(matris)







