sgis = read.csv('sgis.csv', header = T)
population = read.csv('population.csv', header = T)

regPop = lm(ratePercent[2:6] ~ seq(1:5), data = population)
summary(regPop)
plot(regPop)

rat.inc = data.frame(name = sgis$지역, ratio = sgis$여성.1인가구 / sgis$인구, incre = sgis$증가율)
rat.inc$ratio = scale(rat.inc$ratio)
rat.inc$incre = scale(rat.inc$incre)

dist_rat = dist(rat.inc[2:3], method = "euclidean")
hc_rat = hclust(dist_rat, method = "ward.D")
plot(hc_rat)

tot_withinss = c()
for (i in 2 : nrow(rat.inc) - 1) {
  set.seed(322)
  ratio_kmean = kmeans(rat.inc[2:3], centers = i, iter.max = 1000)
  tot_withinss[i] <- ratio_kmean$tot.withinss
}
plot(c(2 : nrow(rat.inc) - 1), tot_withinss, type = "b")

r2 = c()
for (i in 2 : nrow(rat.inc) - 1) {
  set.seed(322)
  ratio_kmean = kmeans(rat.inc[2:3], centers = i, iter.max = 1000)
  r2[i] <- ratio_kmean$betweenss / ratio_kmean$totss
}
plot(c(2 : nrow(rat.inc) - 1), r2, type = "b")

ratio_kmean = kmeans(rat.inc[2:3], 5, nstart = 100)
table(ratio_kmean$cluster)
clus = as.factor(ratio_kmean$cluster)
plot(rat.inc$ratio, rat.inc$incre)
points(ratio_kmean$centers[, c("ratio", "incre")], col = 1:5, pch = 8, cex = 4)


ratio = sgis$여성.1인가구 / sgis$인구


c0 = data.frame(ratio = ratio)
c0[, "cult"] = 1 / sgis$인구per문화시설 * sgis$인구 / sgis$여성.1인가구
c0[, "liba"] = sgis$도서관 / sgis$여성.1인가구
c0[, "secu"] = sgis$치안시설 / sgis$여성.1인가구
c0[, "home"] = sgis$노후주택.30년.이상 / sgis$여성.1인가구
c0[, "elec"] = sgis$전기차충전소 / sgis$여성.1인가구
c0[, "chic"] = sgis$치킨전문점 / sgis$여성.1인가구
c0[, "apar"] = sgis$아파트 / sgis$여성.1인가구
c0[, "mult"] = sgis$연립.다세대 / sgis$여성.1인가구
c0[, "acad"] = sgis$사설학원per1000 * 1000 / sgis$여성.1인가구

plot(c0)
cor.test(y = c0$ratio, x = c0$cult)
cor.test(y = c0$ratio, x = c0$liba)
cor.test(y = c0$ratio, x = c0$secu)
cor.test(y = c0$ratio, x = c0$home)
cor.test(y = c0$ratio, x = c0$elec)
cor.test(y = c0$ratio, x = c0$chic)
cor.test(y = c0$ratio, x = c0$apar)
cor.test(y = c0$ratio, x = c0$mult)
cor.test(y = c0$ratio, x = c0$acad)

model0 = lm(ratio ~ cult + liba + home + chic + apar, c0)
summary(model0)
step(model0, direction = "both")

model0 = lm(ratio ~ liba, c0)
summary(model0)
model0 = lm(ratio ~ chic, c0)
summary(model0)
model0 = lm(ratio ~ apar, c0)
summary(model0)


sgis1 = sgis[c(10, 11, 12), ]
c1 = data.frame(ratio = ratio[c(10, 11, 12)])
c1[, "liba"] = sgis1$도서관 / sgis1$여성.1인가구
c1[, "chic"] = sgis1$치킨전문점 / sgis1$여성.1인가구
c1[, "apar"] = sgis1$아파트 / sgis1$여성.1인가구

model1 = lm(ratio ~ liba, c1)
summary(model1)
model1 = lm(ratio ~ chic, c1)
summary(model1)
model1 = lm(ratio ~ apar, c1)
summary(model1)
model1 = lm(ratio ~ liba + chic + apar, c1)
summary(model1)

sgis2 = sgis[c(13, 17), ]
c2 = data.frame(ratio = ratio[c(13, 17)])
c2[, "liba"] = sgis2$도서관 / sgis2$여성.1인가구
c2[, "chic"] = sgis2$치킨전문점 / sgis2$여성.1인가구
c2[, "apar"] = sgis2$아파트 / sgis2$여성.1인가구

model2 = lm(ratio ~ liba, c2)
summary(model2)
model2 = lm(ratio ~ chic, c2)
summary(model2)
model2 = lm(ratio ~ apar, c2)
summary(model2)
step(model2, direction = "both")


sgis4 = sgis[c(4, 7, 14, 15), ]
c4 = data.frame(ratio = ratio[c(4, 7, 14, 15)])
c4[, "liba"] = sgis4$도서관 / sgis4$여성.1인가구
c4[, "chic"] = sgis4$치킨전문점 / sgis4$여성.1인가구
c4[, "apar"] = sgis4$아파트 / sgis4$여성.1인가구

model4 = lm(ratio ~ liba, c4)
summary(model4)
model4 = lm(ratio ~ chic, c4)
summary(model4)
model4 = lm(ratio ~ apar, c4)
summary(model4)
step(model2, direction = "both")
model4 = lm(ratio ~ liba + chic + apar, c4)
summary(model4)


sgis5 = sgis[c(1, 3, 5, 6, 8, 9, 16), ]
c5 = data.frame(ratio = ratio[c(1, 3, 5, 6, 8, 9, 16)])
c5[, "liba"] = sgis5$도서관 / sgis5$여성.1인가구
c5[, "chic"] = sgis5$치킨전문점 / sgis5$여성.1인가구
c5[, "apar"] = sgis5$아파트 / sgis5$여성.1인가구

model5 = lm(ratio ~ liba, c5)
summary(model5)
model5 = lm(ratio ~ chic, c5)
summary(model5)
model5 = lm(ratio ~ apar, c5)
summary(model5)
step(model2, direction = "both")
model5 = lm(ratio ~ liba + chic + apar, c5)
summary(model5)
step(model5, direction = "both")