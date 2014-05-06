works_with_R("3.1.0", Segmentor3IsBack="1.8")
x <- c(-sqrt(2), 0)
sd(x)
var(x)
fit <- Segmentor(x, 4L, 2L)
fit@parameters
fit@breaks
Segmentor(x, 4L, 3L)
