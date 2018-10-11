barplot2 <- function(x, y, fun.lines=sd, ylab="meu_eixo_y_lindo",
xlab="meu_eixo_x_lindo", main="meu_gráfico_lindo") {
    means <- aggregate(x~y, FUN=mean)
    vars <- aggregate(x~y, FUN=fun.lines)
    foo <- as.numeric(barplot(means$x, ylim=c(0,max(means[,2]+vars[,2])), xaxt="n", ylab=ylab, xlab=xlab, main=main))
    axis(side=1, at=foo, lab=means[,1], lwd=0)
    abline(h=0)
    arrows(x0=foo, x1=foo, y0=means[,2], y1=means[,2]+vars[,2], angle=90)
    arrows(x0=foo, x1=foo, y0=means[,2], y1=means[,2]-vars[,2], angle=90)
}

a <- runif(50)
b <- rep(c("a","b","c"), length.out=50)
barplot2(x=a, y=b, fun.lines=sd, ylab="coisa aleatória", xlab="grupo
aleatório", main="gráfico aleatório")

