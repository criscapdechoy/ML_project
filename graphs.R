
ggplot_screeplot <- function(eigenvalues, kaiser.rule.lim) {
  p <- length(eigenvalues)
  ggplot() + geom_hline(yintercept=kaiser.rule.lim, col="red") +
    geom_point(aes(1:p,eigenvalues), col="blue", size=2.8)  +
    geom_line(aes(1:p,eigenvalues), col="blue", size=0.8) + customtheme(tsize=12) + 
    ggtitle("Scree Plot") + xlab("Component Number") + ylab("Eigenvalue")
}

ggplot_ind_fm <- function(eigenvalues, proj.ind, colind, indnames) {
  d1.expl <- eigenvalues[1]/sum(eigenvalues)*100
  d2.expl <- eigenvalues[2]/sum(eigenvalues)*100
  xlab <- xlab(paste0("Dim 1 (", round(d1.expl, digits=2), "%)"))
  ylab <- ylab(paste0("Dim 2 (", round(d2.expl, digits=2), "%)"))
  ggplot() + geom_factormap_theme(xintercept=0, yintercept=0) + customtheme() +
    geom_point(aes(x=proj.ind[,1], y=proj.ind[,2], col=factor(data$demo)), size=2) +
    geom_text(aes(x=proj.ind[,1], y=proj.ind[,2], label=indnames), hjust=0, vjust=-0.5) +
    ggtitle("Individuals factor map (PCA)") + labs(colour="Demo") + xlab + ylab
}

ggplot_var_fm <- function(eigenvalues, proj.var, varnames) {
  d1.expl <- eigenvalues[1]/sum(eigenvalues)*100
  d2.expl <- eigenvalues[2]/sum(eigenvalues)*100
  xlab <- xlab(paste0("Dim 1 (", round(d1.expl, digits=2), "%)"))
  ylab <- ylab(paste0("Dim 2 (", round(d2.expl, digits=2), "%)"))
  ggplot() + geom_factormap_theme(xintercept=0, yintercept=0, circle=T) + customtheme() +
    geom_segment(aes(x=0, y=0, xend=proj.var[,1], yend=proj.var[,2]), arrow=arrow(length=unit(0.3,"cm"))) +
    geom_text(aes(x=proj.var[,1], y=proj.var[,2], label=varnames), hjust=0.5, vjust=-0.5) +
    ggtitle("Variables factor map (PCA)") + xlab + ylab + coord_fixed(ratio=1)
}

customtheme <- function(tsize=12) {
  return (theme_bw() + 
            theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=tsize), axis.text.y=element_text(size=tsize),
                  axis.title.x=element_text(size=tsize), axis.title.y=element_text(size=tsize),
                  plot.title=element_text(size=tsize+2, face="bold", hjust=0.5),
                  legend.text=element_text(size=tsize)) +
            theme(legend.position="bottom"))
}

geom_circle <- function(center=c(0,0), rad=1, npoints=100){
  c <- annotate("path",
                x=c(0)+rad*cos(seq(0,2*pi,length.out=100)),
                y=c(0)+rad*sin(seq(0,2*pi,length.out=100)))
  return(c)
}

geom_factormap_theme <- function(xintercept, yintercept, circle=FALSE) {
  hline <- geom_hline(yintercept=0, linetype="dashed")
  vline <- geom_vline(xintercept=0, linetype="dashed")
  if (circle) {
    circle <- geom_circle(center=c(0,0), rad=1, npoints=100)
    return(c(hline, vline, circle))
  } else {
    return(c(hline, vline))
  }
}

