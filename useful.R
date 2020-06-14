impute_NAs <- function(data) {
    data <- knnImputation(data, k=10, scale=T, meth="weighAvg")
    return(data)
}

standardize_matrix <- function(X) {
    n <- nrow(X); p <- ncol(X)
    N <- diag(x=1/n, nrow=n, ncol=n)
    G <- apply(N %*% X, 2, function(var) sum(var))
    Xcent <- sweep(X,2,G)
    s <- apply(X, 2, function(var) sd(var))
    S <- diag(x=1/s, nrow=p, ncol=p)
    Xstan <- Xcent %*% S
    
    colnames(Xstan) <- colnames(X)
    
    return (Xstan)
}

ggplot_biplot <- function(Psi, Phi, eigenvalues) {
  d1.expl <- eigenvalues[1]/sum(eigenvalues)*100
  d2.expl <- eigenvalues[2]/sum(eigenvalues)*100
  xlab <- xlab(paste0("Dim 1 (", round(d1.expl, digits=2), "%)"))
  ylab <- ylab(paste0("Dim 2 (", round(d2.expl, digits=2), "%)"))
  
  par(mar=c(4.2,4.2,2,2))
  biplot(Psi, Phi, xlab=xlab, ylab=ylab)
}

ggplot_variables <- function(Phi, eigenvalues, varnames) {
  d1.expl <- eigenvalues[1]/sum(eigenvalues)*100
  d2.expl <- eigenvalues[2]/sum(eigenvalues)*100
  xlab <- xlab(paste0("Dim 1 (", round(d1.expl, digits=2), "%)"))
  ylab <- ylab(paste0("Dim 2 (", round(d2.expl, digits=2), "%)"))
  
  ggplot() + geom_factormap_theme(xintercept=0, yintercept=0, circle=T) + customtheme() +
    geom_segment(aes(x=0, y=0, xend=Phi[,1], yend=Phi[,2]), arrow=arrow(length=unit(0.3,"cm"))) +
    geom_text(aes(x=Phi[,1], y=Phi[,2], label=varnames), hjust=0.5, vjust=-0.5) +
    ggtitle("Variables factor map (PCA)") + xlab + ylab + coord_fixed(ratio=1)
}

ggplot_individuals <- function(Psi, eigenvalues, indnames, color.factor=NULL) {
    d1.expl <- eigenvalues[1]/sum(eigenvalues)*100
    d2.expl <- eigenvalues[2]/sum(eigenvalues)*100
    xlab <- xlab(paste0("Dim 1 (", round(d1.expl, digits=2), "%)"))
    ylab <- ylab(paste0("Dim 2 (", round(d2.expl, digits=2), "%)"))
    
    g <- ggplot() + geom_factormap_theme(xintercept=0, yintercept=0) + customtheme() +
        geom_text(aes(x=Psi[,1], y=Psi[,2], label=indnames), hjust=0, vjust=-0.5) +
        ggtitle("Individuals factor map (PCA)") + xlab + ylab
    
    if (is.null(color.factor)) g <- g + geom_point(aes(x=Psi[,1], y=Psi[,2]), size=2)
    else g <- g + geom_point(aes(x=Psi[,1], y=Psi[,2], col=color.factor), size=2) + labs(colour="Demo")
    
    return(g)
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