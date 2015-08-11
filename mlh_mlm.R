mlh.mlm <- 
  function(object, L = null, T = diag(nrow = p), 
           test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")) 
  { 
    # test the multivariate linear hypothesis LBT'=0 
    # B = matrix of regression coefficients 
    # L = matrix, each row is a linear combination of the parameters 
    # T = transformation matrix 
    
    if(!inherits(object, "mlm")) 
      stop("object must be of class \"mlm\"") 
    
    if( is.null(L) ) 
      stop("L matrix is not specified.") 
    
    # L must be a matrix 
    if( is.null(dim(L)) ) 
      L <- t(L) 
    
    if( nrow(object$coef) != ncol(L) ) 
      stop("nrow(object$coef) != ncol(L)") 
    
    p <- ncol(SSD(object)$SSD) 
    ssd <- SSD(object) 
    df.res <- ssd$df 
    rss.qr <- qr(T %*% ssd$SSD %*% t(T)) 
    
    X <- as.matrix( model.matrix(object) ) 
    B <- as.matrix( object$coef ) 
    
    df <- nrow(L) 
    ss <- t(L %*% B) %*% 
      as.matrix(solve(L %*% solve(t(X) %*% X) %*% t(L))) %*% 
      (L %*% B) 
    
    eigs <- Re(eigen(qr.coef(rss.qr, 
                             T %*% ss %*% t(T)), 
                     symmetric = FALSE)$values) 
    
    test <- match.arg(test) 
    stats <- switch(test, 
                    "Pillai" = Pillai(eigs, df, df.res), 
                    "Wilks"  =  Wilks(eigs, df, df.res), 
                    "Hotelling-Lawley" = HL(eigs, df, df.res), 
                    "Roy"    = Roy(eigs, df, df.res) 
    ) 
    stats[5] <- pf(stats[2], stats[3], stats[4], lower.tail = FALSE) 
    names(stats) <- c(test, "approx F", "num Df", "den Df", "Pr(>F)") 
    
    stats 
  } 
