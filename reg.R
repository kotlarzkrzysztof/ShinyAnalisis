# regLin ------------------------------------------------------------------
# Dodano możliwość wprowadzenia dowolnej ilości zmiennych niezależnych
# Dodano output w postaci tabeli
# Dodano podstawowe statystyki dla reszt
# Dodano wskaźnik istotności w postaci '*'
# Dodano reszty studentyzowane


regLin = function(y, x, names, rstu) {
  stars <- function(p_wartosc) {
    s_vec <- c()
    for (i in 1:length(p_wartosc)) {
      if (p_wartosc[i] < 0.001)
        s_vec[i] <- '***'
      else if (p_wartosc[i] < 0.01)
        s_vec[i] <- '**'
      else if (p_wartosc[i] < 0.05)
        s_vec[i] <- '*'
      else if (p_wartosc[i] < 0.1)
        s_vec[i] <- '.'
      else
        s_vec[i] <- ' '
    }
    
    return(s_vec)
  }
  
  parameters <- function(matx, n, k, names) {
    alpha <- solve(t(matx) %*% matx) %*% t(matx) %*% y
    y2 <- matx %*% alpha
    residua <- y - y2
    rstud <- residua / sd(residua)
    df <- (n - k - 1)
    S2e <- sum(residua ^ 2) / df
    D <- S2e * solve(t(matx) %*% matx)
    Salpha <- sqrt(diag(D))
    testT <- alpha / Salpha
    p_wartosc <- 2 * pt(abs(testT), df, lower.tail = FALSE)
    R2 <- 1 - sum(residua ^ 2) / sum((y - mean(y)) ^ 2)
    F <- (R2 / (1 - R2)) * df / (k)
    p_wartoscF <- pf(F, k, df, lower.tail = FALSE)
    stars <- stars(p_wartosc)
    
    return(
      list(
        Estimate = alpha,
        Std.Error = Salpha,
        t.value = testT,
        Pr = p_wartosc,
        Signf. = stars,
        R2 = R2,
        F_pval = p_wartoscF,
        res = residua,
        t_res = rstud,
        S2e = S2e,
        df = df,
        F_stat = F
        
      )
    )
  }
  
  createTable <- function(output, names, k, rstu) {
    table <- data.frame(out[1:5])
    row.names(table) <- c('(Intercept)', names)
    cat('\nResiduals:\n', summary(out$res))
    cat('\n\nCoefficients:\n')
    print(format(table, digits = 4, justify = 'left'))
    cat('---\n')
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n\n")
    cat(sprintf(
      "Residual standard error: %.4f on %d degrees of freedom\n",
      sqrt(out$S2e),
      out$df
    ))
    cat(sprintf('Multiple R-squared: %.4f\n', out$R2))
    cat(
      sprintf(
        'F-statistic: %.4f on %d and %d degrees of freedom, p-value: %.4e\n',
        out$F_stat,
        k,
        out$df,
        out$F_pval
      )
    )
    if (rstu == TRUE){
      cat('--- rstud ---\n')
      print(as.data.frame(out[9]))
    }
  }
  
  n <- length(y)
  matx <- matrix(1, n)
  matx <- as.matrix(cbind(matx, x))
  k <- dim(matx)[2] - 1
  out <- parameters(matx, n , k)
  createTable(out, names, k, rstu)
  
  invisible(list(out))
}
