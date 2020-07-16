anovaHSD <- function(Y, X1, X2, names){
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
  tukey <- function(m, X, Se, N, df){
    diff = matrix(m$x, length(unique(X)),length(unique(X)))
    diff_t = t(diff)
    macierz = abs(diff - diff_t)
    colnames(macierz) <- levels(X)
    rownames(macierz) <- levels(X)
    SE = sqrt(matrix(Se, length(unique(X)))/N$x)
    SE_mat = matrix(SE, length(unique(X)),length(unique(X)))
    tukey_p = (macierz / SE_mat)
    tukey_p = round(1 - ptukey(tukey_p, length(unique(X)), df), digits = 4)
    tukey_signif = matrix(stars(tukey_p), length(unique(X)),length(unique(X)))
    colnames(tukey_signif) <- levels(X)
    rownames(tukey_signif) <- levels(X)
    
    return(list(Tukey_diff = macierz, Tukey_p = tukey_p, Tukey_signif = tukey_signif))
  }
  parameters_X1 <- function(Y, X1){
    srY = mean(Y)
    srG = aggregate(Y, by = list(X1), FUN = "mean")
    Ni = aggregate(Y, by = list(X1), FUN = "length")
    SSt = sum((Y - srY)^2)
    SSb = sum(Ni$x * (srG$x-srY)^2)
    SSw = SSt - SSb
    k1 = (length(unique(X1))-1)
    nk = (length(X1)-length(unique(X1)))
    Sb = SSb/k1
    Sw = SSw/nk
    F = Sb/Sw
    p_wartosc = pf(F,length(unique(X1))-1,
                   length(X1)-length(unique(X1)), lower.tail = FALSE)
    
    # TukeyHSD One Way
    tukey_X1 <- tukey(srG, X1, Sw, Ni, nk)
    
    return(list(Mean_Sq = c(Sb, Sw), Sum_Sq = c(SSb, SSw), F_value = F, df = c(k1, nk), p_val = p_wartosc, Tukey_diff = list(X1 = tukey_X1$Tukey_diff), Tukey_p = list(X1 = tukey_X1$Tukey_p), Tukey_signif = list(X1 = tukey_X1$Tukey_signif)))
    
  }
  parameters_X2 <- function(Y, X1, X2){
    srY = mean(Y)
    mAlpha = aggregate(Y, by = list(X1), FUN = "mean")
    mBeta = aggregate(Y, by = list(X2), FUN = "mean")
    
    SSt = sum((Y - srY)^2)
    Ni = aggregate(Y, by = list(X1), FUN = "length")
    Ny = aggregate(Y, by = list(X2), FUN = "length")
    SSa = sum((mAlpha$x - srY)^2 * Ni$x)
    SSb = sum((mBeta$x - srY)^2 *Ny$x)
    SSe = SSt - SSa - SSb
    
    df_a = length(unique(X1)) - 1
    df_b = length(unique(X2)) - 1
    df = length(Y) - (length(unique(X1)) - 1) - (length(unique(X2)) - 1) - 1
    
    Sa = SSa/df_a
    Sb = SSb/df_b
    Se = SSe/df
    Fa = Sa / Se
    Fb = Sb / Se
    
    pA = pf(Fa, df_a, df, lower.tail = FALSE)
    pB = pf(Fb, df_b, df, lower.tail = FALSE)
    
    # TukeyHSD Two Way
    tukey_X1 = tukey(mAlpha, X1, Se, Ni, df)
    tukey_X2 = tukey(mBeta, X2, Se, Ny, df)
    
    return(list(Sum_Sq = c(SSa, SSb, SSe), Mean_Sq = c(Sa,Sb,Se), F_value = c(Fa, Fb), df = c(df_a, df_b, df), p_val = c(pA, pB), Tukey_diff = list(X1 = tukey_X1$Tukey_diff, X2 = tukey_X2$Tukey_diff), Tukey_p = list(X1 = tukey_X1$Tukey_p, X2 = tukey_X2$Tukey_p), Tukey_signif = list(X1 = tukey_X1$Tukey_signif, X2 = tukey_X2$Tukey_signif)))
    
  }
  create_table <- function(out, name){
    df <- c(out$df)
    sum_sq <- c(out$Sum_Sq)
    Mean <- c(out$Mean_Sq)
    F_val <- c(signif(out$F_value, digits = 4),'')
    P <- c(signif(out$p_val, digits = 4), '')
    stars <- c(stars(out$p_val), '')
    table <- format(data.frame(df, sum_sq, Mean, F_val, P, stars),  digits = 3)
    rownames(table) <- c(name, 'Residuals')
    colnames(table) <- c('Df', 'Sum Sq', 'Mean Sq', 'F Value', 'Pr(>F)', 'Signif.')
    print(table)
    cat('---\n')
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    cat('\n--- TukeyHSD Diff ---\n')
    print(out$Tukey_diff)
    cat('\n--- TukeyHSD p_value ---\n\n')
    print(out$Tukey_p)
    cat('\n--- TukeyHSD Signif. ---\n\n')
    print(out$Tukey_signif, quote = FALSE)
  }
  
  if (missing(X2)){
    out <- parameters_X1(Y, X1)
  }
  else {
    out <- parameters_X2(Y, X1, X2)
  }
  
  name <- names
  create_table(out, name)
}

