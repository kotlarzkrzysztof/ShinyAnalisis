# T Test Script -----------------------------------------------------------
# Skrypt obejmuj?cy 3 rodzaje testu t-studenta
# Funkcja 'ttest' przysosowana do samodzielnego wyboru testu
# Dodanie argument?w domy?lnych w przypadku nie zdefiniowania przez u?ytkowanika
# Dodanie outputu przedstawiaj?cego uzyskane wyniki
# Dodanie funkcji dodaj?cej komponenty w przypadku przypisania funkcji 'ttest'
#
# Dodanie funkcji sprawdzaj?cej poprawno?? wsp?lnych argument?w "diagno":
# X jako numeric,
# Alpha w przedziale <0,1>
# Alternative jako (0, 1, 2)
# R?wno?? d?ugo?ci obu argument?w w funkcji test3 (paired test)
#
# Oraz funkcje sprawdzaj?ce poszczeg?lne argumenty w danych testach:
# mu jako numeric
# Y jako numeric



ttest <-
  function(X,
           Y = NULL,
           alpha = 0.05,
           alternative = 0,
           mu = NULL,
           paired = FALSE) {
    diagno <- function(X, alpha, alternative) {
      if (!alternative %in% c(0, 1, 2)) {
        stop('Alternative must be 0, 1 or 2')
      }
      
      else if (is.character(X)) {
        stop('X must be numeric')
      }
      
      else if (alpha > 1 || alpha < 0) {
        stop("Alpha must be a float number between 0 and 1")
      }
      
    }
    if (is.data.frame(X))
      X <- X[, 1]
    
    test1 = function(X, alpha, mu, alternative) {
      diagno(X, alpha, alternative)
      if (!is.numeric(mu)) {
        stop("Argument mu must be numeric")
      }
      
      m = mean(X)
      s = sd(X)
      n = length(X)
      test = sqrt(n) * (m - mu) / s
      df = n - 1
      if (alternative == 1) {
        p = pt(test, df, lower.tail = F)
      } else if (alternative == 2) {
        p = pt(test, df)
      } else {
        p = 2 * pt(abs(test), df, lower.tail = F)
      }
      if (p > alpha) {
        dec = 0
      } else {
        dec = '1'
      }
      
      cat(
        sprintf(
          '\n\tt-Test One Sample \n
  t = %f, df = %d, p-value = %e\n
  Decision: %s',
          test,
          df,
          p,
          dec
        )
      )
      invisible(list(
        stat_t = test,
        p_value = p,
        decision = dec
      ))
    }
    
    test2 = function(X, Y, alpha, alternative) {
      diagno(X, alpha, alternative)
      if (!is.numeric(Y)) {
        stop('Y must be numeric')
      }
      
      m1 = mean(X)
      m2 = mean(Y)
      v1 = var(X)
      v2 = var(Y)
      n1 = length(X)
      n2 = length(Y)
      df = n1 + n2 - 2
      test = (m1 - m2) / sqrt((n1 - 1) * v1 + (n2 - 1) * v2) * sqrt(n1 * n2 * (n1 +
                                                                                 n2 - 2) / (n1 + n2))
      if (alternative == 1) {
        p = pt(test, df, lower.tail = F)
      } else if (alternative == 2) {
        p = pt(test, df)
      } else {
        p = 2 * pt(abs(test), df, lower.tail = F)
      }
      if (p > alpha) {
        dec = '0'
      } else {
        dec = '1'
      }
      
      cat(
        sprintf(
          '\n\tt-Test Two Sample \n
  t = %f, df = %d, p-value = %e\n
  Decision: %s',
          test,
          df,
          p,
          dec
        )
      )
      invisible(list(
        stat_t = test,
        p_value = p,
        decision = dec
      ))
    }
    
    test3 = function(X, Y, alpha, alternative) {
      diagno(X, alpha, alternative)
      
      if (length(X) != length(Y))
        stop('Vectors must be same the length')
      
      if (!is.numeric(Y)) {
        stop('Y must be numeric')
      }
      
      D = X - Y
      m = mean(D)
      s = sd(D)
      n = length(D)
      test = sqrt(n) * (m) / s
      df = n - 1
      p = pt(test, df, lower.tail = T)
      
      if (p > alpha) {
        dec = '0'
      } else {
        dec = '1'
      }
      
      cat(
        sprintf(
          '\n\tt-Test Paired \n
  t = %f, df = %d, p-value = %e\n
  Decision: %s',
          test,
          df,
          p,
          dec
        )
      )
      invisible(list(
        stat_t = test,
        p_value = p,
        decision = dec
      ))
    }
    
    X <- as.vector(X)
    
    if (!(is.null(mu))) {
      return(test1(X, alpha, mu, alternative))
    }
    
    if (!paired %in% c(TRUE, FALSE)) {
      stop("Paired must be logical")
    }
    
    else if (paired == TRUE) {
      return(test3(X, Y, alpha, alternative))
    }
    
    else if (!(is.null(Y))) {
      return(test2(X, Y, alpha, alternative))
    }
    
  }
