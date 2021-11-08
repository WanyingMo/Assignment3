
# Team Members:

# Animesh Sharma                - as3592
# Yaniv Bronshtein              - yb262
# Wanying Mo                    - wm318
# Venkata Krishnam Raju Dalta   - vkd20
# Vipul Gharde                  - vig4
# Aditya Maheshwari             - am2971
# Toshitt Ahuja                 - ta498
# Fan Shen                      - fs470
moment_estimator <- function(data, distribution) {
  if (distribution == "Binomial") {
    ip = data$data
    n = data$n
    mubi2 = var(ip) + mean(ip) * mean(ip)
    pbi = sqrt((mean(ip) - mubi2) / (n - n * n))
    return(pbi)
  }
  
  else if (distribution == "Geometric") {
    mean = mean(data) + 1
    muge = var(data) + mean * mean
    pge = (-1 + sqrt(1 + 8 * muge)) / (2 * muge)
    return(pge)
  }
  
  else if (distribution == "Poisson") {
    mupo = var(data) + mean(data) * mean(data)
    lambda = (-1 + sqrt(1 + 4 * mupo)) / 2
    return(lambda)
  }
  
  else if (distribution == "Uniform") {
    a = mean(data) - sqrt(3 * var(data))
    b = mean(data) + sqrt(3 * var(data))
    return(c(a, b))
  }
  
  else if (distribution == "Normal") {
    muno = mean(data)
    sig2 = sqrt(var(data))
    return(c(muno, sig2))
  }
  
  else if (distribution == "Exponential") {
    muex = var(data) + mean(data) * mean(data)
    rate = sqrt(muex / 2)
    return(rate)
  }
  
  else if (distribution == "Beta") {
    ex_2 = var(data) + mean(data) * mean(data)
    m = mean(data)
    alpha = (m * m - ex_2 * m) / (ex_2 - m * m)
    beta = (1 - m) * alpha / m
    return(c(alpha, beta))
  }
  
  else if (distribution == "Multinormial") {
    ip = data$data
    n = data$n
    nmul = as.numeric(table(ip))
    pmul = c(nmul[1] / n, nmul[2] / n, nmul[3] / n)
    return(pmul)
  }
  
  else if (distribution == "Mul_Normal") {
    ip = data$data
    n = data$n
    mumn <-
      c(sum(ip[1:n, 1]) / n, sum(ip[1:n, 2]) / n, sum(ip[1:n, 3]) /
          n)
    mumn <- as.matrix(unlist(mumn))
    vamn2 <-
      matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 3, ncol = 3)
    for (i in 1:n) {
      vamn <- as.matrix(unlist(ip[i, 1:3])) - mumn
      vamn1 <- t(as.matrix(unlist(ip[i, 1:3])) - mumn)
      vamn2 <- vamn2 + vamn %*% vamn1
    }
    return(list(mumn, vamn2 / n))
  }
}
moment_estimator(list(data = rbinom(1000, 100, 0.5), n = 100), "Binomial")
moment_estimator(rgeom(1000, 0.5), "Geometric")
moment_estimator(rpois(1000, 0.5), "Poisson")
moment_estimator(runif(1000, 0, 1), "Uniform")
moment_estimator(rnorm(1000, 0, 2), "Normal")
moment_estimator(rexp(100, 1), "Exponential")
moment_estimator(rbeta(1000, 0.5, 1), "Beta")
moment_estimator(list(data = sample(c(1, 2, 3), 1000, replace = TRUE, c(0.1, 0.3, 0.6)), n = 1000), "Multinormial")
moment_estimator(list(data = mvrnorm(1000, c(1, 2, 3), matrix(
  c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3
)), n = 1000), "Mul_Normal")
