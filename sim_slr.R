sim_slr = function(n_obs) {
  sim_data =
    tibble(
      x = rnorm(n_obs, mean = 1, sd = 1),
      y = 2 + 3*x + rnorm(n_obs , mean = 0, sd = 1)
    )
  
  fit = sim_data |> 
    lm(y ~ x, data = _)
  
  return_df = 
    tibble(
      beta0_hat = coef(fit)[1],
      beta1_hat = coef(fit)[2]
    )
  
  return(return_df)
}