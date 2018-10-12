# Save file parameters

directory <- "C:/Users/Lowell Moore/Desktop/traveling salesman"
if(!dir.exists(directory)){
  dir.create(directory)
}

output_path <- paste(directory, "ts_anneal_params.csv", sep = "/")
save_pdf <- TRUE
pdf_path <- paste(directory, "traveling_salesman.pdf", sep = "/")

# Parameters for synthetic annealing
temp <- 1
temp_min <- 1E-6
alpha <- 0.9
n_wander <- 200

# Function definitions
cost <- function(soln, city_xs, city_ys){
  distances <- numeric(0)
  for(i in 2:length(soln)){
    city_next <- soln[i]
    city_prev <- soln[i-1]
    delta_x <- city_xs[city_next] - city_xs[city_prev]
    delta_y <- city_ys[city_next] - city_ys[city_prev]
    dist_i <- ((delta_x)^2 + (delta_y^2))^0.5
    distances <- c(distances, dist_i)
  }
  total_dist <- sum(distances)
  return(total_dist)
}

calc_segs <- function(soln, city_xs, city_ys){
  distances <- numeric(0)
  mid_xs <- numeric(0)
  mid_ys <- numeric(0)
  for(i in 2:length(soln)){
    city_next <- soln[i]
    city_prev <- soln[i-1]
    
    delta_x <- city_xs[city_next] - city_xs[city_prev]
    delta_y <- city_ys[city_next] - city_ys[city_prev]
    dist_i <- ((delta_x)^2 + (delta_y^2))^0.5
    distances <- c(distances, dist_i)
    
    mid_x <- (city_xs[city_next] + city_xs[city_prev])/2
    mid_y <- (city_ys[city_next] + city_ys[city_prev])/2
    mid_xs <- c(mid_xs, mid_x)
    mid_ys <- c(mid_ys, mid_y)
  }
  return(cbind.data.frame(distances, mid_xs, mid_ys))
}

neighbor <- function(old_soln){
  pos <- sample(x = 1:length(old_soln), size = 2, replace = FALSE)
  switch <- old_soln[pos]
  new_soln <- old_soln
  new_soln[pos] <- rev(switch)
  return(new_soln)
}

calc_ap <- function(cost_old, cost_new, temp){
  ap <- exp((cost_old - cost_new)/temp)
  if(ap > 1){ap <- 1}
  return(ap)
}

plot_result <- function(){
  plot(city_xs, city_ys, type = "n", xlab = "City X pos.", ylab = "City Y pos.")
  mtext(text = paste("Total dist. = ", round(old_dist, 2), sep = "")
        , side = 3, line = 1, adj = 0, cex = 1.2)
  lines(city_xs[order], city_ys[order], lty = 2)
  points(city_xs, city_ys, pch = 21, bg = "blue")
  
  #text(city_xs[order], city_ys[order], labels = 1:n_cities, adj = c(0.5, -1))
  #segs <- calc_segs(soln = order, city_xs, city_ys)
  #text(segs$mid_xs, segs$mid_ys, label = round(segs$distances, 2), col = "red", cex = 0.5)
}

# Generate random city locations
n_cities <- 100
if(FALSE){
  city_xs <- rnorm(n_cities)
  city_ys <- rnorm(n_cities)
}
if(TRUE){
  city_xs <- runif(n_cities)
  city_ys <- runif(n_cities)
}

# Start saving .pdf file
if(save_pdf){
  pdf(pdf_path, width = 8, height = 6, useDingbats = FALSE)
}

# Set 2x2 display
par(mfrow = c(2, 2))

# Set random start condition
order <- sample(size = n_cities, x = 1:n_cities, replace = FALSE)
old_dist <- cost(soln = order, city_xs, city_ys)

# Plot the initial guess
plot_result()

# Initialize output file
save_params <- data.frame(temp = temp, order = paste(order, collapse = " ")
                          , distance = old_dist, prob_accept = 1)

# Perform annealing calculations
while(temp > temp_min){
  # random wander
  for(i in 1:n_wander){
    new_order <- neighbor(order)
    new_dist <- cost(soln = new_order, city_xs, city_ys)
    prob_accept <- calc_ap(cost_old = old_dist, cost_new = new_dist, temp)
    
    random <- runif(n = 1, min = 0, max = 1)
    if(prob_accept > random){
      order <- new_order
      old_dist <- new_dist
      
      if(TRUE){
        new_row <- data.frame(temp = temp, order = paste(order, collapse = " ")
                              , distance = old_dist, prob_accept = prob_accept)
        save_params <- rbind(save_params, new_row)
      }
      if(FALSE){
      print(data.frame(temp = temp
                       #, order = paste(order, collapse = " ")
                       , distance = old_dist
                       , prob_accept = prob_accept))
      }
    }
  }
  print(paste("T =", temp))
  temp <- temp*alpha
}

# Plot final results
plot_result()
plot(1:nrow(save_params), save_params$distance
     , type = "l", col = rgb(0, 0, 0.6)
     , xlab = "Iteration #", ylab = "Cost (distance)"
     )
plot(1:nrow(save_params), save_params$prob_accept
     , type = "p", col = rgb(0, 0, 0.6), cex = 0.7
     , xlab = "Iteration #", ylab = "Acceptance probability"
     )

# Save run parameters to .csv
write.csv(save_params, output_path, row.names = FALSE)

# Stop .pdf
if(save_pdf){
  dev.off()
}

# reset display to 1x1
par(mfrow = c(1, 1))