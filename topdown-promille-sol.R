#### input check functions ####
# checks if age is specified meaningful
check_age <- function(age){
  # check if age is a positive integer
  checkmate::assert_count(age)
  
  # check if the age variable has unrealistic values
  if (age > 150) {
    warning("Wow. You look so much younger.")
  }
  if (age < 16) {
    warning("Stop drinking kid, that's bad for you!")
  }
}

# checks if height is specified meaningful
check_height <- function(height){
  # check if height is a single numeric value
  checkmate::assert_number(height, finite = TRUE, lower = .Machine$double.eps)
  
  # check if height has unrealistic values
  if (height < 62.8) {
    warning("Congratulations! You are the smallest person on earth.")
  }
  if (height > 251) {
    warning("Congratulations! You are the tallest person on earth.")
  }
}

# checks if weight is specified meaningful
check_weight <- function(weight){
  # check if weight is a single numeric value
  checkmate::assert_number(weight, finite = TRUE, lower = .Machine$double.eps)
  
  # check if weight has unrealistic values
  if (weight < 20) {
    warning("Are you sure you haven't forgot a 0?")
  }
  if (weight > 600) {
    warning("Congratulations! You are the heaviest person on earth.")
  }
}

# checks if drinking_time is specified meaningful
check_drinking_time <- function(drinking_time, age){
  # check if drinking_time is a vector of POSIXct - time variables
  checkmate::assert_posixct(drinking_time, len = 2)
  
  # compute the actual duration of drinking
  drinking_time_duration <- as.numeric(difftime(drinking_time[2], drinking_time[1], units = "secs"))
  
  # check if the drinking_time exceeds the age
  checkmate::assert(drinking_time_duration < age * 365 * 24 * 3600)
  
  # check if the drinking time is unrealistic
  if (drinking_time_duration > 14 * 24 * 3600) {
    warning("You should probably go to therapy.")
  }
}

# checks if drinks is specified meaningful
check_drinks <- function(drinks){
  #check if drinks is either a vector or a list
  checkmate::assert(checkmate::check_list(drinks),
                    checkmate::check_vector(drinks),
                    combine = "or"
  )
  #check if drinks only contains defined drinks
  checkmate::assert_subset(names(drinks),c("massn", "hoibe", "wein","schnaps"))
  
  #if length of drinks is 0, don't trust the input
  if (length(drinks) == 0) {
    warning("Are you sure you don't lie to me?")
  }
  else {
    quantity_sum <- 0
    # check if every quantity is an integer >= 0
    for (i in 1:length(drinks)) {
      quantity <- drinks[[i]]
      checkmate::assert_count(quantity)
      quantity_sum <- quantity_sum + quantity 
    }
    # if the sum of all quantities is 0, don't trust the input
    if (quantity_sum == 0) {
      warning("Are you sure you don't lie to me?")
    }
  }
}

#### alcohol level calculation functions ####
# compute consumed mass of alcohol
compute_alc_mass <- function(drinks) {
  which_drinks <- names(drinks)
  if (length(drinks) == 0) {
    return(0)
  }
  mass <- 0
  for (i in 1:length(drinks)) {
    current_drink <- which_drinks[i]
    current_quantity <- drinks[[i]]
    if (current_drink == "massn") {
      mass <- mass + current_quantity * 1000 * 0.06 * 0.8 
    }
    if (current_drink == "hoibe") {
      mass <- mass + current_quantity * 500 * 0.06 * 0.8
    }
    if (current_drink == "wein") {
      mass <- mass + current_quantity * 200 * 0.11 * 0.8 
    }
    if (current_drink == "schnaps") {
      mass <- mass + current_quantity * 40 * 0.4 * 0.8
    }
  }
  return(mass)
}

# compute body water volume
compute_body_water_volume <- function(age, sex, height, weight) {
  if (sex == "male") {
    volume <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  }
  if (sex == "female") {
    volume <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight 
  }
  return(volume)
}

# compute distribution factor according to Whatson
compute_distr_fact <- function(body_water_volume, weight) {
  distr_fact <- 1.055 * body_water_volume / (0.8 * weight)
  return(distr_fact)
}

# compute blood level of alcohol (without considering drinking time)
compute_alcohol_level <- function(alc_mass, distr_fact, weight) {
  concentration <- alc_mass / (distr_fact * weight)
  return(concentration)
}

# compute drinking-time-adjusted blood level of alcohol
compute_alcohol_level_adjusted <- function(blood_concentration, drinking_time_duration) {
  drinking_time_duration_hrs <- drinking_time_duration / 3600
  if (drinking_time_duration_hrs > 1) {
    concentration_adjusted <- blood_concentration - ((drinking_time_duration_hrs - 1) * 0.15)
    return(concentration_adjusted)  
  }
  return(blood_concentration)
}

# compute your alcohol level
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  # do the input checks
  check_age(age)
  # check if sex is one of the two genders made by god and accepted by Viktor Orbán
  sex <- match.arg(sex)
  check_height(height)
  check_weight(weight)
  check_drinking_time(drinking_time, age)
  check_drinks(drinks)
  
  # compute necessary intermediate results
  alc_mass <- compute_alc_mass(drinks)
  body_water_volume <- compute_body_water_volume(age, sex, height, weight)
  distr_fact <- compute_distr_fact(body_water_volume, weight)
  
  # level of blood alcohol without considering drinking time:
  blood_concentration <- compute_alcohol_level(alc_mass, distr_fact, weight)
  
  return(blood_concentration)
  
  # compute the actual duration of drinking
  drinking_time_duration <- as.numeric(difftime(drinking_time[2], drinking_time[1], units = "secs"))
  drinking_time_duration <- 0
  # level of blood alcohol adjusted by the drinking time
  blood_concentration_adjusted <- compute_alcohol_level_adjusted(blood_concentration, drinking_time_duration)
  
  if (blood_concentration_adjusted > 0) {
    return(list(blood_concentration_adjusted, tolerance = 0.01))
  }
}

