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

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  # check if age is a positive integer
  checkmate::assert_count(age)
  if (age > 150) {
    warning("Wow. You look so much younger.")
  }
  if (age < 16) {
    warning("Stop drinking kid, that's bad for you!")
  }
  
  # check if sex is one of the two genders made by god and accepted by Viktor Orbán
  sex <- match.arg(sex)
  
  # check if height is a single numeric value
  checkmate::assert_number(height, finite = TRUE, lower = .Machine$double.eps)
  
  if (height < 62.8) {
    warning("Congratulations! You are the smallest person on earth.")
  }
  if (height > 251) {
    warning("Congratulations! You are the tallest person on earth.")
  }
  
  # check if weight is a single numeric value
  checkmate::assert_number(weight, finite = TRUE, lower = .Machine$double.eps)
  
  if (weight < 20) {
    warning("Are you sure you haven't forgot a 0?")
  }
  if (weight > 600) {
    warning("Congratulations! You are the heaviest person on earth.")
  }
  
  # check if drinking_time is a vector of POSIXct - time variables
  checkmate::assert_posixct(drinking_time, len = 2)
  checkmate::assert(drinking_time[2] - drinking_time[1] < )
  
  if (drinking_time > ) {
    warning("Are you sure you haven't forgot a 0?")
  }
 
  
}