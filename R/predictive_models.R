
#' Function for making the predictions based on a linear model
#'
#' @param x named numeric vector of data
#' @param named numeric vector of model parameters for the linear model
#'
linear_function <- function(x, my_coef) {

  ## Arrange coefficients

  my_coef <- my_coef[names(my_coef)]

  ## Do the multiplication

  out <- x * my_coef

  ## Return

  sum(out)

}

#' Library of models
#'
model_library <- function(which_model = "sth") {

  switch(which_model,
         serv_plant = c(biomass = 0.00043),
         serv_co2 = c(lnCO2),
         other = c(biomass = 5,
                 CO2 = 0,
                 nitrates = -10,
                 retention = 1,
                 erosion = 1,
                 bare = 1,
                 aesthetics = 1,
                 employment = 1
         ),
         stop("Unknown model")
         )
}

#'
#'
apply_models <- function(sample, soil_type, temp, rain,
                         biomass, CO2, nitrates, retention,
                         erosion, bare,
                         water_bodies, nat_vegetation, protected_spaces,
                         geological_heritage, recreation, motor_vehicles,
                         accessibility,
                         employment) {

  ## Categorical variables

  Sagri <- as.numeric(soil_type == "Agricultural")
  Sforest <- as.numeric(soil_type == "Forest")

  ## Calculate indices

  #- Plant biomass

  index_biomass <- biomass*0.00043

  #- CO2 sequestration

  index_CO2 <- log(CO2)*0.7112 - 1.67*Sagri + 0.051*Sagri*log(CO2) + 0.0377*Sforest*log(CO2) + 0.2368*temp
  index_CO2 <- exp(index_CO2)

  #- Pollution

  index_polution <- -1.806*nitrates + 40.427*temp + 0.508*rain - 825.58

  #- Water retention

  index_retention <- 0.000241*retention - 2.79e-9*retention^2 + 0.0031*Sagri*retention + 1.2534*temp - 0.00216*rain - 8.5918
  index_retention <- exp(index_retention)

  #- Soil erosion

  index_soil <- 0.0002*erosion - 2.82e-9*erosion^2
  index_soil <- exp(index_soil)

  #- Vegetation

  index_vegetation <- -4.404*bare + 0.03*bare^2 + 168.13

  #- Aesthetics

  aesthetics <- (water_bodies + nat_vegetation + protected_spaces + geological_heritage + recreation + motor_vehicles + accessibility)*10/7

  index_aes <- 1530.63*aesthetics - 25266.4*Sagri - 25450.2*Sforest + 30152.49

  #- Employment

  index_employment <- 5726.38*employment

  ## Output

  tibble(biomass = index_biomass,
         CO2 = index_CO2,
         pollution = index_polution,
         retention = index_retention,
         erosion = index_soil,
         vegetation = index_vegetation,
         aesthetics = index_aes,
         employment = index_employment,
         total = biomass + CO2 + pollution + retention + erosion + vegetation + aesthetics + employment
         )

}

# model_library("sth")
#
# linear_function(c(biomass = 1,
#                   CO2 = 10,
#                   nitrates = -3,
#                   retention = 1,
#                   erosion = 1,
#                   bare = 1,
#                   aesthetics = 1,
#                   employment = 1
# ),
# model_library("other"))













