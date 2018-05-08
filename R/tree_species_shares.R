#' Creates vector with percentage values for tree species
#'
#' Function selects area for each tree from the statistical list and calculates
#' the percentages.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list A list holding the statistical data, create with one
#' of the FVBN.bagrupp.akl.dkl.stratum.fun.2[abcde] functions.
#' @export
#' @return Named array with tree species areas in percent.
tree_species_percent <- function(statistics_list) {
  checkmate::assertList(statistics_list)
  tree_species_areas <- get(
    "T.FVBN.Bagr.Akl.Dkl",
    statistics_list
  )[1, 1, , , ]
  names <- NULL
  for (attribute in c("BAGR", "AKL", "DKL")) {
      if (length(statistics_list[[attribute]]) > 1) ## catch both 0 and 1 
          names[[attribute]] <- statistics_list[[attribute]]
  }
  if (length(names) > 1) {
      dimnames(tree_species_areas) <- names
  } else {
      names(tree_species_areas) <- unlist(names)
  }
  total_area <- sum(tree_species_areas)
  tree_species_percent <- tree_species_areas / total_area * 100
  return(tree_species_percent)
}

#' Calculates relative tree species area
#'
#' Function calculates the relative tree species area for each tree species
#' group for all three BWI's.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list String which defines the list holding the statistical
#'  data.
#' @param bagr A list setting up tree species groups. Stick with the default.
#' @export
#' @return Data Frame with information about relative tree species area for each
#'  species. Information for all three BWI's.
melt_tree_species_percent <- function(statistics_list, 
                                      bagr = get_bwi_species_groups()) {
  checkmate::assertString(statistics_list)
  tree_species_labels <- factor(bagr$bagr.lab,
    levels = bagr$bagr.lab
  )
  df1 <- get(paste0(statistics_list, ".1"))
  df2 <- get(paste0(statistics_list, ".2"))
  df3 <- get(paste0(statistics_list, ".3"))
  value <- Reduce(
      function(...) merge(..., all = T),
      list(
        data.frame(
          bwi = "1987",
          group = tree_species_labels,
          value = tree_species_percent(df1)
          ),
        data.frame(
          bwi = "2002",
          group = tree_species_labels,
          value = tree_species_percent(df2)
          ),
        data.frame(
          bwi = "2012",
          group = tree_species_labels,
          value = tree_species_percent(df3)
          )
        )
      )
  return(value)
}

#' Calculates relative tree species area
#'
#' Function calculates the relative tree species area for each tree species
#' group for all three BWI's. BWI's just given through the different years:
#' 1987, 2002 and 2012.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list String which defines the list holding the statistical
#'  data.
#' @export
#' @return Data Frame with information about relative tree species area for each
#'  species. Information for all three BWI's.
cast_tree_species_percent <- function(statistics_list) {
  checkmate::assertString(statistics_list)
  molten_data <- melt_tree_species_percent(statistics_list)
  casted_data <- reshape2::dcast(molten_data, value.var = "value", group ~ bwi)
  names(casted_data)[1] <- "Baumartengruppe"
  return(casted_data)
}
