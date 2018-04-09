# TODO:
# replace referneces to global object stratii in
# d_name <- paste0(gsub(' ', '_', gsub('/', '_', file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
# with function arguments and adjust function calls in district_groups.r
get_label_for_NatHoe <- function(height_class) {
  label <- switch(as.character(height_class),
    "1" = "Planar",
    "2" = "Kollin",
    "3" = "Submontan",
    "4" = "Montan",
    "5" = "Hochmontan",
    stop(paste("NatHoe ", height_class, " out of range."))
  )
  return(label)
}
tex_table <- function(file_name, out) {
  tools::texi2dvi(file_name, pdf = TRUE, clean = TRUE)
  pdf_name <- sub("\\.tex$", "\\.pdf", basename(file_name))
  file.rename(pdf_name, file.path(graphics_directory, pdf_name))
  table_pdf <- ascii_umlauts(file.path(graphics_directory, paste0("table_", out, ".pdf")))
  table_pdf_out <- file.path(tables_directory, paste0(out, ".pdf"))
  file.link(
    to = table_pdf_out,
    from = file.path(table_pdf)
  )
}
table_tex_front_matter <- function() {
  return(c(
    generator_notice,
    "\\documentclass{standalone}\n",
    "\\input{.tex/preamble.latex}\n",
    "\\usepackage{caption}\n",
    "\\begin{document}\n",
    "\\minipage{1.08\\textwidth}\n",
    "\\captionsetup{labelformat=empty}\n"
  ))
}
table_tex_back_matter <- function() return("\\endminipage\n\\end{document}")
tex_text_width <- function() return(0.4)
theme_bold_axes <- function() {
  return(
    ggplot2::theme(
      axis.title = ggplot2::element_text(colour = "black", size = 20),
      axis.text = ggplot2::element_text(colour = "black", size = 15)
    )
  )
}
theme_bold_legend <- function() {
  return(
    ggplot2::theme(
      legend.title = ggplot2::element_text(colour = "black", size = 20),
      legend.text = ggplot2::element_text(colour = "black", size = 20)
    )
  )
}
theme_bold_title <- function() {
  return(
    ggplot2::theme(plot.title = ggplot2::element_text(colour = "black", size = 14))
  )
}
theme_rotate_xasis_labels <- function() {
  return(
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -70, hjust = 0, vjust = 1))
  )
}
ptplot <- function(obj) {
  graphics::plot(obj + theme_bold_axes() + theme_bold_title() + theme_bold_legend() +
    ggplot2::theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ))
}

tplot <- function(obj) {
  graphics::plot(obj + theme_bold_axes() + theme_bold_title() + theme_bold_legend() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) + theme_rotate_xasis_labels())
}
tplot1 <- function(obj) {
  graphics::plot(obj + theme_bold_axes() + theme_bold_title() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    theme_rotate_xasis_labels() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(colour = "black", size = 12),
      legend.text = ggplot2::element_text(colour = "black", size = 12)
    ))
}


to_tex <- function(..., file_name = dot_district_tex, append = TRUE) {
  cat(..., "\n", file = file_name, append = append, sep = "")
}

grouping_species <- function(species = c("Fichte", "Wei\u00dftanne"),
                             minimal_share = 10,
                             shares = species_shares,
                             species_list = get_bwi_species_groups("regional")) {
  species_shares <- shares[species]

  distinct_species <- which(species_shares >= minimal_share)
  group <- setdiff(seq(along.with = species_shares), distinct_species)
  ## group labels are never null, so we only add them if they contain anything
  label <- names(distinct_species)
  if (length(group) > 0) {
    label <- c(label, paste(species[group], collapse = "/"))
  }

  species_index <- which(species_list$ba.text %in% species)
  group_index <- species_index[group]
  distinct_species_index <- species_index[distinct_species]

  grouping <- species_list$ba.grupp[distinct_species_index]
  grouping[[length(grouping) + 1]] <-
    unlist(species_list$ba.grupp[group_index])
  return(list(label = label, group = grouping))
}

group_district_species <-
  function(groupings = list(
               c("Fichte", "Wei\u00dftanne", "Douglasie", "Kiefer", "L\u00e4rchen/sNB"),
               c("Buche", "Eichen"),
               c("Esche", "Bergahorn", "HBu", "sBlb", "Aln")
             ),
             shares = NULL) {
    species_groups <- NULL
    for (species in groupings) {
      y <- grouping_species(species, shares = shares)
      # order matters!
      species_groups$label <- c(species_groups$label, y$label)
      species_groups$group <- c(species_groups$group, y$group)
    }
    names(species_groups) <- c("ba.text", "ba.grupp")
    return(species_groups)
  }


## % plotting functions
copyright <- function() {
  anno <- ggplot2::annotate("text",
    label = "Copyright 2015: Abt. BuI, FVA BW",
    size = 4, colour = "white",
    x = -Inf, y = -Inf, hjust = 0, vjust = 0
  )
  return(anno)
}

plot_groups_areas <- function(b1, b2, b3,
                              do_errors_relative = FALSE,
                              graphic_width = get_options("graphics_width"),
                              graphic_height = graphics_height,
                              graphic_directory = graphics_directory,
                              file_name_district = regional_file_name,
                              title_district = krs.grupp$string[i],
                              species_groups_labels = c(district_groups$ba.text, "Alle BA")) {
  # reformat species labels as factor
  species_label <- factor(species_groups_labels,
    levels = species_groups_labels
  )
  data_frame <- rbind( # have fun with arrays!
    data.frame(
      bwi = "1987",
      species = species_label,
      prediction = b1$T.FVBN.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b1$T.FVBN.Bagr.Akl.Dkl[1, 2, , , ]
    ),
    data.frame(
      bwi = "2002",
      species = species_label,
      prediction = b2$T.FVBN.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b2$T.FVBN.Bagr.Akl.Dkl[1, 2, , , ]
    ),
    data.frame(
      bwi = "2012",
      species = species_label,
      prediction = b3$T.FVBN.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b3$T.FVBN.Bagr.Akl.Dkl[1, 2, , , ]
    )
  )
  table_data <- data_frame
  levels(data_frame$species) <- gsub("/", "\n", levels(species_label))
  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Fl\u00e4che"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot(ggplot2::ggplot(
    data = data_frame[ data_frame$species != "Alle BA", ],
    ggplot2::aes(
      y = prediction,
      x = species,
      fill = bwi
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors(), name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab("Fl\u00e4che [ha]") + copyright() +
    if (TITLE_PLOT) {
      ggplot2::ggtitle(title_district)
    })
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )

  to_tex(
    "\n\\begin{center}\\includegraphics{", out_path,
    "}\\end{center}"
  )

  names(table_data) <- c("BWI", "Artengruppe", "Vorhersage", "Standardfehler")
  caption <- c('Fl\\"achen und Fehler in Hektar.')
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c('Fl\\"achen in Hektar, Fehler in Prozent.')
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Artengruppe ~ ...
  )
  df_v_abs <- df_cast
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(".*_Vorhersage", "Fl\u00e4che", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  ## noch die Anteile
  data_frame <- rbind( # have fun with arrays!
    data.frame(
      bwi = "1987",
      species = species_label,
      prediction = b1$FVBN.ha.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b1$FVBN.ha.Bagr.Akl.Dkl[1, 2, , , ]
    ),
    data.frame(
      bwi = "2002",
      species = species_label,
      prediction = b2$FVBN.ha.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b2$FVBN.ha.Bagr.Akl.Dkl[1, 2, , , ]
    ),
    data.frame(
      bwi = "2012",
      species = species_label,
      prediction = b3$FVBN.ha.Bagr.Akl.Dkl[1, 1, , , ],
      standard_error = b3$FVBN.ha.Bagr.Akl.Dkl[1, 2, , , ]
    )
  )
  type_name <- "Fl\u00e4chenanteil"
  out_name <- paste(d_name, type_name, sep = "_")
  table_data <- data_frame[data_frame$species != "Alle BA", ]
  names(table_data) <- c("BWI", "Artengruppe", "Vorhersage", "Standardfehler")
  caption <- c("Fl\u00e4chenanteil und Fehler in Prozent.")
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c("Anteile und Fehler in Prozent.")
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Artengruppe ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(".*_Vorhersage", "Anteil", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  return(invisible(df_v_abs))
}

plot_groups_stocks <- function(b1, b2, b3,
                               do_errors_relative = FALSE,
                               graphic_width = get_options("graphics_width"),
                               graphic_height = graphics_height,
                               graphic_directory = graphics_directory,
                               file_name_district = regional_file_name,
                               title_district = krs.grupp$string[i],
                               species_groups_labels = c(district_groups$ba.text, "Alle BA")) {
  # reformat species labels as factor
  species_label <- factor(species_groups_labels,
    levels = species_groups_labels
  )
  data_frame <- rbind( # have fun with arrays!
    data.frame(
      bwi = "1987",
      species = species_label,
      prediction = b1$T.FVBN.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b1$T.FVBN.Bagr.Akl.Dkl[2, 2, , , ]
    ),
    data.frame(
      bwi = "2002",
      species = species_label,
      prediction = b2$T.FVBN.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b2$T.FVBN.Bagr.Akl.Dkl[2, 2, , , ]
    ),
    data.frame(
      bwi = "2012",
      species = species_label,
      prediction = b3$T.FVBN.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b3$T.FVBN.Bagr.Akl.Dkl[2, 2, , , ]
    )
  )
  table_data <- data_frame
  names(table_data) <- c(
    "BWI", "Artengruppe",
    "Vorhersage",
    "Standardfehler"
  )
  levels(data_frame$species) <- gsub("/", "\n", levels(species_label))
  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Vorrat"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot(ggplot2::ggplot(
    data = subset(data_frame, species != "Alle BA"),
    ggplot2::aes(
      y = prediction,
      x = species,
      fill = bwi
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors(), name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab(expression(paste("Vorrat [", m^3, "]", sep = ""))) + copyright() +
    if (TITLE_PLOT) {
      ggplot2::ggtitle(title_district)
    })
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics{", out_path,
    "}\\end{center}"
  )
  table_data <- data_frame
  names(table_data) <- c("BWI", "Artengruppe", "Vorhersage", "Standardfehler")
  caption <- c('Vorr\\"ate und Fehler in Kubikmetern Derbholz mit Rinde.')
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c('Vorr\\"ate in Kubikmetern Derbholz mit Rinde, Fehler in
                      Prozent.')
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Artengruppe ~ ...
  )
  df_v_abs <- df_cast
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(".*_Vorhersage", "Vorrat", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)

  ## noch: Hektarvorraete HB
  data_frame <- rbind( # have fun with arrays!
    data.frame(
      bwi = "1987",
      species = species_label,
      prediction = b1$FVBN.ha.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b1$FVBN.ha.Bagr.Akl.Dkl[2, 2, , , ]
    ),
    data.frame(
      bwi = "2002",
      species = species_label,
      prediction = b2$FVBN.ha.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b2$FVBN.ha.Bagr.Akl.Dkl[2, 2, , , ]
    ),
    data.frame(
      bwi = "2012",
      species = species_label,
      prediction = b3$FVBN.ha.Bagr.Akl.Dkl[2, 1, , , ],
      standard_error = b3$FVBN.ha.Bagr.Akl.Dkl[2, 2, , , ]
    )
  )
  table_data <- data_frame
  names(table_data) <- c(
    "BWI", "Artengruppe",
    "Vorhersage",
    "Standardfehler"
  )
  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Vorrat_je_Hektar"
  out_name <- paste(d_name, type_name, sep = "_")
  table_data <- data_frame
  names(table_data) <- c("BWI", "Artengruppe", "Vorhersage", "Standardfehler")
  caption <- c('Vorr\\"ate und Fehler in Kubikmetern Derbholz mit Rinde je Hektar.')
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c('Vorr\\"ate in Kubikmetern Derbholz mit Rinde je Hektar,
                      Fehler in Prozent.')
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Artengruppe ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(".*_Vorhersage", "Vorrat", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  return(invisible(df_v_abs))
}
plot_group_stocks_by_girth <- function(b1, b2, b3, v,
                                       do_errors_relative = FALSE,
                                       graphic_width = get_options("graphics_width"),
                                       graphic_height = graphics_height,
                                       graphic_directory = graphics_directory,
                                       file_name_district = regional_file_name,
                                       title_district = krs.grupp$string[i]) {
  # to skip the first girth class, we need their number
  num_girth_classes <- length(b1$DKL)
  # get an ordered factor
  girth_classes <- factor(b1$DKL[2:num_girth_classes],
    levels = b1$DKL[2:num_girth_classes]
  )
  ## % plot stock by girth
  for (species_group in b3$BAGR) {
    group_index <- which(species_group == b3$BAGR)
    data_frame <- rbind( # have fun with arrays!
      data.frame(
        bwi = "1987",
        group = girth_classes,
        prediction = b1$T.FVBN.Bagr.Akl.Dkl[2, 1, group_index, , 2:num_girth_classes],
        standard_error = b1$T.FVBN.Bagr.Akl.Dkl[2, 2, group_index, , 2:num_girth_classes]
      ),
      data.frame(
        bwi = "2002",
        group = girth_classes,
        prediction = b2$T.FVBN.Bagr.Akl.Dkl[2, 1, group_index, , 2:num_girth_classes],
        standard_error = b2$T.FVBN.Bagr.Akl.Dkl[2, 2, group_index, , 2:num_girth_classes]
      ),
      data.frame(
        bwi = "2012",
        group = girth_classes,
        prediction = b3$T.FVBN.Bagr.Akl.Dkl[2, 1, group_index, , 2:num_girth_classes],
        standard_error = b3$T.FVBN.Bagr.Akl.Dkl[2, 2, group_index, , 2:num_girth_classes]
      )
    )
    table_data <- data_frame
    names(table_data) <- c(
      "BWI", "Durchmesser",
      "Vorhersage",
      "Standardfehler"
    )
    d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
    type_name <- paste("Vorrat", gsub(" ", "_", gsub("/", "_", species_group)), sep = "_")
    out_name <- paste(d_name, type_name, sep = "_")
    out_file <- paste0(out_name, ".eps")
    out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
    grDevices::postscript(out_path,
      width = graphic_width, height = graphic_height,
      horizontal = FALSE, onefile = FALSE, paper = "special",
      family = "Courier"
    )
    tplot(ggplot2::ggplot(
      data = data_frame,
      ggplot2::aes(
        y = prediction,
        x = group,
        fill = bwi
      ),
      group = bwi
    ) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = prediction - standard_error,
        ymax = prediction + standard_error
      ),
      width = .3,
      position = ggplot2::position_dodge(width = 0.9)
      ) +
      ggplot2::scale_fill_manual(values = get_bwi_colors(), name = "BWI") +
      ggplot2::scale_y_continuous(labels = german_number) +
      ggplot2::xlab("Durchmesser [cm]") +
      ggplot2::ylab(expression(paste("Vorrat [", m^3, "]", sep = ""))) + copyright() +
      if (TITLE_PLOT) {
        ggplot2::ggtitle(bquote(atop(
          .(title_district),
          atop(italic(.(species_group)), "")
        )))
      })
    grDevices::dev.off()
    file.link(
      to = file.path(plots_directory, out_file),
      from = file.path(out_path)
    )
    species_group_text <- paste(vapply(
      strsplit(species_group, "/")[[1]],
      get_label_for_abbreviation,
      "character"
    ), collapse = ", ")
    to_tex("\n\\subsubsection{", species_group_text, "}")
    to_tex(
      "\n\\begin{center}\\includegraphics{", out_path,
      "}\\end{center}"
    )
    table_data <- data_frame
    names(table_data) <- c("BWI", "Durchmesser", "Vorhersage", "Standardfehler")
    caption <- c('Vorr\\"ate und Fehler in Kubikmetern Derbholz mit Rinde.')
    if (do_errors_relative) {
      table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
      caption <- c('Vorr\\"ate in Hektar, Fehler in Prozent.')
    }
    df_cast <- reshape2::dcast(reshape2::melt(table_data),
      value.var = "value", formula =
        Durchmesser ~ ...
    )
    alle_durchmesser <- v[gsub("\n", "/", v$Artengruppe) == species_group, ]

    df_cast <-
      cbind(
        Durchmesser = factor(
          c(levels(df_cast$Durchmesser), "Alle"),
          c(levels(df_cast$Durchmesser), "Alle")
        ),
        as.data.frame(rbind(
          as.matrix(df_cast[, 2:ncol(df_cast)]),
          as.numeric(alle_durchmesser[2:length(alle_durchmesser)])
        ))
      )
    df_cast <- german_number(df_cast, digits = c(2, 1))
    names(df_cast) <- gsub(".*_Vorhersage", "Vorrat", gsub(
      ".*_Standardfehler",
      "Fehler",
      names(df_cast)
    ))
    label_row <- paste(
      "BWI & ", paste0("\\multicolumn{2}{c}{",
        levels(table_data$BWI), "}",
        collapse = " & "
      ),
      "\\\\"
    )
    print(
      file = dot_district_tex, floating = TRUE, append = TRUE,
      size = "scriptsize", booktabs = TRUE,
      hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
      xtable::xtable(df_cast,
        caption = caption,
        align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
          collapse = ""
        ))
      ),
      add.to.row = list(pos = list(-1), command = label_row),
      include.rownames = FALSE
    )
    table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
    to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
    print(
      file = table_tex, floating = TRUE, append = TRUE,
      size = "scriptsize", booktabs = TRUE,
      hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
      xtable::xtable(df_cast,
        caption = caption,
        align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
          collapse = ""
        ))
      ),
      add.to.row = list(pos = list(-1), command = label_row),
      include.rownames = FALSE
    )
    to_tex(table_tex_back_matter(), file_name = table_tex)
    tex_table(table_tex, out_name)
  }
}

plot_group_area_by_age <- function(b1, b2, b3, a,
                                   do_errors_relative = FALSE,
                                   graphic_width = get_options("graphics_width"),
                                   graphic_height = graphics_height,
                                   graphic_directory = graphics_directory,
                                   file_name_district = regional_file_name,
                                   title_district = krs.grupp$string[i]) {
  # get an ordered factor
  age_classes <- factor(b1$AKL, levels = b1$AKL)
  ## % plot area by age
  for (species_group in b3$BAGR) {
    group_index <- which(species_group == b3$BAGR)
    data_frame <- rbind( # have fun with arrays!
      data.frame(
        bwi = "1987",
        group = age_classes,
        prediction = b1$T.FVBN.Bagr.Akl.Dkl[1, 1, group_index, , ],
        standard_error = b1$T.FVBN.Bagr.Akl.Dkl[1, 2, group_index, , ]
      ),
      data.frame(
        bwi = "2002",
        group = age_classes,
        prediction = b2$T.FVBN.Bagr.Akl.Dkl[1, 1, group_index, , ],
        standard_error = b2$T.FVBN.Bagr.Akl.Dkl[1, 2, group_index, , ]
      ),
      data.frame(
        bwi = "2012",
        group = age_classes,
        prediction = b3$T.FVBN.Bagr.Akl.Dkl[1, 1, group_index, , ],
        standard_error = b3$T.FVBN.Bagr.Akl.Dkl[1, 2, group_index, , ]
      )
    )
    table_data <- data_frame
    names(table_data) <- c(
      "BWI", "Alter",
      "Vorhersage",
      "Standardfehler"
    )
    d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
    type_name <- paste("Fl\u00e4che", gsub(" ", "_", gsub("/", "_", species_group)), sep = "_")
    out_name <- paste(d_name, type_name, sep = "_")
    out_file <- paste0(out_name, ".eps")
    out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
    grDevices::postscript(out_path,
      width = graphic_width, height = graphic_height,
      horizontal = FALSE, onefile = FALSE, paper = "special",
      family = "Courier"
    )
    tplot(ggplot2::ggplot(
      data = data_frame,
      ggplot2::aes(
        y = prediction,
        x = group,
        fill = bwi
      ),
      group = bwi
    ) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = prediction - standard_error,
        ymax = prediction + standard_error
      ),
      width = .3,
      position = ggplot2::position_dodge(width = 0.9)
      ) +
      ggplot2::scale_fill_manual(values = get_bwi_colors(), name = "BWI") +
      ggplot2::scale_y_continuous(labels = german_number) +
      ggplot2::xlab("Alter [a]") +
      ggplot2::ylab("Fl\u00e4che [ha]") + copyright() +
      if (TITLE_PLOT) {
        ggplot2::ggtitle(bquote(atop(
          .(title_district),
          atop(italic(.(species_group)), "")
        )))
      })
    grDevices::dev.off()
    file.link(
      to = file.path(plots_directory, out_file),
      from = file.path(out_path)
    )
    species_group_text <- paste(vapply(
      strsplit(species_group, "/")[[1]],
      get_label_for_abbreviation,
      "character"
    ), collapse = ", ")
    to_tex("\n\\subsubsection{", species_group_text, "}")
    to_tex(
      "\n\\begin{center}\\includegraphics{",
      out_path, "}\\end{center}"
    )
    table_data <- data_frame
    names(table_data) <- c("BWI", "Alter", "Vorhersage", "Standardfehler")
    caption <- c('Fl\\"achen und Fehler in Hektar.')
    if (do_errors_relative) {
      table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
      caption <- c('Fl\\"achen in Hektar, Fehler in Prozent.')
    }
    df_cast <- reshape2::dcast(reshape2::melt(table_data),
      value.var = "value", formula =
        Alter ~ ...
    )
    alle_alter <- a[gsub("\n", "/", a$Artengruppe) == species_group, ]

    df_cast <-
      cbind(
        Alter = factor(
          c(levels(df_cast$Alter), "Alle"),
          c(levels(df_cast$Alter), "Alle")
        ),
        as.data.frame(rbind(
          as.matrix(df_cast[, 2:ncol(df_cast)]),
          as.numeric(alle_alter[2:length(alle_alter)])
        ))
      )
    df_cast <- german_number(df_cast, digits = c(2, 1))
    names(df_cast) <- gsub(".*_Vorhersage", "Fl\u00e4che", gsub(
      ".*_Standardfehler",
      "Fehler",
      names(df_cast)
    ))
    label_row <- paste(
      "BWI & ", paste0("\\multicolumn{2}{c}{",
        levels(table_data$BWI), "}",
        collapse = " & "
      ),
      "\\\\"
    )
    table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
    to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
    print(
      file = table_tex, floating = TRUE, append = TRUE,
      size = "scriptsize", booktabs = TRUE,
      hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
      xtable::xtable(df_cast,
        caption = caption,
        align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
          collapse = ""
        ))
      ),
      add.to.row = list(pos = list(-1), command = label_row),
      include.rownames = FALSE
    )
    to_tex(table_tex_back_matter(), file_name = table_tex)
    tex_table(table_tex, out_name)
    print(
      file = dot_district_tex, floating = TRUE, append = TRUE,
      size = "scriptsize", booktabs = TRUE,
      hline.after = c(-1, nrow(df_cast) - 1, nrow(df_cast)),
      xtable::xtable(df_cast,
        caption = caption,
        align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
          collapse = ""
        ))
      ),
      add.to.row = list(pos = list(-1), command = label_row),
      include.rownames = FALSE
    )
  }
}
plot_deadwood <- function(deadwood_2, deadwood_3_2, deadwood_3,
                          deadwood_2a, deadwood_3_2a, deadwood_3a,
                          graphic_width = get_options("graphics_width"),
                          graphic_height = graphics_height,
                          graphic_directory = graphics_directory,
                          file_name_district = regional_file_name,
                          title_district = krs.grupp$string[i]) {
  deadwood_class <- c(
    "Liegend", "Stehend (Baum)", "Stehend (Bruchst\u00fcck)",
    "Wurzelstock", "Abfuhrrest", "Gesamtvorrat"
  )
  deadwood_class <- factor(deadwood_class, levels = deadwood_class)
  data_frame <- rbind(
    data.frame(
      bwi = "2002",
      group = deadwood_class,
      prediction = c(deadwood_2$ThVN.ha.klass[1, 1, ], deadwood_2a$ThVN.ha.klass[1, 1, ]),
      standard_error =
        c(deadwood_2$ThVN.ha.klass[1, 2, ], deadwood_2a$ThVN.ha.klass[1, 2, ])
    ),
    data.frame(
      bwi = "2012*",
      group = deadwood_class,
      prediction =
        c(deadwood_3_2$ThVN.ha.klass[1, 1, ], deadwood_3_2a$ThVN.ha.klass[1, 1, ]),
      standard_error =
        c(deadwood_3_2$ThVN.ha.klass[1, 2, ], deadwood_3_2a$ThVN.ha.klass[1, 2, ])
    ),
    data.frame(
      bwi = "2012",
      group = deadwood_class,
      prediction =
        c(deadwood_3$ThVN.ha.klass[1, 1, ], deadwood_3a$ThVN.ha.klass[1, 1, ]),
      standard_error =
        c(deadwood_3$ThVN.ha.klass[1, 2, ], deadwood_3a$ThVN.ha.klass[1, 2, ])
    )
  )
  table_data <- data_frame
  names(table_data) <- c(
    "BWI", "Totholzart",
    "Vorhersage",
    "Standardfehler"
  )
  levels(deadwood_class) <- gsub(" ", "\n", levels(deadwood_class))
  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Totholz"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot(ggplot2::ggplot(
    data = data_frame,
    ggplot2::aes(
      y = prediction,
      x = group,
      fill = bwi
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors(), name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab(expression(paste("Vorrat [", m^3,
      ha^-1, "]",
      sep = ""
    ))) + copyright() + ggplot2::ggtitle("2012*: Berechnung nach Kriterien von 2002"))
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics[width=", 1, "\\textwidth]{", out_path,
    "}\\end{center}"
  )

  df_cast <- reshape2::dcast(reshape2::melt(table_data), value.var = "value", formula = Totholzart ~ ...)
  df_r <- df_cast
  names(df_cast) <- gsub(".*_Vorhersage", "Vorrat", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  caption <- c('Totholz: Vorr\\"ate und Fehler in Kubikmetern Derbholz mit Rinde je Hektar,
                   2012*:  Berechnung des Totholzes 2012 nach Aufnahmekriterien von 2002.')
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      )),
      digits = 1
    ),
    decimal.mark = ",", big.mark = ".",
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      )),
      digits = 1
    ),
    decimal.mark = ",", big.mark = ".",
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  return(invisible(df_r))
}
plot_growth_loss <- function(g_l_12, g_l_23, g_l_12_all,
                             do_errors_relative = FALSE,
                             graphic_width = get_options("graphics_width"),
                             graphic_height = graphics_height,
                             graphic_directory = graphics_directory,
                             file_name_district = regional_file_name,
                             title_district = krs.grupp$string[i],
                             species_groups_labels = district_groups$ba.text) {
  species_label <- factor(c(species_groups_labels, "Alle BA"),
    levels = c(species_groups_labels, "Alle BA")
  )
  data_frame <- rbind(
    data.frame(
      period = get_period(1),
      group = "Zuwachs",
      x = species_label,
      ## g_l_12 is lacking 'Alle BA'
      prediction = c(g_l_12$iVB.bagr.akl.dkl[9, 1, , , ], g_l_12_all$iVB.bagr.akl.dkl[9, 1, , , ]),
      standard_error = c(g_l_12$iVB.bagr.akl.dkl[9, 2, , , ], g_l_12_all$iVB.bagr.akl.dkl[9, 2, , , ])
    ),
    data.frame(
      period = get_period(1),
      group = "Abgang",
      x = species_label,
      prediction = c(g_l_12$VB.A.bagr.akl.dkl[9, 1, , , ], g_l_12_all$VB.A.bagr.akl.dkl[9, 1, , , ]),
      standard_error = c(g_l_12$VB.A.bagr.akl.dkl[9, 2, , , ], g_l_12_all$VB.A.bagr.akl.dkl[9, 2, , , ])
    ),
    data.frame(
      period = get_period(2),
      group = "Zuwachs",
      x = species_label,
      prediction = g_l_23$iVB.bagr.akl.dkl[9, 1, , , ],
      standard_error = g_l_23$iVB.bagr.akl.dkl[9, 2, , , ]
    ),
    data.frame(
      period = get_period(2),
      group = "Abgang",
      x = species_label,
      prediction = g_l_23$VB.A.bagr.akl.dkl[9, 1, , , ],
      standard_error = g_l_23$VB.A.bagr.akl.dkl[9, 2, , , ]
    )
  )
  table_data <- data_frame
  names(table_data)
  names(table_data) <- c("Periode", "Art", "Artengruppe", "Vorhersage", "Standardfehler")
  levels(data_frame$x) <- gsub("/", "\n", levels(species_label))
  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Zuwachs_Abgang"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  data_frame$foo <-
    factor(paste(data_frame$period, data_frame$group),
      levels = unique(paste(data_frame$period, data_frame$group)),
      ordered = TRUE,
      labels = unique(paste(data_frame$group, data_frame$period))
    )
  tplot1(ggplot2::ggplot(
    data = data_frame,
    ggplot2::aes(
      y = prediction,
      x = x,
      fill = foo
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity", position = ggplot2::position_dodge()
      , size = 1
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .6,
    position = ggplot2::position_dodge(width = 0.9)
    , size = 1
    ) +
    ggplot2::scale_fill_manual(values = c("#33FF33", "#FF6666", "#006600", "#990000"), name = "") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab(expression(paste("Derbholz mit Rinde [", m^3, ha^-1, a^-1, "]", sep = ""))) +
    copyright() + ggplot2::theme(legend.position = "top"))
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics[width=", 1,
    "\\textwidth]{", out_path,
    "}\\end{center}"
  )
  caption <- c("Zuwachs, ausgeschiedener Vorrat (Abgang) und Fehler in Kubikmeternn Derbholz mit Rinde pro Hektar und Jahr.")
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c('Vorratsa\\"anderungen in Kubikmeternn Derbholz mit Rinde
                      pro Hektar und Jahr, Fehler in Prozent.')
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Artengruppe + Periode ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub("_Vorhersage", "", gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "& ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 2, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = c("ll", "ll", rep("|rr", ncol(df_cast) - 1))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 2, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = c("ll", "ll", rep("|rr", ncol(df_cast) - 1))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
}
plot_ownership <- function(ownerships,
                           graphic_width = get_options("graphics_width"),
                           graphic_height = graphics_height,
                           graphic_directory = graphics_directory,
                           file_name_district = regional_file_name,
                           title_district = krs.grupp$string[i]) {
  ownerships$labels <- map_abbreviations_to_labels(ownerships$ownership)
  ownerships$ownership_realive_area <- paste0(round_and_prettify_german(ownerships$area / sum(ownerships$area) * 100), "%")

  ownerships$position <- cumsum(ownerships$area) - ownerships$area / 2
  ownerships$radius <- c(rep(c(1.6, 1.8), length.out = nrow(ownerships)))
  tmp <- ggplot2::ggplot(
    data = ownerships,
    ggplot2::aes(
      x = factor(1),
      weight = area,
      fill = ownership
    )
  ) + ggplot2::geom_bar() + coord_polar(theta = "y") +
    ggplot2::xlab("") + ylab("") + scale_x_discrete(breaks = NULL) +
    ggplot2::scale_fill_manual(
      values = as.character(get_colors_for_ownership(map_labels_to_abbreviations(ownerships$ownership)))
      , name = "Eigentumsart",
      labels = ownerships$ownership
    ) +
    geom_text(ggplot2::aes(
      x = radius, y = position,
      label = ownership_realive_area
    )
    ,
    size = 6, color = "black"
    ) +
    ggplot2::ggtitle(paste(
      "Waldfl\u00e4che:", round_and_prettify_german(sum(ownerships$area)),
      "ha"
    ))

  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)))
  type_name <- "Eigentumsarten"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  ptplot(tmp)

  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics[width=", 0.8,
    "\\textwidth]{", out_path,
    "}\\end{center}"
  )
  to_tex(
    'Hier bezeichnet StW den Staatswald, KW den K\\"o{}rperschaftswald,',
    " GPW den Gro\\ss{}privatwald mit mehr als 200 Hektar und MPW den",
    " Mittleren Privatwald mit mehr als 5 und nicht mehr als 200 Hektar",
    " und KPW den  Kleinprivatwald mit nicht mehr als 5 Hektar",
    ' Waldfl\\"a{}che.'
  )
}



ntns_output <- function(ntns.list) {
  return(data.frame(
    NTNS = factor(ntns.list$NTNS, levels = ntns.list$NTNS),
    Flaeche_ha = round(ntns.list$NTNS.Flaeche.Anteil[1, 1, ], 0),
    SE_Flaeche_ha = round(ntns.list$NTNS.Flaeche.Anteil[1, 2, ], 0),
    Anteil = round(ntns.list$NTNS.Flaeche.Anteil[2, 1, ], 1),
    SE_Anteil = round(ntns.list$NTNS.Flaeche.Anteil[2, 2, ], 1)
  ))
}
plot_ntns <- function(ntns2, ntns3,
                      graphic_width = get_options("graphics_width"),
                      graphic_height = graphics_height,
                      graphic_directory = graphics_directory,
                      file_name_district = regional_file_name,
                      title_district = krs.grupp$string[i]) {
  data_frame <- rbind(
    cbind(ntns_output(ntns2), bwi = "2002"),
    cbind(ntns_output(ntns3), bwi = "2012")
  )
  names(data_frame) <- sub("SE_Anteil", "standard_error", names(data_frame))
  names(data_frame) <- sub("Anteil", "prediction", names(data_frame))
  tmp <- ggplot2::ggplot(
    data = data_frame,
    ggplot2::aes(
      y = prediction,
      x = NTNS,
      fill = bwi
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors()[2:3], name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab("Fl\u00e4chenanteil [%]") + copyright() +
    if (TITLE_PLOT) {
      ggplot2::ggtitle(title_district)
    }
  d_name <- paste0(gsub(" ", "_", gsub(
    "/", "_",
    file_name_district
  )), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Naturn\u00e4he"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot(tmp)
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics{", out_path,
    "}\\end{center}"
  )

  table_data <- data_frame[, c(1, 4:6)]
  names(table_data) <- c("Naturnaehestufe", "Vorhersage", "Standardfehler", "BWI")
  caption <- c('Naturn\\"ahestufen in der Fl\\"ache und Fehler in Prozent.')
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Naturnaehestufe ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(".*_Vorhersage", 'Fl\\"achenanteil', gsub(
    ".*_Standardfehler",
    "Fehler",
    names(df_cast)
  ))
  label_row <- paste(
    "& ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
}

plot_loss <- function(loss1, loss2, loss1_all, loss2_all,
                      graphic_width = get_options("graphics_width"),
                      graphic_height = graphics_height,
                      graphic_directory = graphics_directory,
                      file_name_district = regional_file_name,
                      title_district = krs.grupp$string[i]) {
  data_frame <- rbind(
    data.frame(
      period = get_period(1),
      x = loss1$DK,
      prediction = loss1$VB.A.bagr.akl.dkl[7, 1, 1, , ],
      standard_error =
        loss1$VB.A.bagr.akl.dkl[7, 2, 1, , ]
    ),
    data.frame(
      period = get_period(2),
      x = loss2$DK,
      prediction = loss2$VB.A.bagr.akl.dkl[7, 1, 1, , ],
      standard_error = loss2$VB.A.bagr.akl.dkl[7, 2, 1, , ]
    )
  )
  tmp <- ggplot2::ggplot(
    data = data_frame,
    ggplot2::aes(
      y = prediction,
      x = x,
      fill = period
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors()[2:3], name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("Durchmesser [cm]") +
    ggplot2::ylab(expression(paste("J\u00e4hrl. Ernte ohne Rinde [", m^3, "]", sep = ""))) + copyright() +
    if (TITLE_PLOT) {
      ggplot2::ggtitle(title_district)
    }
  d_name <- paste0(gsub(" ", "_", gsub(
    "/", "_",
    file_name_district
  )), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Nutzungen"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot(tmp)
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )
  to_tex(
    "\n\\begin{center}\\includegraphics{", out_path,
    "}\\end{center}"
  )

  table_data <- data_frame
  table_data <-
    rbind(
      table_data,
      data.frame(
        period = "1987 - 2002",
        x = "Gesamt",
        prediction = loss1_all$VB.A.bagr.akl.dkl[7, 1, 1, , ],
        standard_error = loss1_all$VB.A.bagr.akl.dkl[7, 2, 1, , ]
      ),
      data.frame(
        period = "2002 - 2012",
        x = "Gesamt",
        prediction = loss2_all$VB.A.bagr.akl.dkl[7, 1, 1, , ],
        standard_error = loss2_all$VB.A.bagr.akl.dkl[7, 2, 1, , ]
      )
    )
  caption <- c('J\\"a{}hrliche Ernte in Kubikmetern Erntevolumen ohne Rinde 
                  nach St\\"a{}rkeklassen \\"u{}ber alle Baumarten.')
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      period ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(
    "period", "Periode",
    gsub(
      ".*_prediction", "Nutzung",
      gsub(
        ".*_standard_error", "Fehler",
        names(df_cast)
      )
    )
  )
  label_row <- paste(
    "Durchmesser & ", paste0("\\multicolumn{2}{c}{",
      c(
        paste0(
          levels(table_data$x)[1:3],
          " cm"
        ),
        levels(table_data$x)[4]
      ), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast) - 1, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
}
regeneration <- function(r2_a, r2_bs1, r2_bs2,
                         r3_a, r3_bs1, r3_bs2,
                         graphic_width = get_options("graphics_width"),
                         graphic_height = graphics_height,
                         graphic_directory = graphics_directory,
                         file_name_district = regional_file_name,
                         title_district = krs.grupp$string[i]) {
  regeneration3 <- data.frame(
    group = c(r3_a$BAGR, "Alle Arten"),
    prediction = r3_a$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r3_a$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  unter_schirm3 <- data.frame(
    group = c(r3_bs2$BAGR, "Alle Arten"),
    prediction = r3_bs2$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r3_bs2$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  abgedeckt3 <- data.frame(
    group = c(r3_bs1$BAGR, "Alle Arten"),
    prediction = r3_bs1$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r3_bs1$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  regeneration2 <- data.frame(
    group = c(r2_a$BAGR, "Alle Arten"),
    prediction = r2_a$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r2_a$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  unter_schirm2 <- data.frame(
    group = c(r2_bs2$BAGR, "Alle Arten"),
    prediction = r2_bs2$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r2_bs2$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  abgedeckt2 <- data.frame(
    group = c(r2_bs1$BAGR, "Alle Arten"),
    prediction = r2_bs1$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ],
    error = r2_bs1$Verjg.kl4m.BAF.VArt.BAGR[2, 6, ]
  )
  nv_prozent2 <- r2_a$Verjg.kl4m.BAF.VArt.BAGR[1, 1, ] / r2_a$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ] * 100
  nv_prozent3 <- r3_a$Verjg.kl4m.BAF.VArt.BAGR[1, 1, ] / r3_a$Verjg.kl4m.BAF.VArt.BAGR[1, 6, ] * 100

  df_cast3 <- data.frame(
    BWI = "2012",
    Artengruppe = abgedeckt3$group,
    "Unter Schirm" = unter_schirm3$prediction,
    Abgedeckt = abgedeckt3$prediction,
    Gesamt = regeneration3$prediction,
    Fehler = regeneration3$error,
    "NV Anteil" = nv_prozent3,
    check.names = FALSE
  )

  df_cast2 <- data.frame(
    BWI = "2002",
    Artengruppe = abgedeckt2$group,
    "Unter Schirm" = unter_schirm2$prediction,
    Abgedeckt = abgedeckt2$prediction,
    Gesamt = regeneration2$prediction,
    Fehler = regeneration2$error,
    "NV Anteil" = nv_prozent2,
    check.names = FALSE
  )
  df_cast <- rbind(df_cast2, df_cast3)
  # data_frame <- reshape2::melt(df_cast, id.vars = c("BWI", "Artengruppe"))
  d_name <- paste0(gsub(" ", "_", gsub(
    "/", "_",
    file_name_district
  )), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Naturverj\u00fcngung"
  out_name <- paste(d_name, type_name, sep = "_")


  table_data <- german_number(df_cast, digits = 3)
  caption <- c('Verj\\"ungung in Hektar, Anteil der Naturverj\\"ungung 
                  (NV Anteil) an der gesamten Naturverj\\"ungung (Gesamt)
                  in Prozent.')
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize",
    xtable::xtable(table_data,
      caption = caption,
      align = paste0("ll", paste(rep(
        "rr",
        ceiling((ncol(df_cast) - 1)
        / 2)
      ),
      collapse = ""
      ))
    ),
    include.rownames = FALSE
  )

  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = FALSE, append = TRUE,
    size = "scriptsize",
    xtable::xtable(df_cast,
      align = paste0("ll", paste(rep(
        "rr",
        ceiling((ncol(df_cast) - 1)
        / 2)
      ),
      collapse = ""
      ))
    ),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)

  return(invisible(table_data))
}
plot_silviculturally_relevant <- function(relevant_species_2, relevant_species_3,
                                          do_errors_relative = FALSE,
                                          graphic_width = get_options("graphics_width"),
                                          graphic_height = graphics_height,
                                          graphic_directory = graphics_directory,
                                          file_name_district = regional_file_name,
                                          title_district = krs.grupp$string[i],
                                          species_groups_labels = c(district_groups$ba.text, "Alle BA")) {
  data_frame <- rbind(
    data.frame(
      bwi = "2002", species = relevant_species_2$FBA,
      prediction = round(relevant_species_2$"FBA.Fl\u00e4che"[1, , 2] * 100, 1),
      standard_error = round(relevant_species_2$"FBA.Fl\u00e4che"[2, , 2] * 100, 2)
    ),
    data.frame(
      bwi = "2012", species = relevant_species_3$FBA,
      prediction = round(relevant_species_3$"FBA.Fl\u00e4che"[1, , 2] * 100, 1),
      standard_error = round(relevant_species_3$"FBA.Fl\u00e4che"[2, , 2] * 100, 2)
    )
  )

  d_name <- paste0(gsub(" ", "_", gsub("/", "_", file_name_district)), "_", gsub(",", "", gsub(" ", "_", names(stratii[stratum_index]))))
  type_name <- "Forstlich_bedeutsame_Arten"
  out_name <- paste(d_name, type_name, sep = "_")
  out_file <- paste0(out_name, ".eps")
  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  grDevices::postscript(out_path,
    width = graphic_width, height = graphic_height,
    horizontal = FALSE, onefile = FALSE, paper = "special",
    family = "Courier"
  )
  tplot1(ggplot2::ggplot(
    data = data_frame,
    ggplot2::aes(
      y = prediction,
      x = species,
      fill = bwi
    ),
    group = bwi
  ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = prediction - standard_error,
      ymax = prediction + standard_error
    ),
    width = .3,
    position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_fill_manual(values = get_bwi_colors()[2:3], name = "BWI") +
    ggplot2::scale_y_continuous(labels = german_number) +
    ggplot2::xlab("") +
    ggplot2::ylab("Fl\u00e4chenanteil in Prozent") + copyright() +
    if (TITLE_PLOT) {
      ggplot2::ggtitle(title_district)
    })
  grDevices::dev.off()
  file.link(
    to = file.path(plots_directory, out_file),
    from = file.path(out_path)
  )

  out_path <- ascii_umlauts(file.path(graphic_directory, out_file))
  to_tex(
    "\n\\begin{center}\\includegraphics{", out_path,
    "}\\end{center}"
  )
  table_data <- data_frame
  names(table_data) <- c("BWI", "Art", "Vorhersage", "Standardfehler")
  out_name <- paste(d_name, type_name, sep = "_")
  table_data <- data_frame[data_frame$species != "Alle BA", ]
  names(table_data) <- c("BWI", "Art", "Vorhersage", "Standardfehler")
  caption <- c("Forstlich bedeutsame Arten: Anteile und Fehler in Prozent.")
  if (do_errors_relative) {
    table_data$Standardfehler <- table_data$Standardfehler / table_data$Vorhersage * 100
    caption <- c("Forstlich bedeutsame Arten: Anteile und Fehler in Prozent.")
  }
  df_cast <- reshape2::dcast(reshape2::melt(table_data),
    value.var = "value", formula =
      Art ~ ...
  )
  df_cast <- german_number(df_cast, digits = c(2, 1))
  names(df_cast) <- gsub(
    ".*_Vorhersage", "Fl\u00e4chenanteil",
    gsub(".*_Standardfehler", "Fehler", names(df_cast))
  )
  label_row <- paste(
    "BWI & ", paste0("\\multicolumn{2}{c}{",
      levels(table_data$BWI), "}",
      collapse = " & "
    ),
    "\\\\"
  )
  print(
    file = dot_district_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  table_tex <- ascii_umlauts(file.path(graphic_directory, paste0("table_", out_name, ".tex")))
  to_tex(table_tex_front_matter(), file_name = table_tex, append = FALSE)
  print(
    file = table_tex, floating = TRUE, append = TRUE,
    size = "scriptsize", booktabs = TRUE,
    hline.after = c(-1, 0, nrow(df_cast)),
    xtable::xtable(df_cast,
      caption = caption,
      align = paste0("ll", paste(rep("|rr", (ncol(df_cast) - 1) / 2),
        collapse = ""
      ))
    ),
    add.to.row = list(pos = list(-1), command = label_row),
    include.rownames = FALSE
  )
  to_tex(table_tex_back_matter(), file_name = table_tex)
  tex_table(table_tex, out_name)
}
