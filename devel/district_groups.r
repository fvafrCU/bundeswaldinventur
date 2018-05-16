#!/usr/bin/Rscript --vanilla
# okay, this is quick 'n' dirty, we're in a hurry.
# FIXME: write at least some documentation.
print(Sys.time())
bundeswaldinventur::get_global_objects()
library("bundeswaldinventur")
bundeswaldinventur::set_options(data_source = "bwibw", 
                                name = "bundeswaldinventur")
print(sessionInfo())
#% define local variables, lists and the like
if (interactive()) { 
    testing <- TRUE 
} else { 
    testing  <- FALSE
}

# Set to TRUE if you want to change the LaTeX wrapper files without 
# compiling the R analyses.
generate_tex_wrappers_only <- FALSE 
my_name <- get_script_name('district_groups.r')
landkreise_only <- TRUE
if (landkreise_only) {
    is_city <- grepl("Stadt", gsub("Universitätss", "S", gsub("Landeshaupts", "S", kreise$langKreis)))
    
    krs.grupp <- list(krs.bez = character(sum(as.integer(! is_city))), 
                      string = as.character(kreise$langKreis[! is_city]),
                      krs.code= as.list(kreise$codeKreis[! is_city])
                      )
} else {
    krs.grupp <- list(krs.bez = c("S/LB", "BB", "ES", "GP", "WN", 
                                  "HN (S/L)", "KÜN", "SHA", "TBB", "HDH", "AA", 
                                  "BAD/RA", "KA (S/L)", "HD/MA/RNK", "MOS", 
                                  "PF/Enz", "CW", "FDS", "FR/LBH", "EM", "OG", "RW",
                                  "VS", "TUT", "KN", "LÖ", "WT", "RT", "TÜ", "BL", 
                                  "UL/ADK", "BC", "FN", "RV", "SIG"), 
                      string =
                      c("Stuttgart/Ludwigsburg", "Böblingen", "Esslingen",
                        "Göppingen", "Rems-Murr-Kreis", "Heilbronn (S/L)",
                        "Hohenlohekreis", "Schwäbisch Hall", "Main-Tauber-Kreis", 
                        "Heidenheim", "Ostalbkreis", "Baden-Baden/Rastatt", 
                        "Karlsruhe", "Heidelberg/Mannheim/Rhein-Neckar-Kreis", 
                        "Neckar-Odenwald-Kreis", " Pforzheim/Enzkreis", "Calw", 
                        "Freudenstadt", "Freiburg/Breisgau-Hochschwarzwald", 
                        "Emmendingen", "Ortenaukreis", "Rottweil", 
                        "Schwarzwald-Baar-Kreis", "Tuttlingen", "Konstanz", 
                        "Lörrach", "Waldshut", "Reutlingen", "Tübingen", 
                        "Zollernalbkreis", "Ulm/Alb-Donau-Kreis", "Biberach", 
                        "Bodenseekreis", "Ravensburg", "Sigmaringen"),
                      krs.code=list(c(111, 118), 115, 116, 117, 119, c(121, 125), 
                                    126, 127, 128, 135, 136, c(211, 216), 
                                    c(212, 215), c(221, 222, 226), 225, c(231, 236),
                                    235, 237, c(311, 315), 316, 317, 325, 326, 327, 
                                    335, 336, 337, 415, 416, 417, c(421, 425), 426, 
                                    435, 436, 437)
                      )
}
bagr.xx <- list(
                bagr.lab = c("FI", "TA", "DGL", "KI", 
                             "SNB", "BU", "EI", "ES", 
                             "BAH", "HBU", "SBLB", "ALN"), 
                ba.grupp =list(c(10), c(30), c(40), c(20), 
                               c(50, 51, 11:19, 21:29, 31:39, 90:99), c(100), 
                               c(110, 111, 112),  c(120), c(140), c(130), 
                               c(113, 114, 121, 141:144, 150, 160, 170, 180, 
                                 181, 190:199), 
                               c(200, 201, 210:213, 220:224, 230, 240, 250:252, 
                                 290:299)),
                ba.text = c("Fichte", "Weißtanne", "Douglasie", "Kiefer", 
                             "Lärchen/sNB", "Buche", "Eichen", "Esche", 
                             "Bergahorn", "HBu", "sBlb", "Aln")
                )
n.bagr.xx <- length(bagr.xx$bagr.lab)
# parameters for graphics output
graphics_width <- 10
graphics_height <- golden_ratio(graphics_width)$a
# get aliases to long function names
FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2d
dead <- Totholz.klass.stratum.fun
#% prepare for output
generator_notice <- c("% This file was generated on ", date(), " by ", my_name, 
                      ".\n")
tex_front_matter <- c(generator_notice,
                      "\\documentclass[twoside, a4paper, DIV=15]{scrreprt}\n",
                      "\\input{.tex/preamble.latex}\n",
                      "\\begin{document}\n\\input{.tex/frontmatter.latex}")
tex_post_title <- "\\maketitle\n\\tableofcontents\n\\input{.tex/preface.latex}"
tex_back_matter <- "\\end{document}"
provide_output_directory('graphics')
provide_output_directory('output')
provide_output_directory('reports', output_directory)
provide_output_directory('plots', output_directory)
provide_output_directory('tables', output_directory)
all_districts_tex <- 'BWI3_Alle_regionalen_Auswertungen.tex'
#% initialize districts report
if (file.exists(all_districts_tex)) unlink(all_districts_tex)
to_tex(tex_front_matter, 
       file_name = all_districts_tex)
to_tex("\\subtitle{}\n",
       tex_post_title, file_name = all_districts_tex)


to_tex("\\part{Bundesland}", file_name = all_districts_tex)
regional  <- NULL
district_code <- FALSE ## needed in if(district_code == 437)
district_name <- "Baden-Württemberg"
regional_name <- paste("Land", district_name)
source("district_inloop.r")

to_tex("\\part{Regierungsbezirke}", file_name = all_districts_tex)
regierungsbezirke <- sort(unique(ecken.3$RBez))
names(regierungsbezirke) <- c("Stuttgart", "Karlsruhe", "Freiburg", "Tübingen")
if(testing) {
    num_regierungsbezirke <- 1
} else {
    num_regierungsbezirke <- length(regierungsbezirke)
}
for (i in 1:num_regierungsbezirke) {
    # set index for manual testing
    if (num_regierungsbezirke == 1) i <- 3
    ##% set local loop variables
    district_code <- regierungsbezirke[i]
    regional  <- list("RBez"  = district_code)
    district_name <- names(regierungsbezirke)[i]
    regional_name <- paste("Regierungsbezirk", district_name)
    source("district_inloop.r")
}

to_tex("\\part{Wuchsgebiete}", file_name = all_districts_tex)
wuchsgebiete  <-  c(
"Oberrheinisches Tiefland" = 1,
"Odenwald" = 2,
"Schwarzwald" = 3,
"Neckarland" = 4,
"Baar-Wutach" = 5,
"Schwäbische Alb" = 6,
"Südwestdeutsches Alpenvorland" = 7)
if(testing) {
    num_wuchsgebiete <- 1
} else {
    num_wuchsgebiete <- length(wuchsgebiete)
}
for (i in 1:num_wuchsgebiete) {
    # set index for manual testing
    if (num_wuchsgebiete == 1) i <- 5
    ##% set local loop variables
    district_code <- wuchsgebiete[i]
    regional  <- list("WGNr_BW"  = district_code)
    district_name <- names(wuchsgebiete)[i]
    regional_name <- paste("Wuchsgebiet", district_name)
    source("district_inloop.r")
}

to_tex("\\part{Raumordnerische Regionen}", file_name = all_districts_tex)
raumordnerische_regionen <- as.integer(unique(ecken.3$region))
names(raumordnerische_regionen) <- as.character(unique(ecken.3$region))
if(testing) {
    num_raumordnerische_regionen <- 1
} else {
    num_raumordnerische_regionen <- length(raumordnerische_regionen)
}
for (i in 1:num_raumordnerische_regionen) {
    # set index for manual testing
    if (num_raumordnerische_regionen == 1) i <- 1
    ##% set local loop variables
    district_code <- names(raumordnerische_regionen)[i]
    regional  <- list("region"  = district_code)
    district_name <- names(raumordnerische_regionen)[i]
    regional_name <- paste("Region", district_name)
    source("district_inloop.r")
}
to_tex("\\part{Kreise}", file_name = all_districts_tex)
if(testing) {
    num_districts <- 1
} else {
    num_districts <- length(krs.grupp$krs.bez)
}
for (i in 1:num_districts) {
    # set index for manual testing
    if (num_districts == 1) i <- 35 # 30 # 7
    if (num_districts == 1) i <- 30 # 7    
    if (num_districts == 1) i <- 2 # 7    
    if (num_districts == 1) i <- 1 # 7    
    if (num_districts == 1) i <- 23 # 7    
    if (num_districts == 1) i <- 11
    ##% set local loop variables
    district_code <- unlist(krs.grupp$krs.code[i])
    regional  <- list("Kreis"  = district_code)
    district_name <- krs.grupp$string[i]
    regional_name <- paste('Kreis', district_name)
    source("district_inloop.r")
}
to_tex('\\part{Nat\\"u{}rliche H\\"o{}henstufen}', file_name = all_districts_tex)
if(testing) {
    num_districts <- 1
} else {
    num_districts <- length(unique(ecken.3$NatHoe))
}
for (i in 1:num_districts) {
    # set index for manual testing
    if (num_districts == 1) i <- 5 
    ##% set local loop variables
    district_code <- sort(unique(ecken.3$NatHoe))[i]
    regional  <- list("NatHoe"  = district_code)
    district_name <- get_label_for_NatHoe(district_code)
    regional_name <- paste('Natürliche Höhenstufe', district_name)
    source("district_inloop.r")
}




to_tex(tex_back_matter, file_name = all_districts_tex)
tools::texi2dvi(all_districts_tex, pdf = TRUE)
file.link(
          to = file.path(reports_directory, sub(".tex", ".pdf", all_districts_tex)),
          from = sub(".tex", ".pdf", all_districts_tex)
          )

