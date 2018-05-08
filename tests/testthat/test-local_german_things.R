if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
}
context("german_things.R")

test_that("prettify_data_frame", {
              data(mtcars)
  result <- prettify_data_frame(mtcars, digits = 1)
  
  reference <-
structure(c("21", "21", "22.8", "21.4", "18.7", "18.1", "14.3", 
"24.4", "22.8", "19.2", "17.8", "16.4", "17.3", "15.2", "10.4", 
"10.4", "14.7", "32.4", "30.4", "33.9", "21.5", "15.5", "15.2", 
"13.3", "19.2", "27.3", "26", "30.4", "15.8", "19.7", "15", "21.4", 
"6", "6", "4", "6", "8", "6", "8", "4", "4", "6", "6", "8", "8", 
"8", "8", "8", "8", "4", "4", "4", "4", "8", "8", "8", "8", "4", 
"4", "4", "8", "6", "8", "4", "160", "160", "108", "258", "360", 
"225", "360", "146.7", "140.8", "167.6", "167.6", "275.8", "275.8", 
"275.8", "472", "460", "440", "78.7", "75.7", "71.1", "120.1", 
"318", "304", "350", "400", "79", "120.3", "95.1", "351", "145", 
"301", "121", "110", "110", "93", "110", "175", "105", "245", 
"62", "95", "123", "123", "180", "180", "180", "205", "215", 
"230", "66", "52", "65", "97", "150", "150", "245", "175", "66", 
"91", "113", "264", "175", "335", "109", "3.9", "3.9", "3.8", 
"3.1", "3.1", "2.8", "3.2", "3.7", "3.9", "3.9", "3.9", "3.1", 
"3.1", "3.1", "2.9", "3", "3.2", "4.1", "4.9", "4.2", "3.7", 
"2.8", "3.1", "3.7", "3.1", "4.1", "4.4", "3.8", "4.2", "3.6", 
"3.5", "4.1", "2.6", "2.9", "2.3", "3.2", "3.4", "3.5", "3.6", 
"3.2", "3.1", "3.4", "3.4", "4.1", "3.7", "3.8", "5.2", "5.4", 
"5.3", "2.2", "1.6", "1.8", "2.5", "3.5", "3.4", "3.8", "3.8", 
"1.9", "2.1", "1.5", "3.2", "2.8", "3.6", "2.8", "16.5", "17", 
"18.6", "19.4", "17", "20.2", "15.8", "20", "22.9", "18.3", "18.9", 
"17.4", "17.6", "18", "18", "17.8", "17.4", "19.5", "18.5", "19.9", 
"20", "16.9", "17.3", "15.4", "17.1", "18.9", "16.7", "16.9", 
"14.5", "15.5", "14.6", "18.6", "0", "0", "1", "1", "0", "1", 
"0", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "1", "1", 
"1", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", 
"1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", 
"0", "0", "0", "0", "1", "1", "1", "0", "0", "0", "0", "0", "1", 
"1", "1", "1", "1", "1", "1", "4", "4", "4", "3", "3", "3", "3", 
"4", "4", "4", "4", "3", "3", "3", "3", "3", "3", "4", "4", "4", 
"3", "3", "3", "3", "3", "4", "5", "5", "5", "5", "5", "4", "4", 
"4", "1", "1", "2", "1", "4", "2", "2", "4", "4", "3", "3", "3", 
"4", "4", "4", "1", "2", "1", "1", "2", "2", "4", "2", "1", "2", 
"2", "4", "6", "8", "2"), .Dim = c(32L, 11L), .Dimnames = list(
    c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", 
    "Hornet Sportabout", "Valiant", "Duster 360", "Merc 240D", 
    "Merc 230", "Merc 280", "Merc 280C", "Merc 450SE", "Merc 450SL", 
    "Merc 450SLC", "Cadillac Fleetwood", "Lincoln Continental", 
    "Chrysler Imperial", "Fiat 128", "Honda Civic", "Toyota Corolla", 
    "Toyota Corona", "Dodge Challenger", "AMC Javelin", "Camaro Z28", 
    "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2", "Lotus Europa", 
    "Ford Pantera L", "Ferrari Dino", "Maserati Bora", "Volvo 142E"
    ), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", 
    "am", "gear", "carb")))
  testthat::expect_identical(result, reference)
})

test_that("add_colSums_prettify_and_print_xtable", {
              data(mtcars)
  result <- add_colSums_prettify_and_print_xtable(mtcars)
  
  
  reference <-
"% latex table generated in R 3.5.0 by xtable 1.8-2 package\n% Tue May  8 18:03:32 2018\n\\begin{table}[ht]\n\\centering\n\\begin{tabular}{lrrrrrrrrrrr}\n  \\hline\n & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ mpg } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ cyl } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ disp } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ hp } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ drat } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ wt } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ qsec } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ vs } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ am } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ gear } & \\multicolumn{1}{>{\\centering}p{ 0.0833333333333333 \\textwidth}}{ carb } \\\\ \n  \\hline\nMazda RX4 & 21 & 6 & 160 & 110 & 4 & 3 & 16 & 0 & 1 & 4 & 4 \\\\ \n  Mazda RX4 Wag & 21 & 6 & 160 & 110 & 4 & 3 & 17 & 0 & 1 & 4 & 4 \\\\ \n  Datsun 710 & 23 & 4 & 108 & 93 & 4 & 2 & 19 & 1 & 1 & 4 & 1 \\\\ \n  Hornet 4 Drive & 21 & 6 & 258 & 110 & 3 & 3 & 19 & 1 & 0 & 3 & 1 \\\\ \n  Hornet Sportabout & 19 & 8 & 360 & 175 & 3 & 3 & 17 & 0 & 0 & 3 & 2 \\\\ \n  Valiant & 18 & 6 & 225 & 105 & 3 & 3 & 20 & 1 & 0 & 3 & 1 \\\\ \n  Duster 360 & 14 & 8 & 360 & 245 & 3 & 4 & 16 & 0 & 0 & 3 & 4 \\\\ \n  Merc 240D & 24 & 4 & 147 & 62 & 4 & 3 & 20 & 1 & 0 & 4 & 2 \\\\ \n  Merc 230 & 23 & 4 & 141 & 95 & 4 & 3 & 23 & 1 & 0 & 4 & 2 \\\\ \n  Merc 280 & 19 & 6 & 168 & 123 & 4 & 3 & 18 & 1 & 0 & 4 & 4 \\\\ \n  Merc 280C & 18 & 6 & 168 & 123 & 4 & 3 & 19 & 1 & 0 & 4 & 4 \\\\ \n  Merc 450SE & 16 & 8 & 276 & 180 & 3 & 4 & 17 & 0 & 0 & 3 & 3 \\\\ \n  Merc 450SL & 17 & 8 & 276 & 180 & 3 & 4 & 18 & 0 & 0 & 3 & 3 \\\\ \n  Merc 450SLC & 15 & 8 & 276 & 180 & 3 & 4 & 18 & 0 & 0 & 3 & 3 \\\\ \n  Cadillac Fleetwood & 10 & 8 & 472 & 205 & 3 & 5 & 18 & 0 & 0 & 3 & 4 \\\\ \n  Lincoln Continental & 10 & 8 & 460 & 215 & 3 & 5 & 18 & 0 & 0 & 3 & 4 \\\\ \n  Chrysler Imperial & 15 & 8 & 440 & 230 & 3 & 5 & 17 & 0 & 0 & 3 & 4 \\\\ \n  Fiat 128 & 32 & 4 & 79 & 66 & 4 & 2 & 19 & 1 & 1 & 4 & 1 \\\\ \n  Honda Civic & 30 & 4 & 76 & 52 & 5 & 2 & 19 & 1 & 1 & 4 & 2 \\\\ \n  Toyota Corolla & 34 & 4 & 71 & 65 & 4 & 2 & 20 & 1 & 1 & 4 & 1 \\\\ \n  Toyota Corona & 22 & 4 & 120 & 97 & 4 & 2 & 20 & 1 & 0 & 3 & 1 \\\\ \n  Dodge Challenger & 16 & 8 & 318 & 150 & 3 & 4 & 17 & 0 & 0 & 3 & 2 \\\\ \n  AMC Javelin & 15 & 8 & 304 & 150 & 3 & 3 & 17 & 0 & 0 & 3 & 2 \\\\ \n  Camaro Z28 & 13 & 8 & 350 & 245 & 4 & 4 & 15 & 0 & 0 & 3 & 4 \\\\ \n  Pontiac Firebird & 19 & 8 & 400 & 175 & 3 & 4 & 17 & 0 & 0 & 3 & 2 \\\\ \n  Fiat X1-9 & 27 & 4 & 79 & 66 & 4 & 2 & 19 & 1 & 1 & 4 & 1 \\\\ \n  Porsche 914-2 & 26 & 4 & 120 & 91 & 4 & 2 & 17 & 0 & 1 & 5 & 2 \\\\ \n  Lotus Europa & 30 & 4 & 95 & 113 & 4 & 2 & 17 & 1 & 1 & 5 & 2 \\\\ \n  Ford Pantera L & 16 & 8 & 351 & 264 & 4 & 3 & 14 & 0 & 1 & 5 & 4 \\\\ \n  Ferrari Dino & 20 & 6 & 145 & 175 & 4 & 3 & 16 & 0 & 1 & 5 & 6 \\\\ \n  Maserati Bora & 15 & 8 & 301 & 335 & 4 & 4 & 15 & 0 & 1 & 5 & 8 \\\\ \n  Volvo 142E & 21 & 4 & 121 & 109 & 4 & 3 & 19 & 1 & 1 & 4 & 2 \\\\ \n   \\hline\nSumme & 643 & 198 & 7,383 & 4,694 & 115 & 103 & 571 & 14 & 13 & 118 & 90 \\\\ \n   \\hline\n\\end{tabular}\n\\caption{XXX} \n\\label{tab:mtcars}\n\\end{table}\n"
  result <- unlist(strsplit(result, split = "\n"))
  reference <- unlist(strsplit(reference, split = "\n"))
  result[2] <- NA
  reference[2] <- NA
  testthat::expect_identical(result, reference)
})
