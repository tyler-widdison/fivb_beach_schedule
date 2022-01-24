remap <- function(x, col, schema, from = "from", to = "to") {
  this_schema <- type_change(schema)
  if (col %in% names(x)) x[[col]] <- plyr::mapvalues(x[[col]], this_schema[[from]], this_schema[[to]])
  x
}


type_change <- function(type) {
  
  switch(type,
         "Beach Tournament Type" = tribble(~from, ~to, ~description,
                                           0L, "GrandSlam", "Grand slam",
                                           1L, "Open", "Open",
                                           2L, "Challenger", "Challenger",
                                           3L, "WorldSeries", "World series",
                                           4L, "WorldChamp", "World championship",
                                           5L, "OlympicGames", "Olympic games",
                                           6L, "Satellite", "Satellite",
                                           7L, "ContinentalChamp", "Continental championship",
                                           8L, "OtherContinental", "Other continental",
                                           9L, "Other", "Other",
                                           10L, "Masters", "CEV Masters",
                                           11L, "ContinentalCup", "Continental cup",
                                           12L, "ContinentalTour", "Continental tour",
                                           13L, "JuniorWorldChamp", "Junior world championship",
                                           14L, "YouthWorldChamp", "Youth world championship",
                                           15L, "NationalTour", "National tour",
                                           16L, "NationalTourU23", "National tour (under 23 years)",
                                           17L, "NationalTourU21", "National tour (under 21 years)",
                                           18L, "NationalTourU19", "National tour (under 19 years)",
                                           19L, "NationalTourU20", "National tour (under 20 years)",
                                           20L, "NationalTourU17", "National tour (under 17 years)",
                                           21L, "NationalTourU15", "National tour (under 15 years)",
                                           22L, "ContinentalChampU22", "Continental championship (under 22 years)",
                                           23L, "ContinentalChampU20", "Continental championship (under 20 years)",
                                           24L, "ContinentalChampU18", "Continental championship (under 18 years)",
                                           25L, "WorldChampU23", "World championship (under 23 years)",
                                           26L, "WorldChampU21", "World championship (under 21 years)",
                                           27L, "WorldChampU19", "World championship (under 19 years)",
                                           28L, "NationalTourU14", "National tour (under 14 years)",
                                           29L, "NationalTourU16", "National tour (under 16 years)",
                                           30L, "NationalTourU18", "National tour (under 18 years)",
                                           31L, "WorldChampU17", "World championship (under 17 years)",
                                           32L, "MajorSeries", "Major Series",
                                           33L, "WorldTourFinals", "World Tour Finals",
                                           34L, "ZonalTour", "Zonal Tour",
                                           35L, "Test", "Test",
                                           36L, "SnowVolleyball", "Snow Volleyball",
                                           37L, "ContinentalCupFinal", "Continental Cup Final",
                                           38L, "WorldTour5Star", "World Tour 5*",
                                           39L, "WorldTour4Star", "World Tour 4*",
                                           40L, "WorldTour3Star", "World Tour 3*",
                                           41L, "WorldTour2Star", "World Tour 2*",
                                           42L, "WorldTour1Star", "World Tour 1*",
                                           43L, "YouthOlympicGames", "Youth Olympic Games",
                                           44L, "MultiSports", "Multiple sports",
                                           45L, "NationalSnow", "National snow volleyball",
                                           46L, "NationalTourU22", "National Tour (under 22 years)",
                                           47L, "ContinentalChampU21", "Continental championship (under 21 years)",
                                           48L, "ContinentalChampU19", "Continental championship (under 19 years)",
                                           49L, "OlympicGamesQualification", "Qualification tournament for Olympic Games",
                                           50L, "KingOfTheCourt", "King of the Court"),
         stop("unexpected schema type: ", type)
         )
}


save(remap, file = 'remap.rda')
save(type_change, file = 'type_change.rda')