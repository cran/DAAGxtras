.First.lib <-
function (...)  {
  indaag <- c('bomregions', 'cps1', 'cps2', 'cps3', 'cricketer',
  'edcCO2', 'edcT', 'excessRisk', 'gaba', 'grog', 'hotspots',
  'hotspots2006', 'intersalt', 'nassCDS', 'nasshead', 'nihills',
  'nswdemo', 'nswpsid1', 'progression', 'psid1', 'psid2', 'psid3',
  'worldRecords')
  cat("\nThe following datasets that were formerly in the",
      "\nDAAGxtras package are now in the DAAG package:\n")
  print(indaag)
  cat("\nAdditionally, the function dataFile() has been removed from DAAGxtras")
  cat("\n(Use datafile(), in the DAAG package.)\n")
}

