site.list <- function(s) {
  
  ## all sites
  if(s=='total') {
    years <- c(2003:2014)
    sites <- list('L21'=years,
                  'L24'=years,
                  'L33'=years,
                  'L35'=years,
                  'P24'=years,
                  'P310'=years,
                  'P33'=years,
                  'P37'=years,
                  'P43'=years,
                  'W04'=years,
                  'N131'=years,
                  'N132'=years,
                  'N133'=years,
                  'N134'=years,
                  'N135'=years,
                  'N136'=years,
                  'N137'=years,
                  'N139'=years
                  )
  }
  list(sites=sites)
}
