library("shiny")

##run local version
runApp("~/whatcheer_posts/armenian_diaspora/shiny_app/")

##publish to shiny.io
library("rsconnect")

rsconnect::setAccountInfo(
  name='ktra', 
  token='8BAE8D5B4AF59F21F2FF3C1BE8A5A6C3', 
  secret='qYxQnfpkOcDAVEoHCn7Hhy2RHtUWxKlcW40y+Yh6')

rsconnect::deployApp('~/whatcheer_posts/armenian_diaspora/shiny_app/')

