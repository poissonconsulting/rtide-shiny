function(input, output, session) {
  
  callModule(about, 'about')
  
  callSearch <- callModule(search, 'search')
  
  callModule(map, "map", callSearch)
  
}


