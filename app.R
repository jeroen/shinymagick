# Minimal example of Shiny widget using 'magick' images
ui <- fluidPage(
  titlePanel("Magick Shiny Demo"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
      textInput("size", "Size", value = "500x500!"),
      sliderInput("rotation", "Rotation", 0, 360, 0),
      sliderInput("blur", "Blur", 0, 20, 0),
      sliderInput("implode", "Implode", -1, 1, 0, step = 0.01),
      
      checkboxGroupInput("effects", "Effects",
                         choices = list("edge", "charcoal", "negate", "flip", "flop"))
    ),
    mainPanel(
      imageOutput("img")
    )
  )
)

server <- function(input, output, session) {
  
  library(magick)
  
  # Start with placeholder img
  image <- image_read("https://raw.githubusercontent.com/ThinkR-open/collage/master/inst/tigrou/tigrou.jpg")
  
  # When uploading new image
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      image <<- image_convert(image_read(input$upload$datapath), "jpeg")
    info <- image_info(image)
    updateCheckboxGroupInput(session, "effects", selected = "")
    updateTextInput(session, "size", value = paste0(info$width, "x", info$height, "!"))
  })
  
  # A plot of fixed size
  output$img <- renderImage({
    
    # Boolean operators
    if("edge" %in% input$effects)
      image <- image_edge(image)
    
    if("charcoal" %in% input$effects)
      image <- image_charcoal(image)
    
    if("negate" %in% input$effects)
      image <- image_negate(image)    
    
    if("flip" %in% input$effects)
      image <- image_flip(image)
    
    if("flop" %in% input$effects)
      image <- image_flop(image)
    
    # Numeric operators
    tmpfile <- image %>%
      image_resize(input$size) %>%
      image_implode(input$implode) %>%
      image_blur(input$blur, input$blur) %>%
      image_rotate(input$rotation) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
}

shinyApp(ui, server)
