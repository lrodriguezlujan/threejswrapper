
#' @noRd
#' @export
runShinyExample <- function(example = c("spheres",
                                         "cylinders",
                                         "torus",
                                         "mesh",
                                         "parametric")) {

  # Check for shiny
  if (!requireNamespace("shiny", quietly = F))
    stop("Shiny library required")

  example <- match.arg(example)
  switch(example,
    spheres = spheresShinyExample(),
    cylinders = cylindersShinyExample(),
    torus = torusShinyExample(),
    mesh = meshShinyExample(),
    parametric = parametricShinyExample(),
    stop("Unknown example name")
  )
}

# Spheres example
spheresShinyExample <- function() {


  shiny::shinyApp(
    ui =
      shiny::fluidPage(
        shiny::titlePanel("Spheres"),

        shiny::sidebarLayout(
          shiny::sidebarPanel(shiny::p("Sphere geometry test")),
          shiny::mainPanel(sceneOutput("spheres"))
        )
      ),
    server = function(input, output) {
      output$spheres <- renderScene({
        colors <-
          c("blue","red","green","yellow","orange","purple","white","pink")

        # Ambient light + directional light
        light <-
          list(
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(1,1,0)
            ),
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(0,0,1)
            )
          )

        # Create random spheres
        nspheres <- rpois(1,100)
        objs <- lapply(1:nspheres, function(x) {
          pos <- runif(3,min = 0,max = 10)
          rad <- rgamma(1,shape = 1) / 3
          color <- sample(colors,1)

          geo <- sphereGeometry(rad, position = pos)
          return(objThreeJS(
            sphereGeometry(rad, position = pos), lambertMaterial(color = color)
          ))

        })
        scene(
          objects = objs , lights = light, camera.pos = c(10,10,10)
        )
      })

    }
  )
}

# Torus
torusShinyExample <- function() {

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Trous"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::p("Torus geometry test")),
        shiny::mainPanel(sceneOutput("torus"))
      )
    ),
    server = function(input, output) {
      output$torus <- renderScene({
        colors <-
          c("blue","red","green","yellow","orange","purple","white","pink")

        # Ambient light + directional light
        light <-
          list(
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(1,1,0)
            ),
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(0,0,1)
            )
          )

        # Create random torus
        ntors <- rpois(1,50)
        objs <- lapply(1:ntors, function(x) {
          pos <- runif(3,min = 0,max = 10)
          rot <- runif(3,min = 0,max = 2 * pi)
          init_rad <- rgamma(1,shape = 1) / 2
          end_rad <- init_rad + rgamma(1,shape = 1) / 4
          color <- sample(colors,1)

          return(objThreeJS(
            torusGeometry(inner = init_rad, outer = end_rad, position = pos, rotation = rot),
            lambertMaterial(color = color)
          ))
        })
        scene(
          objects = objs , lights = light, camera.pos = c(10,10,10)
        )
      })
    }
  )
}

# Cylinders example
cylindersShinyExample <- function() {

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Cylinders"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::p("Cylinder geometry test")),
        shiny::mainPanel(sceneOutput("cylinders"))
      )
    ),
    server = function(input, output) {
      output$cylinders <- renderScene({
        colors <-
          c("blue","red","green","yellow","orange","purple","white","pink")

        # Ambient light + directional light
        light <-
          list(
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(1,1,0)
            ),
            directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(0,0,1)
            )
          )

        # Create random cylinders
        ncyl <- rpois(1,100)
        objs <- lapply(1:ncyl, function(x) {
          initPos <- runif(3,min = 0,max = 10)
          orientation <- runif(3,min = 0,max = 1)
          height <- rgamma(1,shape = 1)
          init_rad <- rgamma(1,shape = 1) / 5
          end_rad <- rgamma(1,shape = 1) / 5
          color <- sample(colors,1)

          return(objThreeJS(
            cylinderGeometry(
              initPoint = initPos, height = height, orientation = orientation,
              initRad = init_rad, endRad = end_rad
            ),
            lambertMaterial(color = color)
          ))
        })
        scene(
          objects = objs , lights = light, camera.pos = c(10,10,10)
        )
      })
    }
  )
}

# Mesh example
meshShinyExample <- function() {

  # Check for rvcg
  if (!requireNamespace("Rvcg", quietly = T))
    stop("Rvcg library required")

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Jimmy"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::p("Mesh geometry test")),
        shiny::mainPanel(sceneOutput("monkey"))
      )
    ),
    server = function(input, output) {
      output$monkey <- renderScene({
        # Ambient light + directional light
        light <-
          list(
            ambientLight("#555555"), directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(0,0,1)
            )
          )

        # Object
        obj.helper <- boundingBox()
        mesh <-
          Rvcg::vcgImport(system.file(
            file.path("examples","monkey.ply"), package = "threejswrapper"
          ))
        geos <-
          meshGeometry(t(mesh$vb[1:3,]) , t(mesh$it), compute.fn = T)
        obj <-
          objThreeJS(geos, lambertMaterial(color = "#FFFFFF"), helpers = obj.helper)
        helpers <- gridHelper(center = c(0,-1,0))
        scene(
          objects = obj , lights = light, helpers = helpers, bgColor = "#333333", camera.pos = c(3,3,3)
        )
      })

    }
  )
}

# Parametric
parametricShinyExample <- function() {

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Parametric"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::p("Parametric geometry test")),
        shiny::mainPanel(sceneOutput("parametric", height = 800))
      )
    ),
    server = function(input, output) {
      output$parametric <- renderScene({
        # Ambient light + directional light
        light <-
          list(
            ambientLight("#555555"), directionalLight(
              "#FFFFFF",intensity = 0.5,from = c(0,0,1)
            )
          )

        # Colorscale
        colorscale <-
          colorscaleThreeJS(
            max = 1, min = -1, legend.on = T, legend.labels.on = T,
            legend.labels.title = 'EXAMPLE'
          )

        # Object
        geos <- parametricGeometry(
          htmlwidgets::JS(
            "function(u,v){
            u -= 0.5;
            u *= 2;
            v *= (2 * 3.1415);
            return( new THREE.Vector3( u * Math.cos(v) , u * Math.sin(v) , u ) ); }"
          ),
          vertex.color.fun = htmlwidgets::JS("function(v,g,i,e,cs){ return(cs.getColor( v.z )) }")
        );

        obj <-
          objThreeJS(
            geos, lambertMaterial(
              color = "#AAAAAA",wireframe = F,
              vertexColors = htmlwidgets::JS("THREE.VertexColors"),
              side = htmlwidgets::JS("THREE.DoubleSide")
            )
          )
        scene(
          objects = obj , lights = light, colorscale = colorscale , camera.pos = c(3,3,3)
        )
      })
    }
  )
}
