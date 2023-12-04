# plotly es una librería de R para crear gráficos dinámicos 
# Cargar la librería plotly
library(plotly)

# Establecer una semilla para la reproducibilidad
set.seed(24)

# Definir el número de puntos para el árbol, adornos y luces
n_tree <- 1000
n_ornaments <- 50
n_lights <- 200

# Generate spiral data points
x <- c()
y <- c()
z <- c()

for (i in 1:n_tree) {
  r <- i / 30
  x <- c(x, r * cos(i / 30))
  y <- c(y, r * sin(i / 30))
  z <- c(z, n_tree - i)
}

tree <- data.frame(x, y, z)

# Sample for ornaments:
#   - sample n_ornaments points from the tree spiral
#   - modify z so that the ornaments are below the line
#   - color column: optional, add if you want to add color range to ornaments
ornaments <- tree[sample(nrow(tree), n_ornaments), ]
ornaments$z <- ornaments$z - 50
ornaments$color <- 1:nrow(ornaments)

# Sample for lights:
#   - sample n_lights points from the tree spiral
#   - Add normal noise to z so the lights spread out
lights <- tree[sample(nrow(tree), n_lights), ]
lights$x <- lights$x + rnorm(n_lights, 0, 20)
lights$y <- lights$y + rnorm(n_lights, 0, 20)
lights$z <- lights$z + rnorm(n_lights, 0, 20)

# hide axes
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

plot_ly() %>%
  add_trace(data = tree, x = ~x, y = ~y, z = ~z,
            type = "scatter3d", mode = "lines",
            line = list(color = "#1A8017", width = 7)) %>%
  add_markers(data = ornaments, x = ~x, y = ~y, z = ~z,
              type = "scatter3d",
              marker = list(color = ~color,
                            colorscale = list(c(0,'#EA4630'), c(1,'#CF140D')),
                            size = 12)) %>%
  add_markers(data = lights, x = ~x, y = ~y, z = ~z,
              type = "scatter3d",
              marker = list(color = "#FDBA1C", size = 3, opacity = 1)) %>%
  layout(scene = list(xaxis=ax, yaxis=ax, zaxis=ax), showlegend = FALSE)



