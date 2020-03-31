
# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

# Loading in the ggplot2 package
library(ggplot2)

library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Test that ggplot2 is loaded", {
    expect_true( "package:ggplot2" %in% search(), 
        info = "The ggplot2 package should be loaded using library().")
    })
})

t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point()

run_tests({
    test_that("Check that a geom_point plot was plotted.", {
    expect_true( "GeomPoint" %in% class( last_plot()$layers[[1]]$geom ) , 
        info = "Add geom_point() to produce a scatter plot.")
    })
})

# Defining the number of points
points <- 500

# Defining the Golden Angle
angle <- pi*(3 - sqrt(5))

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point()

run_tests({
    test_that("points are 500.", {
    expect_equal(points, 500, 
        info = "There should be 500 points.")
    })
    
    test_that("angle is golden.", {
    expect_equal(angle, pi*(3-sqrt(5)), 
        info = "angle should be set to the Golden Angel. Check the hint!")
    })
})

df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point() + theme(panel.background = element_rect(fill = 'white'),
                         panel.grid.major = element_blank(), 
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(), 
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank())

# Maybe just check if one of the this are blank, 
# seems too much to check them all...

run_tests({
    test_that("Background is white.", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "The background should be white.")
    })
    test_that("Grid is removed.", {
    expect_true("element_blank" %in% class(last_plot()$theme$panel.grid), 
        info = "The grid lines should be removed.")
    })
})

# Change the code from Task 4 to modify the 
# size, transparency, and color of the points
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(size = 8, alpha = 0.5, color = 'darkgreen') + theme(panel.background = element_rect(fill = 'white'),
                         panel.grid.major = element_blank(), 
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(), 
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank())

run_tests({
    test_that("Point size equal to 8.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$size, 8, 
        info = "size should be set 8.")
    })
    test_that("alpha equal to 0.5.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$alpha, 0.5, 
        info = "alpha should be set 0.5.")
    })
})

# Copy the code from Task 5 and modify the 
# color, size, and shape of the points
p <- ggplot(df, aes(x*t, y*t))
p + # .... YOUR CODE FOR TASK 6....

run_tests({
    test_that("Map size of points to t.", {
    expect_equal(last_plot()$labels$size, "t", 
        info = "Map size of points to t. Check the hint!")
    })
    test_that("point shape is asterisk.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$shape, 8, 
        info = "Change the shape of all points to asterisks.")
    })
    test_that("Legend is removed.", {
    expect_equal(last_plot()$theme$legend.position, "none", 
        info = "Remove the legend from the plot.")
    })
})

# Copy the code from Task 6 and modify the color and
# shape of the points, and the background color
p <- ggplot(df, aes(x*t, y*t))
p + # .... YOUR CODE FOR TASK 7....

run_tests({
    test_that("point shape is filled triangles.", {
        expect_equal(last_plot()$layers[[1]]$aes_params$shape, 17, 
                     info = "Change the shape of all points to filled triangles. Check the hint.")
    })
    test_that("The triangles are yellow", {
        expect_equal(last_plot()$layers[[1]]$aes_params$colour, "yellow", 
                     info = "The triangles are not yellow. Check the hint.")
    })
    test_that("The background is dark magenta", {
        expect_equal(last_plot()$theme$panel.background$fill, "darkmagenta", 
         info = "The background is not dark magenta. Check the hint.")
    })
})

# Change the value of the angle
angle <- ....
points <- 1000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Copy the plotting code from Task 7
# .... YOUR CODE FOR TASK 8 ....

run_tests({
    test_that("angle is 2.", {
    expect_equal(angle, 2, 
        info = "angle should be equal to 2")
    })
})

# Change the values of angle and points
angle <- ....
points <- ....

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Adjust the plot parameters to create the magenta flower
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(size = ...., alpha = ...., shape = ...., color = ....)+
  theme(legend.position="none",
        panel.background = element_rect(fill = ....),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

run_tests({
    test_that("points is equal to 2000.", {
    expect_equal(points, 2000, 
        info = "There should be 2000 points.")
    })
    test_that("point shape is empty circle.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$shape, 1, 
        info = "Change the shape of all points to empty circles. Check the hint!")
    })
    test_that("alpha is equal 0.1", {
    expect_equal(last_plot()$layers[[1]]$aes_params$alpha, 0.1, 
        info = "alpha of points should be 0.1")
    })
    test_that("Background is white.", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "The background should be white.")
    })
    test_that("angle is 13*pi/180.", {
    expect_equal(angle, 13*pi/180, 
        info = "angle should be set to 13*pi/180.")
    })
})
