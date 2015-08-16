
# draw a function
curve(2*x+5, xlim = c(0,100), ylim = c(0,100),
      main = "Графика - икономика",
      sub = "за студентите", 
      xlab = "Количество", ylab = "Цена", 
      family = "Times", 
      cex.main = 1.5, cex.lab = 1, 
      cex.sub = 1.2)
curve(1/20*x^2 + 5, add = T)
# The minus sign in front turns the function from U-shaped
# to bell curve. 1/2 in front of x and ()/10 changes the 
# width. The -30 parameter moves the vertex left and right
# and + 60 moves it vertically.
curve((-(1/2*x-30)^2)/10 + 60, add = T)
# draw two horizontal lines at heights 10 and 30
abline(h = c(10, 30))
# draw an arrow from point A(20, 10) to point B(30, 25)
# angle and length control the arrow head
arrows(20, 10, 30, 25, angle = 20, length = 0.15, col = "darkblue")
# draw two arrows from 10 to 30 vertical, at 10 and 90
arrows(c(10, 90), c(10, 10), c(10, 90), c(30, 30), 
       angle = 30, length = 0.15, col = "orange",
       lty = 5, lwd = 2)
# Draw a segmented line
segments(60, 50, 90, 50, col = "darkblue", lty = 3, lwd = 2)
# Draw a rectangle with point A(30, 10) and B(50,30)
rect(30, 10, 50, 30, col = "red", density = 20, border = "transparent")
rect(20, 10, 60, 20, col = "darkgreen", density = 20, 
     angle = -45, border = "transparent")
# The last parameter of rgb is transparency
rect(0, 0, 80, 10, col = rgb(1,0,0, 0.3), border = NA)
# Another way to use transparency is 
# through adjustcolor
rect(80, 0, 100, 10, col = adjustcolor("darkgreen", alpha.f = 0.3), border = NA)

# We can add text

# and change the font - see fc-list for available fonts
text(90, 35, "Цена", family = "UbuntuCondensed")
# We can change the size of the text with cex
text(60, 35, "Цена", cex = 0.5)
# And the rotation with srt
text(50, 80, "Цена", cex = 0.7, srt = 75)
# Start polygon
# You need to give the coordinates of each point
# That is why we make a vector xx
# which consists of two parts - 
# the first is when x increases
# and then when it decreases
# and the corresponding y values go to yy
xc1 <- seq(0,40, by =1)
xc2 <- seq(40,0, by = -1) # or use rev(xc1)
xx <- c(xc1, xc2)
yy <-c(2*xc1+5, 1/20*xc2^2+5) 
polygon(xx, yy, col = "orange")

# Create a layout with three plots
# One on top of the other two
# The matrix says that we should use plot 1 in the first
# two quadrants. The height of the first will be 3 times
# the size of the others. We can also use the width parameter
# but for this we must use width, instead of height.

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=T), height=c(2,1))
par(mar = c(2,2,2,1))

# plot 1 - equilibrium
# this creates empty plot with limits from 0 to 10 and doesn't 
# draw any axis. We add them later with the axis parameter, 
# which also adds tick with the at parameter.
par(xpd = TRUE) # This allows me to put 
# text labels outside the axis
plot(0, xlim = c(0,10), ylim = c(0,10), main = "Равновесие", type = "n",
     xaxt = "n", yaxt = "n")
axis(side = 1, at = seq(1,10,1))
axis(side = 2, at = seq(1,10,2))
curve(7-x, add = T, clip(1,9,1,9))
curve(2+1/2*x, add = T, clip(1,9,1,9))
abline(h = 11/3, lty = 3, clip(0,10,0,10))
text(1.5, 6.5, "D", cex = 0.5)
text(9, 7, "S", cex = 0.5)
mtext(text = "Количество", side = 1, adj = 1, cex = 0.5)
mtext(text = "Цена", side = 2, adj = 1, cex = 0.5, line = 2)
# side is - 1 for x axis, 2 - for y axis. At is the position on 
# the axis. Line shows how close to be to the axis.
text(-2, 11/3, "P = 11/3")

plot(0, xlim = c(0,10), ylim = c(0,10), main = "Излишък", type = "n")
curve(7-x, add = T, clip(1,9,1,9))
curve(2+1/2*x, add = T, clip(1,9,1,9))
abline(h = 2, lty = 3, clip(0,10,0,10))
text(1.5, 6.5, "D", cex = 0.5)
text(9, 7, "S", cex = 0.5)
mtext(text = "Количество", side = 1, adj = 1, cex = 0.5)
mtext(text = "Цена", side = 2, adj = 1, cex = 0.5)


plot(0, xlim = c(0,10), ylim = c(0,10), main = "Дефицит", type = "n")
curve(7-x, add = T, clip(1,9,1,9))
curve(2+1/2*x, add = T, clip(1,9,1,9))
abline(h = 4.5, lty = 3, clip(0,10,0,10))
text(1.5, 6.5, "D", cex = 0.5)
text(9, 7, "S", cex = 0.5)
mtext(text = "Количество", side = 1, adj = 1, cex = 0.5)
mtext(text = "Цена", side = 2, adj = 1, cex = 0.5)

dev.off() # to reset the layout

# This option options(scipen = 3) is very important. Without it the 
# graph could sometimes have on the axis 1e+05, etc. To restore
# the default use options(scipen = 0). Also, scipen = 10 will work.


# Note: for ggplot we can first load the library scales
# library(scales). Then add
# + scale_x_continuous(labels = comma)

# We also remove the axes with "axes = F" and add them
# later with axis(1, pos = 0) and axis(2, pos = 0)

# Note: if we want our axes to start at 0, we use the options
# xaxs = "i", yaxs = "i"

# Note: if we want to do it in ggplot, we add
# + scale_x_continuous(expand=c(0,0))

manipulate(plot(1:x), x = slider(5, 10))
data(mtcars)
manipulate(
  plot(cars, xlim = c(x.min, x.max), type = type,
       axes = axes, ann = label),
  x.min = slider(0,15),
  x.max = slider(15,30, initial = 25),
  type = picker("p", "l", "b", "c", "o", "h", "s", "S", "n"),
  axes = checkbox(TRUE, "Draw Axes"),
  label = checkbox(FALSE, "Draw Labels"))
