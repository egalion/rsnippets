rm(list = ls())
setwd("/home/alex29/Плот/mydata/")
library(sp)
library(maptools)

bgshape <- readShapeSpatial("BGR_adm1.shp", proj4string = CRS("+init=epsg:4326"))

plot(bgshape)

# The proj4string value here gives the correct projection for
# Bulgaria (and others). Otherwise the graph will be skewed.

# If we want to change the projection we could use

proj4string(bgshape) <- NA_character_
proj4string(bgshape) <- "+proj=longlat +datum=WGS84"

# That value also works correctly for Bulgaria and should
# in principle work for most countries

# We can transform to another spatial object by changing
# the projection.

bgmap <- spTransform(bgshape, CRS("+init=epsg:4326"))

plot(bgmap)
names(bgmap)
spplot(bgmap, "NAME_1", col.regions = gray(5:1/6))

# Alternatively we can use the rgdal library to load
# the shape file, which loads automatically the layers
# and the correct projection.

library(rgdal)
bgdal <- readOGR(dsn=".", layer="BGR_adm1")
plot(bgdal)

# We can save the map as an object

saveRDS(object = bgdal, file = "bgoblasti.Rds")

# We can then use readRDS, which is from the base package

oblasti_map <- readRDS("bgoblasti.Rds")
plot(oblasti_map)
summary(oblasti_map)

# We can also use subsetting and plot only certain districts

names(oblasti_map)
oblasti_map$NAME_1
plot(oblasti_map[oblasti_map$NAME_1 %in% c("Ruse", "Dobrich", "Razgrad", "Shumen", "Silistra"), ])
sbmap <- subset(oblasti_map, oblasti_map$NAME_1 %in% c("Ruse", "Razgrad", "Shumen"))

library(tmap)

tm_shape(sbmap) + tm_polygons() # requires library(tmap)

# Now add some data

# Load the code table for Bulgarian and Latin names

oblasti_kod <- read.csv("bgoblasti.csv")
head(oblasti_kod)
oblasti_kod$latinname

# replace "Grad Sofiya" with "Sofia (stolitsa)"

which(grepl("Sofiya", oblasti_map$NAME_1))
levels(oblasti_map$NAME_1)[5] <- "Sofia (stolitsa)"
oblasti_map$NAME_1

# read the file with the employment data

kz <- read.csv("zaetost-koeficient-regioni-godishni-2008-2014.csv")
head(kz)

# mix the codes file with the data file

kz_mix <- merge(kz, oblasti_kod, by.x = "region", by.y = "bgname")

# Get data only for 2008 and 2012 and for both men and women ("obshto")

kz_mix_2008 <- subset(kz_mix, godina == 2008 & grupa == "obshto")

kz_mix_2012 <- subset(kz_mix, godina == 2012 & grupa == "obshto")

tmp_df <- oblasti_map

# Check where the values match

match(oblasti_map$NAME_1, kz_mix_2012$latinname)

tmp_df$kz2012 <- kz_mix_2012$koef_zaetost[match(oblasti_map$NAME_1, kz_mix_2012$latinname)]
tmp_df$kz2008 <- kz_mix_2008$koef_zaetost[match(oblasti_map$NAME_1, kz_mix_2008$latinname)]

# Choose a palette

library(RColorBrewer)

# List all palettes

display.brewer.all()

# Split the numeric values of employment into 8 factor levels

tmp_df$kz2008 <- as.factor(as.numeric(cut(tmp_df$kz2008, breaks = c(0, 53, 56, 59, 62, 65, 100))))
tmp_df$kz2012 <- as.factor(as.numeric(cut(tmp_df$kz2012, breaks = c(0, 53, 56, 59, 62, 65, 100))))

# Give readable names to the levels

levels(tmp_df$kz2008) # 6 levels 
levels(tmp_df$kz2012) # 6 levels

# The levels must match if we want to plot them on the same graph

levels(tmp_df$kz2008) <- c("под 53%", "53-56%", "56-59%", "59-62%", "62-65%", "над 65%")
levels(tmp_df$kz2012) <- c("под 53%", "53-56%", "56-59%", "59-62%", "62-65%", "над 65%")

# Plot each graph separately, using Yellow-Green palette

spplot(tmp_df, "kz2008", col.regions = brewer.pal(8, "YlGn"))
spplot(tmp_df, "kz2012", col.regions = brewer.pal(8, "YlGn"))

summary(tmp_df$kz2008)
summary(tmp_df$kz2012)

summary(kz_mix_2008$koef_zaetost)
summary(kz_mix_2012$koef_zaetost)

tmp_df$заетост_2012 <- tmp_df$kz2012
tmp_df$заетост_2008 <- tmp_df$kz2008

spplot(tmp_df, c("заетост_2008", "заетост_2012"), 
       names.attr = c("2008 г.", "2012 г."),
       colorkey=list(space="bottom", width = 1, height = 1),
       # scales = list(draw = TRUE), # shows geo coordinates
       col.regions = brewer.pal(8, "YlGn"), 
       as.table = TRUE,
       main = "Коефициент на заетост \nна лицата на възраст 15-64 години")


# If you want to use text labels on the districts

# This prepares the code
sp.label <- function(x, label) {list("sp.text", coordinates(x), label, cex=0.5, col="black")}
NUMB.sp.label <- function(x) {sp.label(x, as.vector(x@data$NAME_1))}
make.NUMB.sp.label <- function(x) {do.call("list", NUMB.sp.label(x))}
tps <- list(fontsize=list(text=12), fontcolor=list(text="black"))
trellis.par.set(tps)

spplot(tmp_df, c("заетост_2008", "заетост_2012"), 
       names.attr = c("2008 г.", "2012 г."),
       colorkey=list(space="bottom", width = 1, height = 1),
       # scales = list(draw = TRUE), # shows geo coordinates
       col.regions = brewer.pal(8, "YlGn"), 
       as.table = TRUE,
       sp.layout = make.NUMB.sp.label(tmp_df), # adds the text labels
       main = "Коефициент на заетост \nна лицата на възраст 15-64 години")

# Alternatively

sp.label <- function(x, label) {
  list("sp.text", coordinates(x), label)
}

ISO.sp.label <- function(x) {
  sp.label(x, x[["NAME_1"]])
}

make.ISO.sp.label <- function(x) {
  do.call("list", ISO.sp.label(x))
}

spplot(tmp_df['mz2008'], sp.layout = make.ISO.sp.label(tmp_df), col.regions = brewer.pal(5, "YlGn"))

# Use tmap

tm_shape(tmp_df) + 
  tm_fill(c("kz2008", "kz2012"), palette = "YlGn", title = c("2008 г.", "2012 г.")) + 
  tm_borders() + 
  tm_layout(legend.title.size = 0.9, legend.position = c(0.75, 0.05), inner.margins = c(0.05,0.05,0.05, 0.2)) + tm_credits("Коефициент на \nзаетост (15-64 г.)", size = 0.9, position = c(0.75, 0.55))# + tm_text("NAME_1")

# Make a slopegraph to show the changes in the employemnt coefficient. 

library(slopegraph)

# But first prepare the dataset.

kz_mix_obshto <- subset(kz_mix, godina %in% c(2008,2012) & grupa == "obshto", select = c("region", "koef_zaetost", "godina"))
head(kz_mix_obshto)
kz_mix_obshto
kz_mix_obshto <- kz_mix_obshto[!grepl("Южен", kz_mix_obshto$region), ]
kz_mix_obshto <- kz_mix_obshto[!grepl("Юго", kz_mix_obshto$region), ]
kz_mix_obshto <- kz_mix_obshto[!grepl("Север", kz_mix_obshto$region), ]

library(tidyr)
kz_mix_table <- spread(kz_mix_obshto, godina, koef_zaetost)
rownames(kz_mix_table) <- kz_mix_table$region
kz_mix_table$region <- NULL

library(extrafont)
?extrafont::choose_font
fonts()
loadfonts()

cairo_pdf("mypdffile.pdf", height = 24, width = 12, family = "Ubuntu Condensed")
slopegraph(kz_mix_table, main = "Изменение в коефициента на заетост (15-64 г.)")
dev.off()

colnames(kz_mix_table) <- c("x2008", "x2012")
kz_change <- data.frame(rownames(kz_mix_table), kz_mix_table$x2012-kz_mix_table$x2008)
names(kz_change) <- c("oblast", "promqna")
kz_change
kz_change[order(kz_change$promqna), ]

# Now return to tmap. We will add shapes to the graph, showing
# the minimum wage as percentage of the average wage.

zaplata_df <- read.csv("zaplata-regioni-godishni-2000-2013.csv")
head(zaplata_df)

str(oblasti_kod)
zaplata_df <- merge(zaplata_df, oblasti_kod, by.x = "region", by.y = "bgname")
zaplata2008 <- zaplata_df[zaplata_df$godina == 2008, ]
zaplata2012 <- zaplata_df[zaplata_df$godina == 2012, ]

oblasti_kod

nrow(zaplata2008)
zaplata2008


zaplata2008$minzaplata <- rep(220, nrow(zaplata2008))
zaplata2012$minzaplata <- rep(290, nrow(zaplata2012))

zaplata2008 <- transform(zaplata2008, mzproc = (minzaplata/(godishna_zaplata/12))*100)
zaplata2012 <- transform(zaplata2012, mzproc = (minzaplata/(godishna_zaplata/12))*100)

summary(zaplata2008$mzproc)
summary(zaplata2012$mzproc)

nrow(zaplata2008)

tmp_df$zaplata2008 <- zaplata2008$mzproc[match(oblasti_map$NAME_1, zaplata2008$latinname)]
tmp_df$zaplata2012 <- zaplata2012$mzproc[match(oblasti_map$NAME_1, zaplata2012$latinname)]

tmp_df$mz2008 <- as.factor(as.numeric(cut(tmp_df$zaplata2008, breaks = c(0,40,45,50,55,100))))
tmp_df$mz2012 <- as.factor(as.numeric(cut(tmp_df$zaplata2012, breaks = c(0,40,45,50,55,100))))

levels(tmp_df$mz2008) <- c("под 40%", "40-45%", "45-50%", "50-55%", "над 55%")
levels(tmp_df$mz2012) <- c("под 40%", "40-45%", "45-50%", "50-55%", "над 55%")

tm_shape(tmp_df) + 
  tm_fill(c("kz2008", "kz2012"), palette = "YlGn", title = c("Заети", "Заети")) + tm_borders() +
  tm_shape(tmp_df) + 
  tm_bubbles(c("zaplata2008", "zaplata2012"), border.col = "black",
             title.col = "МРЗ спрямо средната", 
             title.size =  "", sizes.legend = 0, sizes.legend.labels = " ",
             #size.lim = c(20, 21),
             col = c("mz2008", "mz2012"), palette = "YlOrRd") +
  tm_layout(title = c("2008 г.", "2012 г."), inner.margins = c(0.05, 0.05, 0.1, 0.27), 
            legend.title.size = 0.7,
            legend.position = c(0.75, 0.05),
            title.position =c(0.25, 0.9))

table(tmp_df$mz2008)
table(tmp_df$mz2012)

# Make a composite plot

# see ?print.trellis for more details

p1 <- spplot(tmp_df, c("заетост_2008", "заетост_2012"), 
       names.attr = c("2008 г.", "2012 г."),
       colorkey=list(space="bottom", width = 1, height = 1),
       # scales = list(draw = TRUE), # shows geo coordinates
       col.regions = brewer.pal(8, "YlGn"), 
       as.table = TRUE,
       main = "Коефициент на заетост \nна лицата на възраст 15-64 години")

p2 <- spplot(tmp_df, c("mz2008", "mz2012"), 
       names.attr = c("2008 г.", "2012 г."),
       colorkey=list(space="bottom", width = 1, height = 1),
       # scales = list(draw = TRUE), # shows geo coordinates
       col.regions = brewer.pal(5, "YlOrRd"), 
       as.table = TRUE,
       main = "Минимална работна заплата \nкато дял от средната")

# position gets arguments c(xmin, ymin, xmax, ymax)

png("proben.png", width = 800, height = 600)

print(p1, position = c(0, 0, 0.5, 1),more=T)
print(p2, position = c(.5, 0,1,1),more = T)

dev.off()

# Use ggplot2

library(ggplot2)

obl_df <- fortify(oblasti_map, region = "NAME_1")
head(obl_df)
unique(obl_df$id)
head(kz_mix)
zaetost_obshto <- kz_mix[kz_mix$grupa == "obshto", ]
head(zaetost_obshto)
zaetost <- zaetost_obshto[oblasti_map$NAME_1 %in% zaetost_obshto$latinname, ]
head(zaetost)

# We need this for the text labels

# cnames <-aggregate(cbind(long, lat) ~ id, data = obl_df, FUN = function(x) mean(range(x)))
# but it doesn't work

ggplot() + geom_map(data = zaetost, aes(map_id = latinname, fill = koef_zaetost), map = obl_df) + 
  expand_limits(x = obl_df$long, y = obl_df$lat) +
  scale_fill_gradient2(low = "white", mid = "white", high = "darkgreen", midpoint = 45, 
                       name = "Коефициент на \nзаетост (%)\n", limits = c(40, 75)) +
  facet_wrap(~ godina) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
  


