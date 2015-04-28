library(swiTheme)
library(swiMap)
library(dplyr)
require(rgdal)
require(rgeos)


## data files ######

### FRONTALIER DATA
statf.csv <- "data/px-x-0302010000_101.csv"
## Extracted from stat-tab STATF, Q4 since 1996 until 2014


### OVERALL JOB BY COMMUNE
statent.csv <- "data/px-x-0602010000_102.csv"
## Extracted from stat-tab STATENT, 2012 https://www.pxweb.bfs.admin.ch/Selection.aspx?px_language=de&px_db=px-x-0602010000_102&px_tableid=px-x-0602010000_102%5Cpx-x-0602010000_102.px&px_type=PX

############################################################################################
###		Load data
############################################################################################


## 1 STATF
statf.read <- read.csv(statf.csv, stringsAsFactors = F, check.names = F, sep = "\t", encoding = "latin1")

## 1 b cleaning
# discard fist column
statf.read <- statf.read[,-1]
# get only communes values
idx.com <- grepl("^\\\u0085\\\u0085", statf.read[,1])
statf <- statf.read[idx.com,]
statf[,1]  <- gsub("^\\\u0085\\\u0085", "", statf[,1])
# rename column
colnames(statf)[1] <- 'Commune'


## 2 STATENT
statent.read <- read.csv(statent.csv, stringsAsFactors = F, check.names = F, sep = "\t", encoding = "latin1")

## 2 b cleaning: split the column commune with id code and commnune name into 2 columns
statent.read$bfsid <- as.numeric(substr(statent.read[,2], 1, 4))
statent.read[,2] <- substr(statent.read[,2], 6, 100)


## 3 MAP data load commune map shapefiles and transform it into a data.frame

ch.shp <- readOGR(swiMap::getPathShp('CH'), "municipalities-without-lakes")
ch.df <- formatShp(ch.shp)
lake.df <- formatShp(readOGR(swiMap::getPathShp('CH'), layer = "lakes"))

######### CHECK commune names match commune map names
# get names and id from map
bfsnName <- ch.df[!duplicated(ch.df$BFSNR),c('BFSNR', 'GEMNAME')]
bfsnName <- bfsnName[order(bfsnName[,1]),]


qf <- unique(statf[,1])
qe <- unique(statent.read[,2])
# get the index of statf communes not in statent
idxna.1 <- which(is.na(match(qf, qe)))

# get the communes historical mutation data from OFS
start <- as.Date("2012-01-01")
data <- loadCommunesCHdata()
mutations <- data[match(qf[idxna.1], data$GNAME),]
# get the communes merged
communes14to12 <- sapply(mutations$GINIMUT, function(id) {
	dd <- data[which(data$GFINMUT == id),]
	dd$GNAME
})
names(communes14to12) <- mutations$GNAME

## complete STATF DATA by creating communes in 2014 missing in 2012
statent <- statent.read

for(i in 1:length(communes14to12) ) {
	sumEmplois <- sum(statent.read [which(statent.read$Commune %in% communes14to12[[i]]),'Emplois'])
	statent <- rbind(statent, data.frame(Année = 2012, Commune = names(communes14to12)[i], Emplois = sumEmplois,
		bfsid = mutations[which(mutations$GNAME == names(communes14to12)[i]),'GBFSNR']))
}


############################################################################################
###		Compute
############################################################################################

# transform statf data to a matrix
rownames(statf) <- statf[,1]
statf <- as.matrix(statf[,-1])
# renames column names
colnames(statf) <- gsub(" T4", "", colnames(statf))


# create matrix of the % frontaliers
statf.pc <- (statf /statent[match(rownames(statf), statent$Commune), 'Emplois']) * 100

############################################################################################
###		Map
############################################################################################
breaks <- c(-1,0, 1, 2.5, 5, 7.5, 10, 15, 20, 25, 30, Inf)
idxM <- match(ch.df$GEMNAME, rownames(statf.pc))


colors <- colorRampPalette(c(swi_spal[1], swi_spal[6]))(length(breaks))
colors <- colorRampPalette(c(swi_spal[1], "#663333"))(length(breaks))

# plots <- lapply(as.character(seq(1996, 2014, 3)), function(year) {
# 	df <- ch.df
# 	bin <- cut(statf.pc[,year], breaks)
# 	df$bin <- bin[idxM]
# 	gp <- ggplot(df, aes(x = long, y = lat, group = group)) +
# 		geom_polygon(colour = alpha("black", 0.5), size = 0.01, aes(fill = bin)) + scale_fill_manual(values = colors) + ggtitle(year) +
# 		geom_polygon(data = lake.df, fill = alpha("#366096", alpha = 1), colour = alpha("#366096", alpha = 0.5)) + theme_swiMin() + theme(aspect.ratio = 0.8)
# 	gp
# })

df.y <- do.call(rbind, lapply(as.character(seq(1996, 2014, 6)), function(year) {
	df <- ch.df
	bin <- cut(statf.pc[,year], breaks)
	df$bin <- bin[idxM]
	df$pc <- statf.pc[,year][idxM]
	df$abs <- statf[,year][idxM]
	df$year <- year
	df
}))
map <- ggplot(df.y, aes(x = long, y = lat, group = group)) + geom_polygon(colour = alpha("black", 0.5), size = 0.01, aes(fill = bin)) +
	scale_fill_manual(values = colors) +
	geom_polygon(data = lake.df, fill = alpha("#366096", alpha = 1), colour = alpha("#366096", alpha = 0.5)) +
	theme_swiMin() + theme(aspect.ratio = 0.8, legend.key.size = unit(0.5, "lines"), legend.position = "bottom") + facet_wrap(~ year, ncol = 1)
pdfswi_long("mapFrontalier_l.pdf")
map
dev.off()

map
ggsave(file="mapFrontalier_long.svg", width=6, height=8)



############################################################################################
###		Interactive Map
############################################################################################
library(leaflet)
library(htmlwidgets)

ogrInfo(swiMap::getPathShp('CH'),  "municipalities-without-lakes")

#plot(ch.shp)

# Transform to WGS 1984, EPSG:4326 (same as leaflet)
ch <- spTransform(ch.shp, CRS("+init=epsg:4326"))
plot(ch)


str(ch@data)
idxM <- match(ch$GEMNAME, rownames(statf.pc))
ch@data$pc <- round(statf.pc[,"2014"][idxM], 2)
ch@data$logpc <- asinh(statf.pc[,"2014"][idxM])
ch@data$abs <- statf[,"2014"][idxM]

probs <- c(0, 0.35, 0.575, 0.757, 0.86, 0.85, 0.9, 0.95, 0.99, 1)
pal <- colorQuantile(colorRamp(c("#DAE0E0", "#405C5C")), NULL, probs = probs)
state_popup <- paste0("<strong>Commune: </strong>",
                      ch$GEMNAME,
                      "<br><strong>Frontalier en 2014: </strong>",
                      ch$abs,
					   "<br><strong>% des emplois frontalier: </strong>",
					  ch$pc, " %")

mb_tiles <- "http://a.tiles.mapbox.com/v3/kwalkertcu.l1fc0hab/{z}/{x}/{y}.png"
mb_attribution <- 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a>'

m <- leaflet(data = ch) %>% addTiles(urlTemplate = mb_tiles,
	attribution = mb_attribution) %>% setView(8.227512, 46.818188, zoom = 9)
map <- m %>% addPolygons(fillColor = ~pal(logpc), fillOpacity = 0.9, color = "#ECEFEF", weight = 1, popup = state_popup)

m <- leaflet(data = ch) %>% setView(8.227512, 46.818188, zoom = 9)
map <- m %>% addPolygons(fillColor = ~pal(logpc), fillOpacity = 0.9, color = "#ECEFEF", weight = 1, popup = state_popup)



saveWidget(map, file="frontalierMap.html")


