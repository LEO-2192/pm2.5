library(dplyr)
library(magrittr)
library(ggplot2)


pm0 <- read.table("RD_501_88101_1999-0.txt",na.strings = "",comment.char = "#",header = TRUE,sep = "|",stringsAsFactors = FALSE)
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames,"|",fixed = TRUE)
names(pm0) <- make.names(cnames[[1]])


pm12 <- read.table("RD_501_88101_2012-0.txt",header = TRUE,comment.char = "#",na.strings = "",sep = "|")

cname <- readLines("RD_501_88101_2012-0.txt",1)
cname <- strsplit(cname,"|",fixed = TRUE)
names(pm12) <- make.names(cname[[1]])

sv0 <- pm0$Sample.Value
sv12 <- pm12$Sample.Value

summary(sv0)
summary(sv12)

boxplot(sv0,sv12)
boxplot(log10(sv0),log10(sv12))

pm0$Date <- as.Date(as.character(pm0$Date),"%Y%m%d")
pm12$Date <- as.Date(as.character(pm12$Date),"%Y%m%d")

plot(pm0$Date,pm0$Sample.Value)
plot(pm12$Date,pm12$Sample.Value)
ggplot(pm0,aes(Date,Sample.Value)) + geom_point() + geom_smooth(method = "lm")
ggplot(pm12,aes(Date,Sample.Value)) + geom_point() + geom_smooth(method = "lm")

pm0sub <- pm0 %>%
            select(State.Code,County.Code,Site.ID,Date,Sample.Value)
pm12sub <- pm12 %>%
            select(State.Code,County.Code,Site.ID,Date,Sample.Value)

st0 <- pm0sub %>%
        filter(State.Code == 36)
st12 <- pm12sub %>%
        filter(State.Code == 36)
newst0 <- st0 %>%
            mutate(county.site = paste(County.Code,Site.ID,sep = "."))

newst12 <- st12 %>%
            mutate(county.site = paste(County.Code,Site.ID,sep = "."))


both <- intersect(newst0$county.site,newst12$county.site)

newpm0 <- newst0 %>%
            filter(county.site %in% both)
newpm12 <- newst12 %>%
            filter(county.site %in% both)
newpm0$county.site <- as.numeric(newpm0$county.site)
newpm12$county.site <- as.numeric(newpm12$county.site)

newpm0sub <- newpm0 %>%
                select(Date,State.Code,Sample.Value,county.site) %>%
                filter(county.site == 63.2008 )
newpm12sub <- newpm12 %>%
                select(Date,State.Code,Sample.Value,county.site) %>%
                filter(county.site == 63.2008)

ggplot(newpm0sub,aes(Date,Sample.Value)) + geom_point() + geom_smooth(method = "auto",se = FALSE)

ggplot(newpm12sub,aes(Date,Sample.Value)) + geom_point() + geom_smooth(method = "auto",se = FALSE)

rng <- range(newpm0sub$Sample.Value)

par(mfrow = c(1,2))

plot(newpm0sub$Sample.Value)
abline(h = mean(newpm0sub$Sample.Value,na.rm = TRUE))
plot(newpm12sub$Sample.Value)
abline(h = mean(newpm12sub$Sample.Value,na.rm = TRUE))





                




