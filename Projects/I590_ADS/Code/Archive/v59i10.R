options(stringsAsFactors = FALSE)

library("stringr")
library("reshape2")
library("plyr")

read.fwf2 <- function(path, cols) {
  raw_stations <- readLines(path)
  stations <- data.frame(matrix(ncol = 0, nrow = length(raw_stations)))

  for(i in 1:nrow(cols)) {
    field <- cols[i, ]
    stations[[field$name]] <- str_trim(str_sub(raw_stations, field$start, field$end))
  }
  stations[stations == ""] <- NA
  stations[] <- lapply(stations, type.convert, as.is = TRUE)
  
  stations
}

# Change defaults for xtable to be more attractive
# Inspired by: http://cameron.bracken.bz/sweave-xtable-booktabs
library("xtable")

xtable <- function(x, file = "", ..., rownames = FALSE) {
  table <- xtable::xtable(x, ...)
  print(table, floating = FALSE, hline.after = NULL, 
    add.to.row = list(pos = list(-1,0, nrow(x)), 
    command = c('\\toprule\n ','\\midrule\n  ','\\bottomrule\n')),
    include.rownames = rownames, NA.string = "---",
    file = file, 
    comment = FALSE, timestamp = FALSE
  )
}

####################################################################
## 2. Defining tidy data
####################################################################

set.seed(1014)

preg <- matrix(c(NA, sample(20, 5)), ncol = 2, byrow = TRUE)
colnames(preg) <- paste0("treatment", c("a", "b"))
rownames(preg) <- c("John Smith", "Jane Doe", "Mary Johnson")

xtable(preg, "preg-raw-1.tex", rownames = TRUE, align = "lrr")
xtable(t(preg), "preg-raw-2.tex", rownames = TRUE, align = "lrrr")

# Make tidy version

pregm <- melt(preg, id = "name")
names(pregm) <- c("name", "trt", "result")
pregm$trt <- gsub("treatment", "", pregm$trt)

xtable(pregm, "preg-tidy.tex")


####################################################################
## 3. Tidying messy datasets
####################################################################
## 3.1 Column headers are values, not variable names: toy example
####################################################################

df <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
xtable(df, "melt-raw.tex")

dfm <- melt(df, id = "row")
names(dfm)[2] <- "column"
xtable(dfm, "melt-output.tex")

####################################################################
## 3.1 Column headers are values, not variable names: Pew dataset
####################################################################

library("foreign")

# Data from http://pewforum.org/Datasets/Dataset-Download.aspx

# Load data -----------------------------------------------------------------

pew <- read.spss("data/v59i10-data/pew.sav")
pew <- as.data.frame(pew)

religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c(`Less than $10,000` = "<$10k", `10 to under $20,000` = "$10--20k", 
  `20 to under $30,000` = "$20--30k", `30 to under $40,000` = "$30--40k", `40 to under $50,000` = "$40--50k", 
  `50 to under $75,000` = "$50--75k", `75 to under $100,000` = "$75--100k", `100 to under $150,000` = "$100--150k", 
  `$150,000 or more` = ">150k", `Don't know/Refused (VOL)` = "Don't know/refused")[religion$income]

religion$income <- factor(religion$income, levels = c("<$10k", "$10--20k", "$20--30k", 
  "$30--40k", "$40--50k", "$50--75k", "$75--100k", "$100--150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

xtable(counts[1:10, ], file = "pew-clean.tex")

# Convert into the form in which I originally saw it -------------------------

raw <- dcast(counts, religion ~ income)
xtable(raw[1:10, 1:7], file = "pew-raw.tex")

####################################################################
## 3.2 Multiple variables stored in one column: TB dataset
####################################################################

# Load -----------------------------------------------------------------------
raw <- read.csv("data/v59i10-data/tb.csv", na.strings = "")
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)
names(raw)[1] <- "country"

names(raw) <- str_replace(names(raw), "new_sp_", "")
raw$m04 <- NULL
raw$m514 <- NULL
raw$f04 <- NULL
raw$f514 <- NULL

xtable(raw[1:10, 1:11], file = "tb-raw.tex")

# Melt -----------------------------------------------------------------------

clean <- melt(raw, id = c("country", "year"), na.rm = TRUE)
names(clean)[3] <- "column"
names(clean)[4] <- "cases"

clean <- arrange(clean, country, column, year)
xtable(clean[1:15, ], file = "tb-clean-1.tex")

# Break up variable in to sex and age ----------------------------------------

clean$sex <- str_sub(clean$column, 1, 1)

ages <- c(`04` = "0--4", `514` = "5--14", `014` = "0--14", `1524` = "15--24", `2534` = "25--34", 
  `3544` = "35--44", `4554` = "45--54", `5564` = "55--64", `65` = "65+", u = NA)

clean$age <- factor(ages[str_sub(clean$column, 2)], levels = ages)

clean <- clean[c("country", "year", "sex", "age", "cases")]

xtable(clean[1:15, ], file = "tb-clean-2.tex")


####################################################################
## 3.3 Variables are stored in both rows and columns: weather dataset
####################################################################

# Define format for fixed width file
cols <- data.frame(name = c("id", "year", "month", "element"), start = c(1, 12, 16, 
  18), end = c(11, 15, 17, 21))

names <- str_c(c("value", "mflag", "qflag", "sflag"), rep(1:31, each = 4), sep = "_")
starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
starts <- starts[-length(starts)]
ends <- c(starts[-1], starts[length(starts)] + 1) - 1

values <- data.frame(name = names, start = starts, end = ends)
cols <- rbind(cols, values)

# Load data and subset to small example
raw <- read.fwf2("weather.txt", cols)
raw <- subset(raw, year == 2010 & element %in% c("TMIN", "TMAX"))
raw <- raw[, c(1:4, which(str_detect(names(raw), "value")))]
raw$id <- str_c(str_sub(raw$id, 1, 2), str_sub(raw$id, -5, -1))

names(raw)[-(1:4)] <- str_c("d", 1:31)
raw[raw == -9999] <- NA
raw[-(1:4)] <- raw[-(1:4)]/10
rownames(raw) <- NULL
raw$element <- tolower(raw$element)

xtable(raw[1:10, 1:12], file = "weather-raw.tex", digits = 1)

# Melt and tidy

clean1 <- melt(raw, id = 1:4, na.rm = TRUE)
clean1$day <- as.integer(str_replace(clean1$variable, "d", ""))
clean1$date <- as.Date(ISOdate(clean1$year, clean1$month, clean1$day))

clean1 <- clean1[c("id", "date", "element", "value")]
clean1 <- arrange(clean1, date, element)
clean1$date <- as.character(clean1$date)  # work around xtable bug
xtable(clean1[1:10, ], file = "weather-clean-1.tex", digits = 1)

# Cast

clean2 <- dcast(clean1, ... ~ element)
xtable(clean2[1:10, ], file = "weather-clean-2.tex", digits = 1)


####################################################################
## 3.4 Multiple types in one table: Billboard dataset
####################################################################

library("lubridate")

raw <- read.csv("billboard.csv")

raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", 
  "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", 
  "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", 
  "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", 
  "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", 
  "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", 
  "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", 
  "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", 
  "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", 
  "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", 
  "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", 
  "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", 
  "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", 
  "x74th.week", "x75th.week", "x76th.week")]

names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

long_name <- nchar(raw$track) > 20
raw$track[long_name] <- paste0(substr(raw$track[long_name], 0, 20), "...")


xtable(raw[c(1:3, 6:10), 1:8], "billboard-raw.tex")

clean <- melt(raw, id = 1:5, na.rm = TRUE)
clean$week <- as.integer(str_replace_all(clean$variable, "[^0-9]+", ""))
clean$variable <- NULL

clean$date.entered <- ymd(clean$date.entered)
clean$date <- clean$date.entered + weeks(clean$week - 1)
clean$date.entered <- NULL
clean <- rename(clean, c(value = "rank"))
clean <- arrange(clean, year, artist, track, time, week)
clean <- clean[c("year", "artist", "time", "track", "date", "week", "rank")]

clean_out <- mutate(clean, date = as.character(date))
xtable(clean_out[1:15, ], "billboard-clean.tex")

# Normalization --------------------------------------------------------------

song <- unrowname(unique(clean[c("artist", "track", "time")]))
song$id <- 1:nrow(song)

narrow <- song[1:15, c("id", "artist", "track", "time")]
xtable(narrow, "billboard-song.tex")

rank <- join(clean, song, match = "first")
rank <- rank[c("id", "date", "rank")]
rank$date <- as.character(rank$date)
xtable(rank[1:15, ], "billboard-rank.tex")

####################################################################
## 5. Case study
####################################################################

library("ggplot2")
library("MASS")

if (!file.exists("data/v59i10-data/deaths.rds")) {
  ## from https://github.com/hadley/mexico-mortality/raw/master/deaths/deaths08.csv.bz2
  deaths <- read.csv("deaths08.csv.bz2")
  unlink("deaths08.csv.bz2")
  deaths$hod[deaths$hod == 99] <- NA
  deaths$hod[deaths$hod == 24] <- 0
  deaths$hod[deaths$hod == 0] <- NA
  deaths$hod <- as.integer(deaths$hod)
  deaths <- arrange(deaths, yod, mod, dod, hod, cod)
  deaths <- deaths[c("yod", "mod", "dod", "hod", "cod")]  
  saveRDS(deaths, "deaths.rds")
}

deaths <- readRDS("deaths.rds")

ok <- subset(deaths, yod == 2008 & mod != 0 & dod != 0)
xtable(ok[c(1, 1:14 * 2000), c("yod", "mod", "dod", "hod", "cod")], "raw.tex")

codes <- read.csv("icd-main.csv")
codes$disease <- sapply(codes$disease, function(x) str_c(strwrap(x, width = 30), 
  collapse = "\n"))
names(codes)[1] <- "cod"
codes <- codes[!duplicated(codes$cod), ]

# Display overall hourly deaths
hod_all <- subset(count(deaths, "hod"), !is.na(hod))
qplot(hod, freq, data = hod_all, geom = "line") + scale_y_continuous("Number of deaths", 
  labels = function(x) format(x, big.mark = ",")) + xlab("Hour of day")
ggsave("overall.pdf", width = 10, height = 6)

# Count deaths per hour, per disease
hod2 <- count(deaths, c("cod", "hod"))
hod2 <- subset(hod2, !is.na(hod))
hod2 <- join(hod2, codes)
hod2 <- ddply(hod2, "cod", transform, prop = freq/sum(freq))

# Compare to overall abundance
overall <- ddply(hod2, "hod", summarise, freq_all = sum(freq))
overall <- mutate(overall, prop_all = freq_all/sum(freq_all))

hod2 <- join(overall, hod2, by = "hod")

# Pick better subset of rows to show
cods <- join(arrange(count(deaths, "cod"), desc(freq)), codes)
mutate(tail(subset(cods, freq > 100), 30), disease = str_sub(disease, 1, 30))

hod3 <- subset(hod2, cod %in% c("I21", "N18", "E84", "B16") & hod >= 8 & hod <= 12)[1:15, 
  c("hod", "cod", "disease", "freq", "prop", "freq_all", "prop_all")]

xtable(hod3[c("hod", "cod", "freq")], "counts.tex")
xtable(hod3[c("disease")], "counts-disease.tex")
xtable(hod3[5], "counts-prop.tex")
xtable(hod3[6:7], "counts-all.tex")

devi <- ddply(hod2, "cod", summarise, n = sum(freq), dist = mean((prop - prop_all)^2))
devi <- subset(devi, n > 50)

# Find outliers
xlog10 <- scale_x_log10(breaks = c(100, 1000, 10000), labels = c(100, 1000, 10000), 
  minor_breaks = log10(outer(1:9, 10^(1:5), "*")))
ylog10 <- scale_y_log10(breaks = 10^-c(3, 4, 5), labels = c("0.001", "0.0001", "0.00001"), 
  minor_breaks = log10(outer(1:9, 10^-(3:6), "*")))

qplot(n, dist, data = devi)
ggsave("n-dist-raw.pdf", width = 6, height = 6)
qplot(n, dist, data = devi) + geom_smooth(method = "rlm", se = FALSE) + xlog10 + ylog10
ggsave("n-dist-log.pdf", width = 6, height = 6)

devi$resid <- resid(rlm(log(dist) ~ log(n), data = devi))
coef(rlm(log(dist) ~ log(n), data = devi))
ggplot(devi, aes(n, resid)) + geom_hline(yintercept = 1.5, colour = "grey50") + geom_point() + 
  xlog10
ggsave("n-dist-resid.pdf", width = 6, height = 6)

unusual <- subset(devi, resid > 1.5)
hod_unusual_big <- match_df(hod2, subset(unusual, n > 350))
hod_unusual_sml <- match_df(hod2, subset(unusual, n <= 350))

# Visualize unusual causes of death
ggplot(hod_unusual_big, aes(hod, prop)) + geom_line(aes(y = prop_all), data = overall, 
  colour = "grey50") + geom_line() + facet_wrap(~disease, ncol = 3)
ggsave("unusual-big.pdf", width = 8, height = 6)
last_plot() %+% hod_unusual_sml
ggsave("unusual-sml.pdf", width = 8, height = 4) 
