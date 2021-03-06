library(rgdal)
library(readxl)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shiny)
library(xlsx)

setwd("/Users/Machine/Desktop/test3/Statistiniai paketai x")

###
# 1. IMPORTAI

# 1.1.1 Importuojame savivaldybi� rib� koordinates
spdf <- readOGR(dsn = file.path("Savivaldybes.shp"), stringsAsFactors = F)

# 1.1.2 Patikriname importuotas koordinates
summary(spdf)
#koordinat�s n�ra globalaus koordina�i� standarto

# 1.1.3 Konvertuojame koordinatesi globalu standarta
spdf <- spTransform(spdf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
summary(spdf)

# 1.1.4 Sutvarkome koordinates pagal SAV_PAV tipa
sav_spdf <- broom::tidy(spdf, region = "SAV_PAV")
head(sav_spdf)
# koordina�i� stulpelio "id" reik�m�s neatvaizduoja lietuvi�k� ra�men�

# 1.1.5 Eksportuojame koordinates, kad pataisytume stulpelio "id" reik�mes
#df <- as.data.frame(sav_spdf)
#write.xlsx(df, file = "df.xlsx",
#           sheetName = "df", append = FALSE)
#Atsidar� eksportuot� fail� pataisome stulpeli� "group" ir "id" reik�mes � lietuvi� kalb�

# 1.1.6 Importuojame koordinates su pataisytais pavadinimais
df_fix <- read_excel("df_fix.xlsx")
names(df_fix) <- c("ID", "long", "lat", "order", "hole", "piece", "group", "id")
head(df_fix)
# 1.2 Importuojame poligon� grupi� informacij� poligon� konstravimui
id_list_edit <- read_excel("sav_id.xlsx")
head(id_list_edit)

# 1.3.1 Importuojame savivaldybi� statistik� (gyventoj� skai�ius; prezidento rinkim�
# pirmo turo savivaldyb�je daugiausiai surinkusio kandidato trumpinys)
# kur rajon� ir miest� informacija yra atskirai atskirai
sav_stat_id_ats <- read_excel("sav_stat_id_ats.xlsx")
head(sav_stat_id_ats)

# 1.3.2 Importuojame savivaldybi� statistik� (gyventoj� skai�ius)
# kur rajon� ir miest� informacija yra kartu
sav_stat_id_suj <- read_excel("sav_stat_id_suj.xlsx")
head(sav_stat_id_suj)

# 1.4 Importuojame rinkim� statistik�
# https://www.vrk.lt/2019-prezidento/rezultatai
# 1.4.1 Pirmas turas
rink1_stat <- read_excel("rink_stat_pirmas_turas.xlsx")
head(rink1_stat)
# 1.4.2 Antras turas
rink2_stat <- read_excel("rink_stat_antras_turas.xlsx")
head(rink2_stat)
# 1.4.3 Pirmo turo savivaldybi� duomenys - nesutvarkyti
rink_stat <- read_excel("rink_stat.xlsx")
names(rink_stat) <- c("SAV", "VARD", "BALSU_VISO", "LOG", "RESULT")
head(rink_stat)

# 2. DARBAS SU DUOMENIMIS

# 2.1 Importuot� duomen� sujungimas

# 2.1.1 Sujungiame koordina�i� duomenis su statistikos duomenimis
c1_df <- merge(df_fix,sav_stat_id_suj,by="id")
head(c1_df)
# 2.1.2 Sujungiame koordina�i� duomenis su statistikos duomenimis
c2_df <- merge(df_fix,sav_stat_id_ats,by="id")
head(c2_df)

# 2.2 Importuot� duomen� tvarkymas

# 2.2.1 Stulpeli� VARDAS ir PAVARDE tvarkymas
rink1_stat$VARD <- paste(rink1_stat$VARDAS,rink1_stat$PAVARDE)
rink1_stat$VARDAS <- NULL
rink1_stat$PAVARDE <- NULL
rink1_stat$SAV[is.na(rink1_stat$SAV)] <- "AMBASADA"
head(rink1_stat)

# 2.2.2  Sutvarkome dataframe
rink1_stat_agr1 <- aggregate(BALSU_VISO~VARD+SAV, rink1_stat, sum)
rink1_stat_agr2 <- aggregate(VISO_DALYVAVO~VARD+SAV, rink1_stat, sum)
rink1_stat_agr3 <- merge(rink1_stat_agr1,rink1_stat_agr2,by=c("SAV","VARD"))
rink1_stat_agr3 <- transform(rink1_stat_agr3, PROC = BALSU_VISO / VISO_DALYVAVO)
head(rink1_stat_agr1, 18)
head(rink1_stat_agr2, 18)
head(rink1_stat_agr3, 18)

# 2.3 Sukursime ry�� tarp statistikos dataframe ir koordina�i� dataframe

# 2.3.1 I�renkame savivaldybes i� rinkim� statistikos dataframe
ur <- unique(rink1_stat$SAV)
head(ur, 61)
# 2.3.2 I�renkame savivaldybes i� sujungto koordina�i� ir rinkim� statistikos dataframe
ua <- unique(c2_df$id)
head(ua, 60)
# 2.3.3 Pridedame ambasad� � savivaldybi� s�ra��
uaa <- c(ua, "AMBASADA")
head(uaa, 61)
# 2.3.4 Sutvarkome dataframe ur savivaldybi� eili�kum�, kad atitikt� dataframe uaa
urr <- ur[c(1:57,59,60,61,58)]
head(urr, 61)

# 2.3.5 Sujungiame savivaldybi� pavadinimus i� sujungto koordina�i� ir statistiko dataframe
# su savivaldybi� pavadinimais i� rinkim� statistikos dataframe
a_1 <- cbind(urr,uaa)
head(a_1, 61)

# 2.3.6 Sutvarkome, kad stulpeli� pavadinimai atitikt� stulpelius i� �altinio dataframe
a_1 <- as.data.frame(a_1)
names(a_1) <- c("SAV", "id")

# 2.3.7 Sujungiame sutvarkyt� dataframe su savivaldybi� pavadinim� dataframe
rink_stat_df <- merge(a_1,rink1_stat_agr3,by="SAV")
head(rink_stat_df, 18)

# 2.4.1 I�renkame kiekvieno kandidato statistik� pagal savivaldybes atskiruose dataframe
a_2 <- lapply(unique(rink_stat_df$VARD),
              function(x)
                list(
                  rbind(
                    rink_stat_df[rink_stat_df$VARD == x,][,c("BALSU_VISO", "PROC", "id")])))

#Juozaitis
b_1 <- as.data.frame(a_2[1])
#Naus�da
b_2 <- as.data.frame(a_2[2])
#�imonyt�
b_3 <- as.data.frame(a_2[3])
#Puidokas
b_4 <- as.data.frame(a_2[4])
#Puteikis
b_5 <- as.data.frame(a_2[5])
#Skvernelis
b_6 <- as.data.frame(a_2[6])
#Toma�evski
b_7 <- as.data.frame(a_2[7])
#Mazuronis
b_8 <- as.data.frame(a_2[8])
#Andriukaitis
b_9 <- as.data.frame(a_2[9])

# 2.4.2 Sujungiame koordina�i� duomenis su kandidato Naus�dos statistikos duomenimis
c3_df <- merge(df_fix,b_2,by="id")
head(c3_df)
# 2.4.3 Sujungiame koordina�i� duomenis su kandidato �imonyt�s statistikos duomenimis
c4_df <- merge(df_fix,b_3,by="id")
head(c4_df)

# 2.5 Rinkim� antro turo statistikos tvarkymas

# 2.5.1 Stulpeli� VARDAS ir PAVARDE tvarkymas
rink2_stat$VARD <- paste(rink2_stat$VARDAS,rink2_stat$PAVARDE)
rink2_stat$VARDAS <- NULL
rink2_stat$PAVARDE <- NULL
rink2_stat$SAV[is.na(rink2_stat$SAV)] <- "AMBASADA"
head(rink2_stat)

# 2.5.2  Sutvarkome dataframe 
rink2_stat_agr1 <- aggregate(BALSU_VISO~VARD+SAV, rink2_stat, sum)
rink2_stat_agr2 <- aggregate(VISO_DALYVAVO~VARD+SAV, rink2_stat, sum)
rink2_stat_agr3 <- merge(rink2_stat_agr1,rink2_stat_agr2,by=c("SAV","VARD"))
rink2_stat_agr3 <- transform(rink2_stat_agr3, PROC = BALSU_VISO / VISO_DALYVAVO)
head(rink2_stat_agr1, 18)
head(rink2_stat_agr2, 18)
head(rink2_stat_agr3, 18)

# 2.6 Sukursime ry�� tarp statistikos dataframe ir koordina�i� dataframe

# 2.6.1 I�renkame savivaldybes i� rinkim� statistikos dataframe
Bur <- unique(rink2_stat$SAV)
head(Bur, 61)
# 2.6.2 I�renkame savivaldybes i� sujungto koordina�i� ir rinkim� statistikos dataframe
Bua <- unique(c2_df$id)
head(Bua, 60)
# 2.6.3 Pridedame ambasad� � savivaldybi� s�ra��
Buaa <- c(Bua, "AMBASADA")
head(Buaa, 61)
# 2.6.4 Sutvarkome dataframe Bur savivaldybi� eili�kum�, kad atitikt� dataframe Buaa
Burr <- Bur[c(1:56,58:61,57)]
head(Burr, 61)

# 2.6.5 Sujungiame savivaldybi� pavadinimus i� sujungto koordina�i� ir statistiko dataframe
# su savivaldybi� pavadinimais i� rinkim� statistikos dataframe
Ba_1 <- cbind(Burr,Buaa)
head(Ba_1, 61)

# 2.6.6 Sutvarkome, kad stulpeli� pavadinimai atitikt� stulpelius i� �altinio dataframe
Ba_1 <- as.data.frame(Ba_1)
names(Ba_1) <- c("SAV", "id")

# 2.6.7 Sujungiame sutvarkyt� dataframe su savivaldybi� pavadinim� dataframe
rink2_stat_df <- merge(Ba_1,rink2_stat_agr3,by="SAV")
head(rink2_stat_df)

# 2.7.1 I�renkame kiekvieno kandidato statistik� pagal savivaldybes atskiruose dataframe
Ba_2 <- lapply(unique(rink2_stat_df$VARD),
               function(x)
                 list(
                   rbind(
                     rink2_stat_df[rink2_stat_df$VARD == x,][,c("BALSU_VISO", "PROC", "id")])))

#Naus�da
v_1 <- as.data.frame(Ba_2[1])
#�imonyt�
v_2 <- as.data.frame(Ba_2[2])

# 2.7.2 Sujungiame koordina�i� duomenis su kandidato Naus�dos statistikos duomenimis
c5_df <- merge(df_fix,v_1,by="id")
head(c5_df)
# 2.7.3 Sujungiame koordina�i� duomenis su kandidato �imonyt�s statistikos duomenimis
c6_df <- merge(df_fix,v_2,by="id")
head(c6_df)

# 2.8 Apskritimas

# 2.8.1 Apskritimo ta�kus sukurianti funkcija
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))}

# 2.8.2 Sukuriame ir tvarkome apskritimo ta�kus
apskritimas <- circleFun(c(20,55.5), 0.5, npoints = 100)
names(apskritimas) <- c("long", "lat")
apskritimas["ID"] <- "Na"
apskritimas["order"] <- "Na"
apskritimas["hole"] <- "Na"
apskritimas["piece"] <- "Na"
apskritimas["group"] <- "AMBASADA"
apskritimas["id"] <- "AMBASADA"
head(apskritimas)

# 2.8.3 Sujungiame apskritim� su koordinat�mis
dataf1 <- rbind(apskritimas, df_fix)
# 2.8.5 Sujungiame koordina�i� duomenis su apskritimu su kandidato Naus�dos pirmo turo statistikos duomenimis
d3_df <- merge(dataf1,b_2,by="id")
# 2.8.6 Sujungiame koordina�i� duomenis su apskritimu su kandidato �imonyt�s pirmo turo statistikos duomenimis
d4_df <- merge(dataf1,b_3,by="id")
# 2.8.7 Sujungiame koordina�i� duomenis su apskritimu su kandidato Naus�dos antro turo statistikos duomenimis
d5_df <- merge(dataf1,v_1,by="id")
# 2.8.8 Sujungiame koordina�i� duomenis su apskritimu su kandidato �imonyt�s antro turo statistikos duomenimis
d6_df <- merge(dataf1,v_2,by="id")

# 2.9 I� importuoto koordina�i� dataframe su pataisytomis � lietuvi� kalb� stulpeli� "id" ir "group"
# reik�m�mis sukursime spatialpolygonsdataframe tipo objekt�

# 2.9.1 Skriptas sukuriantis i� koordina�i� dataframe s�ra�� koordina�i� grupi�,
#kur kievienas s�ra�o elementas yra unikalios savivaldyb�s koordian�i� lentel�.
matrica <- lapply(unique(df_fix$group),
                  function(x)
                    list(matrix(unlist(
                      rbind(
                        df_fix[df_fix$group == x,][,c("long", "lat")],
                        df_fix[df_fix$group == x,][1, c("long", "lat")])),
                      ncol = 2)))

# 2.9.2 Sukuriame kiekvienos savivaldyb�s poligon�
m1 <- Polygon(matrica[1])
m2 <- Polygon(matrica[2])
m3 <- Polygon(matrica[3])
m4 <- Polygon(matrica[4])
m5 <- Polygon(matrica[5])
m6 <- Polygon(matrica[6])
m7 <- Polygon(matrica[7])
m8 <- Polygon(matrica[8])
m9 <- Polygon(matrica[9])
m10 <- Polygon(matrica[10])
m11 <- Polygon(matrica[11])
m12 <- Polygon(matrica[12])
m13 <- Polygon(matrica[13])
m14 <- Polygon(matrica[14])
m15 <- Polygon(matrica[15])
m16 <- Polygon(matrica[16])
m17 <- Polygon(matrica[17])
m18 <- Polygon(matrica[18])
m19 <- Polygon(matrica[19])
m20 <- Polygon(matrica[20])
m21 <- Polygon(matrica[21])
m22 <- Polygon(matrica[22])
m23 <- Polygon(matrica[23])
m24 <- Polygon(matrica[24])
m25 <- Polygon(matrica[25])
m26 <- Polygon(matrica[26])
m27 <- Polygon(matrica[27])
m28 <- Polygon(matrica[28])
m29 <- Polygon(matrica[29])
m30 <- Polygon(matrica[30])
m31 <- Polygon(matrica[31])
m32 <- Polygon(matrica[32])
m33 <- Polygon(matrica[33])
m34 <- Polygon(matrica[34])
m35 <- Polygon(matrica[35])
m36 <- Polygon(matrica[36])
m37 <- Polygon(matrica[37])
m38 <- Polygon(matrica[38])
m39 <- Polygon(matrica[39])
m40 <- Polygon(matrica[40])
m41 <- Polygon(matrica[41])
m42 <- Polygon(matrica[42])
m43 <- Polygon(matrica[43])
m44 <- Polygon(matrica[44])
m45 <- Polygon(matrica[45])
m46 <- Polygon(matrica[46])
m47 <- Polygon(matrica[47])
m48 <- Polygon(matrica[48])
m49 <- Polygon(matrica[49])
m50 <- Polygon(matrica[50])
m51 <- Polygon(matrica[51])
m52 <- Polygon(matrica[52])
m53 <- Polygon(matrica[53])
m54 <- Polygon(matrica[54])
m55 <- Polygon(matrica[55])
m56 <- Polygon(matrica[56])
m57 <- Polygon(matrica[57])
m58 <- Polygon(matrica[58])
m59 <- Polygon(matrica[59])
m60 <- Polygon(matrica[60])
m61 <- Polygon(matrica[61])
m62 <- Polygon(matrica[62])
m63 <- Polygon(matrica[63])
m64 <- Polygon(matrica[64])
m65 <- Polygon(matrica[65])

# 2.9.3 Susiejame poligonus su savivaldybi� pavadinimais
mm1 <- Polygons(list(m1),id_list_edit[1,1])
mm2 <- Polygons(list(m2),id_list_edit[2,1])
mm3 <- Polygons(list(m3),id_list_edit[3,1])
mm4 <- Polygons(list(m4),id_list_edit[4,1])
mm5 <- Polygons(list(m5),id_list_edit[5,1])
mm6 <- Polygons(list(m6),id_list_edit[6,1])
mm7 <- Polygons(list(m7),id_list_edit[7,1])
mm8 <- Polygons(list(m8),id_list_edit[8,1])
mm9 <- Polygons(list(m9),id_list_edit[9,1])
mm10 <- Polygons(list(m10),id_list_edit[10,1])
mm11 <- Polygons(list(m11),id_list_edit[11,1])
mm12 <- Polygons(list(m12),id_list_edit[12,1])
mm13 <- Polygons(list(m13),id_list_edit[13,1])
mm14 <- Polygons(list(m14),id_list_edit[14,1])
mm15 <- Polygons(list(m15),id_list_edit[15,1])
mm16 <- Polygons(list(m16),id_list_edit[16,1])
mm17 <- Polygons(list(m17),id_list_edit[17,1])
mm18 <- Polygons(list(m18),id_list_edit[18,1])
mm19 <- Polygons(list(m19),id_list_edit[19,1])
mm20 <- Polygons(list(m20),id_list_edit[20,1])
mm21 <- Polygons(list(m21),id_list_edit[21,1])
mm22 <- Polygons(list(m22),id_list_edit[22,1])
mm23 <- Polygons(list(m23),id_list_edit[23,1])
mm24 <- Polygons(list(m24),id_list_edit[24,1])
mm25 <- Polygons(list(m25),id_list_edit[25,1])
mm26 <- Polygons(list(m26),id_list_edit[26,1])
mm27 <- Polygons(list(m27),id_list_edit[27,1])
mm28 <- Polygons(list(m28),id_list_edit[28,1])
mm29 <- Polygons(list(m29),id_list_edit[29,1])
mm30 <- Polygons(list(m30),id_list_edit[30,1])
mm31 <- Polygons(list(m31),id_list_edit[31,1])
mm32 <- Polygons(list(m32),id_list_edit[32,1])
mm33 <- Polygons(list(m33),id_list_edit[33,1])
mm34 <- Polygons(list(m34),id_list_edit[34,1])
mm35 <- Polygons(list(m35),id_list_edit[35,1])
mm36 <- Polygons(list(m36),id_list_edit[36,1])
mm37 <- Polygons(list(m37),id_list_edit[37,1])
mm38 <- Polygons(list(m38),id_list_edit[38,1])
mm39 <- Polygons(list(m39),id_list_edit[39,1])
mm40 <- Polygons(list(m40),id_list_edit[40,1])
mm41 <- Polygons(list(m41),id_list_edit[41,1])
mm42 <- Polygons(list(m42),id_list_edit[42,1])
mm43 <- Polygons(list(m43),id_list_edit[43,1])
mm44 <- Polygons(list(m44),id_list_edit[44,1])
mm45 <- Polygons(list(m45),id_list_edit[45,1])
mm46 <- Polygons(list(m46),id_list_edit[46,1])
mm47 <- Polygons(list(m47),id_list_edit[47,1])
mm48 <- Polygons(list(m48),id_list_edit[48,1])
mm49 <- Polygons(list(m49),id_list_edit[49,1])
mm50 <- Polygons(list(m50),id_list_edit[50,1])
mm51 <- Polygons(list(m51),id_list_edit[51,1])
mm52 <- Polygons(list(m52),id_list_edit[52,1])
mm53 <- Polygons(list(m53),id_list_edit[53,1])
mm54 <- Polygons(list(m54),id_list_edit[54,1])
mm55 <- Polygons(list(m55),id_list_edit[55,1])
mm56 <- Polygons(list(m56),id_list_edit[56,1])
mm57 <- Polygons(list(m57),id_list_edit[57,1])
mm58 <- Polygons(list(m58),id_list_edit[58,1])
mm59 <- Polygons(list(m59),id_list_edit[59,1])
mm60 <- Polygons(list(m60),id_list_edit[60,1])
mm61 <- Polygons(list(m61),id_list_edit[61,1])
mm62 <- Polygons(list(m62),id_list_edit[62,1])
mm63 <- Polygons(list(m63),id_list_edit[63,1])
mm64 <- Polygons(list(m64),id_list_edit[64,1])
mm65 <- Polygons(list(m65),id_list_edit[65,1])

# 2.9.4 Sujungiame poligonus
m_all = Polygons(list(m1,	m2,	m3,	m4,	m5,	m6,	m7,	m8,	m9,	m10,	m11,	m12,	m13,	m14,	m15,	m16,	m17,	m18,	m19,	m20,	m21,	m22,	m23,	m24,	m25,	m26,	m27,	m28,	m29,	m30,	m31,	m32,	m33,	m34,	m35,	m36,	m37,	m38,	m39,	m40,	m41,	m42,	m43,	m44,	m45,	m46,	m47,	m48,	m49,	m50,	m51,	m52,	m53,	m54,	m55,	m56,	m57,	m58,	m59,	m60,	m61,	m62,	m63,	m64,	m65),1)
# 2.9.5 I� poligon� su ry�iumi su savivaldyb�mis padarome spatialpolygons objekta
m_all_sp = SpatialPolygons(list(mm1,	mm2,	mm3,	mm4,	mm5,	mm6,	mm7,	mm8,	mm9,	mm10,	mm11,	mm12,	mm13,	mm14,	mm15,	mm16,	mm17,	mm18,	mm19,	mm20,	mm21,	mm22,	mm23,	mm24,	mm25,	mm26,	mm27,	mm28,	mm29,	mm30,	mm31,	mm32,	mm33,	mm34,	mm35,	mm36,	mm37,	mm38,	mm39,	mm40,	mm41,	mm42,	mm43,	mm44,	mm45,	mm46,	mm47,	mm48,	mm49,	mm50,	mm51,	mm52,	mm53,	mm54,	mm55,	mm56,	mm57,	mm58,	mm59,	mm60,	mm61,	mm62,	mm63,	mm64,	mm65), 1:65)

# 2.10.1 Sujungiame pirmo turo kandidat� gaut� bals� savivaldyb�se duomenis � dataframe
B <- merge(b_1, b_2, by = "id")
B <- merge(B, b_3, by = "id")
B <- merge(B, b_4, by = "id")
B <- merge(B, b_5, by = "id")
B <- merge(B, b_6, by = "id")
B <- merge(B, b_7, by = "id")
B <- merge(B, b_8, by = "id")
B <- merge(B, b_9, by = "id")
B$PROC.x <- NULL
B$PROC.x <- NULL
B$PROC.x <- NULL
B$PROC.x <- NULL
B$PROC.y <- NULL
B$PROC.y <- NULL
B$PROC.y <- NULL
B$PROC.y <- NULL
B$PROC <- NULL
names(B) <- c("id","ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND")
head(B, 20)

# 2.10.2 Sujungiame pirmo turo kandidat� procent� surinkt� bals� savivaldyb�se duomenis � dataframe
B1 <- merge(b_1, b_2, by = "id")
B1 <- merge(B1, b_3, by = "id")
B1 <- merge(B1, b_4, by = "id")
B1 <- merge(B1, b_5, by = "id")
B1 <- merge(B1, b_6, by = "id")
B1 <- merge(B1, b_7, by = "id")
B1 <- merge(B1, b_8, by = "id")
B1 <- merge(B1, b_9, by = "id")
B1$BALSU_VISO.x <- NULL
B1$BALSU_VISO.x <- NULL
B1$BALSU_VISO.x <- NULL
B1$BALSU_VISO.x <- NULL
B1$BALSU_VISO.y <- NULL
B1$BALSU_VISO.y <- NULL
B1$BALSU_VISO.y <- NULL
B1$BALSU_VISO.y <- NULL
B1$BALSU_VISO <- NULL
names(B1) <- c("id","ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND")
head(B1, 20)

# 2.11.1 I�renkame i� pataisyto savivaldybi� koordina�i� dataframe
# savivaldybi� poligon� grupi� ir savivaldybi� pavadinim� ry��
gr_id <- as.data.frame(unique(cbind(df_fix$group, df_fix$id)))
names(gr_id) <- c("group", "id")

# 2.11.2 Kiekvienai poligon� grup�s ir savivaldyb�s pavadinimo eilutei priskiriame numer�
gr_id$NR <- c(1:65)
head(gr_id, 65)

# 2.11.3 Sujungiame ry�io tarp grupi� ir id dataframe su statistikos dataframe
gr_id_stat <- merge(gr_id, sav_stat_id_ats, by = "id")

# 2.11.4 Sur��iuojame pagal ry�io numer� NR ir i�triname ry�io �ym�
gr_id_stat <- gr_id_stat %>% arrange(NR)
gr_id_stat$NR <- NULL
head(gr_id_stat)

# 2.11.5 Sujungiame savivaldybi� statistik� su rinkim� pirmo turo gaut� bals� statistika
gr_id_stat2 <- gr_id_stat
gr_id_stat2 <- merge(gr_id_stat2, B, by = "id")
gr_id_stat2$RESULT <- NA
head(gr_id_stat2)

# 2.11.6 Savivaldybi� statistika su rinkim� pirmo turo gaut� bals� statistika
# be poligon� grupi� (stulpelis group) informacija
id_stat <- gr_id_stat2
id_stat$group <- NULL
id_stat <- unique(id_stat)
head(id_stat)

# 2.11.7 Kiekvienai dataframe eilutei priskiriame numer�
gr_id_stat3 <- gr_id_stat
gr_id_stat3$NR <- c(1:65)

# 2.11.8 Sujungiame savivaldybi� statistik� su rinkim� pirmo turo procent� statistika
gr_id_stat3 <- merge(gr_id_stat3, B1, by = "id")
gr_id_stat3 <- gr_id_stat3 %>% arrange(NR)
gr_id_stat3$NR <- NULL
head(gr_id_stat3)

# 2.11.9 Skriptas Shiny ptogram�lei nustatantis kas laim�jo savivaldyb�je
# tarp Andriukai�io ir Arvydo Juozai�io
id_stat1 <- id_stat %>% select("id", "AND", "ARV", "RESULT")
head(id_stat1)
for (i in 1:60){
  if (id_stat1[i,2]>id_stat1[i,3]) {
    id_stat1[i, 4] = "AND"
  }
  else {
    id_stat1[i, 4] = "ARV"
  }
}
head(id_stat1)

# 2.11.10 Skriptas Shiny ptogram�lei �em�lapio konstravimui
gr_id_stat1 <- gr_id_stat2 %>% select("id", "group", "AND", "ARV", "RESULT")
head(gr_id_stat1)
for (i in 1:65){
  if (gr_id_stat1[i,3]>gr_id_stat1[i,4]) {
    gr_id_stat1[i, 5] = "AND"
  }
  else {
    gr_id_stat1[i, 5] = "ARV"
  }
}
head(gr_id_stat1)

# 2.12 Kuriame SpatialPolygonsDataFrame objektus

# 2.12.1 Savivaldybi� statistika
add1 = data.frame(gr_id_stat)
spdf1 = SpatialPolygonsDataFrame(m_all_sp,add1)
head(spdf1)
summary(spdf1)
dd_pal <- colorFactor(c("Set1"), domain = spdf1$LAIM)

# 2.12.2 Shiny program�lei
add2 = data.frame(gr_id_stat1)
spdf2 = SpatialPolygonsDataFrame(m_all_sp,add2)
e_pal <- colorFactor(c("Set1"), domain = spdf2$RESULT)

# 2.12.3 Savivaldyb�je procentaliai laim�jim� statistika
add3 = data.frame(gr_id_stat3)
spdf3 = SpatialPolygonsDataFrame(m_all_sp,add3)
summary(spdf3)
p_pal <- colorNumeric("YlOrRd", domain = spdf3$GYV_SK)

# 3. �em�lapi� k�rimas

# 3.
c_pal <- colorNumeric("YlOrRd", domain = spdf1$GYV_SK)
labels <- sprintf(
  "<strong>%s</strong><br/>%g gyvenan�i� �moni�",
  spdf1$id, spdf1$GYV_SK) %>% lapply(htmltools::HTML)
xmap <- leaflet(spdf1) %>%
  addTiles() %>%
  addMarkers(lng=25.279652, lat=54.687157, popup="Vilnius") %>%
  addPolygons(
    fillColor = ~c_pal(GYV_SK),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

####GGPLOT
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank())

#1
map1 <- ggplot() + geom_polygon(data = df_fix, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
#pavadinimai
cnames3 <- aggregate(cbind(long, lat) ~ id, data=dataf1, FUN=mean)
#2
map2 <- ggplot() +
  geom_polygon(data = df_fix, aes(x = long, y = lat, group = group), colour = "black", fill = "lightgreen")+
  xlab("Ilguma")+
  ylab("Platuma")+
  ggtitle("Lietuvos savivaldyb�s")+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)
#3
map3 <- ggplot() +
  geom_polygon(data = c2_df, aes(x = long, y = lat, group = group, fill = GYV_SK))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(5000, 10000, 20000, 50000, 100000, 250000, 500000),
                       trans = "log10",
                       name = "Gyventoj� skai�ius")+
  ggtitle("Rajon� ir miest� sav. atskirai")+
  theme_void()+
  theme(legend.position = "left")
#4
map4 <- ggplot() +
  geom_polygon(data = c1_df, aes(x = long, y = lat, group = group, fill = GYV_SK))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(5000, 10000, 20000, 50000, 100000, 250000, 500000),
                       trans = "log10",
                       name = "Gyventoj� skai�ius")+
  ggtitle("Rajon� ir miest� sav. kartu")+
  theme_void()+
  theme(legend.position = "left")
#5
map5 <- ggplot() +
  geom_polygon(data = c3_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(5000, 10000, 20000, 50000, 100000, 250000, 500000),
                       trans = "log10",
                       name = "Bals� skai�ius")+
  ggtitle("U� Naus�d�")+
  theme_void()+
  theme(legend.position = "left")
#6
map6 <- ggplot() +
  geom_polygon(data = c4_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(1000, 3000, 5000, 10000, 30000, 90000),
                       trans = "log10",
                       name = "Bals� skai�ius")+
  ggtitle("U� �imonyt�")+
  theme_void()+
  theme(legend.position = "left")
#7
map7 <- ggplot() +
  geom_polygon(data = c5_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       trans = "log10",
                       breaks = c(5000, 10000, 50000, 100000),
                       name = "Bals� skai�ius")+
  ggtitle("U� Naus�d� 2 turas")+
  theme_void()+
  theme(legend.position = "left")
#8
map8 <- ggplot() +
  geom_polygon(data = c5_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� Naus�d� 2 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#9
map9 <- ggplot() +
  geom_polygon(data = c6_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       trans = "log10",
                       breaks = c(5000, 10000, 50000, 100000),
                       name = "Bals� skai�ius")+
  ggtitle("U� �imonyt� 2 turas")+
  theme_void()+
  theme(legend.position = "left")
#10
map10 <- ggplot() +
  geom_polygon(data = c6_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� �imonyt� 2 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#11
map11 <- ggplot() +
  geom_polygon(data = c3_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� Naus�d� 1 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#12
map12 <- ggplot() +
  geom_polygon(data = c4_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� �imonyt� 1 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#13
map13 <- ggplot() +
  geom_polygon(data = c2_df, aes(x = long, y = lat, group = group, fill = LAIM))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  ggtitle("Laim�tos savivaldyb�s pagal kandidat�")+
  theme_void()+
  theme(legend.position = "left")

#14
map14 <- ggplot() + geom_polygon(data = dataf1, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
#15
map15 <- ggplot() +
  geom_polygon(data = d3_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(5000, 10000, 20000, 50000, 100000, 250000, 500000),
                       trans = "log10",
                       name = "Bals� skai�ius")+
  ggtitle("U� Naus�d�")+
  theme_void()+
  theme(legend.position = "left")
#16
map16 <- ggplot() +
  geom_polygon(data = d4_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       breaks = c(1000, 3000, 5000, 10000, 30000, 90000),
                       trans = "log10",
                       name = "Bals� skai�ius")+
  ggtitle("U� �imonyt�")+
  theme_void()+
  theme(legend.position = "left")
#17
map17 <- ggplot() +
  geom_polygon(data = d3_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� Naus�d� 1 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#18
map18 <- ggplot() +
  geom_polygon(data = d4_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� �imonyt� 1 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#19
map19 <- ggplot() +
  geom_polygon(data = d5_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       trans = "log10",
                       breaks = c(5000, 10000, 50000, 100000),
                       name = "Bals� skai�ius")+
  ggtitle("U� Naus�d� 2 turas")+
  theme_void()+
  theme(legend.position = "left")
#20
map20 <- ggplot() +
  geom_polygon(data = d6_df, aes(x = long, y = lat, group = group, fill = BALSU_VISO))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       na.value = "black",
                       trans = "log10",
                       breaks = c(5000, 10000, 50000, 100000),
                       name = "Bals� skai�ius")+
  ggtitle("U� �imonyt� 2 turas")+
  theme_void()+
  theme(legend.position = "left")
#21
map21 <- ggplot() +
  geom_polygon(data = d5_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� Naus�d� 2 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#22
map22 <- ggplot() +
  geom_polygon(data = d6_df, aes(x = long, y = lat, group = group, fill = PROC))+
  geom_text(data = cnames3, aes(x = long, y = lat, label = id), size = 2.5)+
  scale_fill_gradientn(colours = rev(rainbow(2)),
                       limits = c(0,1),
                       na.value = "black",
                       name = "Procentai")+
  ggtitle("U� �imonyt� 2 turas (proc.)")+
  theme_void()+
  theme(legend.position = "left")
#map23
c_pal <- colorNumeric("YlOrRd", domain = spdf1$GYV_SK)
map23 <- leaflet(spdf1) %>%
  addTiles() %>%
  addLegend(pal = c_pal, values = ~GYV_SK, opacity = 0.7, title = NULL, position = "bottomright") %>%
  addMarkers(lng=25.279652, lat=54.687157, popup="Vilnius") %>%
  addPolygons(
    fillColor = ~c_pal(GYV_SK),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))
#map24
d_pal <- colorFactor(c("Blues"), domain = spdf1$LAIM)
map24 <- leaflet(spdf1) %>%
  addTiles() %>%
  addMarkers(lng=25.279652, lat=54.687157, popup="Vilnius") %>%
  addLegend(pal = d_pal, values = ~LAIM, opacity = 0.7, title = NULL, position = "bottomright") %>%
  addPolygons(
    fillColor = ~d_pal(LAIM),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

#map25
cnames4 <- aggregate(cbind(long, lat) ~ id, data=df_fix, FUN=mean)
cnames4 <- merge(cnames4, sav_stat_id_ats, by = "id")

ddd_pal <- colorFactor(c("Set1"), domain = spdf1$LAIM)
map25 <- leaflet(spdf1) %>%
  addTiles() %>%
  addMarkers(lng=cnames4$long, lat=cnames4$lat, popup=cnames4$id) %>%
  addLegend(pal = ddd_pal, values = ~LAIM, opacity = 0.7, title = NULL, position = "bottomright") %>%
  addPolygons(
    fillColor = ~ddd_pal(LAIM),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

# 4. �em�lapi� per�i�ra

map1 #Savivalybi� kont�rai
map2 #Savivalybi� kont�rai su pavadinimais 

map3 #Gyventoj� skai�ius pagal savivaldybes r. ir m. atskirai
map4 #Gyventoj� skai�ius pagal savivaldybes r. ir m. kartu

map5 #Naus�da 1 turas 
map6 #�imonyt� 1 turas
map11 #Naus�da 1 turas procentai
map12 #�imonyt� 1 turas procentai

map13 #Laim�jo savivaldyb�je

map7 #Naus�da 2 turas
map9 #�imonyt� 2 turas
map8 #Naus�da 2 turas procentai
map10 #�imonyt� 2 turas procentai

map14 #Su ambasada
map15 #Naus�da 1 turas
map16 #�imonyt� 1 turas
map17 #Naus�da 1 turas procentai
map18 #�imonyt� 1 turas procentai

map19 #Naus�da 2 turas
map20 #�imonyt� 2 turas
map21 #Naus�da 2 turas procentai
map22 #�imonyt� 2 turas procentai

xmap #leaflets savivaldyb�s pagal gyventoj� skai�i�
map23 #leaflet savivaldyb�s pagal gyventoj� skai�i� su legenda/sakle
map24 #leaflet LAIM�JO SAVIVALDYB�JE
map25