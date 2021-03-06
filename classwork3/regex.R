#Загрузите данные о землятресениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)
#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
summa <- grep ("^\\d+/\\d+/\\d+\\s\\d+:\\d+:\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,\\w+,\\d+,,,-?\\d+\\.\\d+,\\w+,\\d+$", anss, value=TRUE)
summa
#Проверьте что все строки (all.equal) в результирующем векторе подходят под шаблон. 
all.equal(summa,"^\\d+/\\d+/\\d+\\s\\d+:\\d+:\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,-?\\d+\\.\\d+,\\w+,\\d+,,,-?\\d+\\.\\d+,\\w+,\\d+$")
