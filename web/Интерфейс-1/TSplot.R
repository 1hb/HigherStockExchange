#Функция TSplot

TSplot <- function(class, starttime, endtime, asset, period, save){
  require(data.table)
  require(dplyr)
  require(ggplot2)
  require(tidyquant)
  #Проверка на наличие orderlog в рабочем пространстве
  if(exists("orderlog") == FALSE){
    print("Укажите путь к файлу orderlog на своем ПК")
    orderlog <- fread(file.choose())
    #Данные импортировались, продолжаем инициацию функции: проверим заданы ли переменные starttime, endtime и period
    if (missing(starttime)){
      starttime = min(orderlog$TIME)
    } else {
      starttime = starttime
    }
    if (missing(endtime)){
      endtime = max(orderlog$TIME)
    } else {
      endtime = endtime
    }
    if (missing(period)){
      period = "M"
    } else {
      period = period
    }
    if (missing(save)){
      save = "no"
    } else {
      save = save
    }
    #Проверка класса графика, который необходим пользователю
    if (class == "cschart"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        #Создадим новую фиктивную таблицу - копию orderlog в локальном пространстве (чтобы повторная инициация функции не нарушалась при задании другой периодичности)
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу со свечами заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, TRADEPRICE, VOLUME) %>%
          group_by(SECCODE, TIME) %>%
          summarise(OPEN = TRADEPRICE[1], HIGH = max(TRADEPRICE), LOW = min(TRADEPRICE), CLOSE = tail(TRADEPRICE, 1), VOLUME1 = sum(VOLUME))
        colnames(dataframe) <- c("SECCODE","TIME","OPEN","LOW","HIGH","CLOSE","VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("Свечной график", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = dataframe %>%
          ggplot(aes(x = TIME, y = CLOSE)) +
          geom_candlestick(aes(open = OPEN, high = HIGH, low = LOW, close = CLOSE),
                           colour_up = "darkgreen", colour_down = "darkred",
                           fill_up = "darkgreen", fill_down = "darkred") +
          labs(title = title1,
               y = "Цена, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу со свечами заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, TRADEPRICE, VOLUME) %>%
          group_by(SECCODE, TIME) %>%
          summarise(OPEN = TRADEPRICE[1], HIGH = max(TRADEPRICE), LOW = min(TRADEPRICE), CLOSE = tail(TRADEPRICE, 1), VOLUME1 = sum(VOLUME))
        colnames(dataframe) <- c("SECCODE","TIME","OPEN","LOW","HIGH","CLOSE","VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("Свечной график", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = dataframe %>%
          ggplot(aes(x = TIME, y = CLOSE)) +
          geom_candlestick(aes(open = OPEN, high = HIGH, low = LOW, close = CLOSE),
                           colour_up = "darkgreen", colour_down = "darkred",
                           fill_up = "darkgreen", fill_down = "darkred") +
          labs(title = title1,
               y = "Цена, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "tradeprice"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        #Создадим новую фиктивную таблицу - копию orderlog в локальном пространстве (чтобы повторная инициация функции не нарушалась при задании другой периодичности)
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1))
        colnames(dataframe) <- c("SECCODE","TIME","TRADEPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График рыночной цены", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADEPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADEPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Рыночная цена, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1))
        colnames(dataframe) <- c("SECCODE","TIME","TRADEPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График рыночной цены", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADEPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADEPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Рыночная цена, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "returns"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1)) %>%
          mutate(RETURNS = c(0, log(tail(TRADEPRICE, -1)/head(TRADEPRICE, -1)))) %>%
          slice(2:nrow(dataframe))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График доходности", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = RETURNS), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = RETURNS), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Доходность, %",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1)) %>%
          mutate(RETURNS = c(0, log(tail(TRADEPRICE, -1)/head(TRADEPRICE, -1)))) %>%
          slice(2:nrow(dataframe))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График доходности", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = RETURNS), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = RETURNS), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Доходность, %",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "ask"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","ASKPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены продажи", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = ASKPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = ASKPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена продажи, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","ASKPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены продажи", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = ASKPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = ASKPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена продажи, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "bid"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","BIDPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены покупки", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = BIDPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = BIDPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена покупки, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","BIDPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены покупки", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = BIDPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = BIDPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена покупки, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "spread"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe1 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe1) <- c("SECCODE","TIME","ASKPRICE")
        dataframe1$SECCODE <- NULL
        dataframe2 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe2) <- c("SECCODE","TIME","BIDPRICE")
        dataframe2$SECCODE <- NULL
        dataframe = inner_join(dataframe1, dataframe2, by = c("TIME" = "TIME"))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        dataframe <- dataframe %>%
          mutate(SPREAD = ASKPRICE - BIDPRICE)
        #Сформируем требуемый график
        title1 = paste("График bid-ask спреда", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = SPREAD), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = SPREAD), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Величина спреда, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe1 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe1) <- c("SECCODE","TIME","ASKPRICE")
        dataframe1$SECCODE <- NULL
        dataframe2 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe2) <- c("SECCODE","TIME","BIDPRICE")
        dataframe2$SECCODE <- NULL
        dataframe = inner_join(dataframe1, dataframe2, by = c("TIME" = "TIME"))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        dataframe <- dataframe %>%
          mutate(SPREAD = ASKPRICE - BIDPRICE)
        #Сформируем требуемый график
        title1 = paste("График bid-ask спреда", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = SPREAD), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = SPREAD), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Величина спреда, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "trades"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME) %>%
          group_by(TIME) %>%
          summarise(TRADES = length(TIME))
        colnames(dataframe) <- c("TIME", "TRADES")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График частоты торгов по", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADES), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADES), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Количество сделок",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME) %>%
          group_by(TIME) %>%
          summarise(TRADES = length(TIME))
        colnames(dataframe) <- c("TIME", "TRADES")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График частоты торгов по", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADES), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADES), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Количество сделок",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "vol"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME, VOLUME) %>%
          group_by(TIME) %>%
          summarise(VOLUME = sum(VOLUME))
        colnames(dataframe) <- c("TIME", "VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График объема торгов", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = VOLUME), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = VOLUME), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Объем торгов.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME, VOLUME) %>%
          group_by(TIME) %>%
          summarise(VOLUME = sum(VOLUME))
        colnames(dataframe) <- c("TIME", "VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График объема торгов", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = VOLUME), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = VOLUME), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Объем торгов.",
               x = "Время") +
          theme_tq()
      }
    }
  } else {
    if (missing(starttime)){
      starttime = min(orderlog$TIME)
    } else {
      starttime = starttime
    }
    if (missing(endtime)){
      endtime = max(orderlog$TIME)
    } else {
      endtime = endtime
    }
    if (missing(period)){
      period = "M"
    } else {
      period = period
    }
    if (missing(save)){
      save = "no"
    } else {
      save = save
    }
    #Проверка класса графика, который необходим пользователю
    if (class == "cschart"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        #Создадим новую фиктивную таблицу - копию orderlog в локальном пространстве (чтобы повторная инициация функции не нарушалась при задании другой периодичности)
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу со свечами заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, TRADEPRICE, VOLUME) %>%
          group_by(SECCODE, TIME) %>%
          summarise(OPEN = TRADEPRICE[1], HIGH = max(TRADEPRICE), LOW = min(TRADEPRICE), CLOSE = tail(TRADEPRICE, 1), VOLUME1 = sum(VOLUME))
        colnames(dataframe) <- c("SECCODE","TIME","OPEN","LOW","HIGH","CLOSE","VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("Свечной график", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = dataframe %>%
          ggplot(aes(x = TIME, y = CLOSE)) +
          geom_candlestick(aes(open = OPEN, high = HIGH, low = LOW, close = CLOSE),
                           colour_up = "darkgreen", colour_down = "darkred",
                           fill_up = "darkgreen", fill_down = "darkred") +
          labs(title = title1,
               y = "Цена, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу со свечами заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, TRADEPRICE, VOLUME) %>%
          group_by(SECCODE, TIME) %>%
          summarise(OPEN = TRADEPRICE[1], HIGH = max(TRADEPRICE), LOW = min(TRADEPRICE), CLOSE = tail(TRADEPRICE, 1), VOLUME1 = sum(VOLUME))
        colnames(dataframe) <- c("SECCODE","TIME","OPEN","LOW","HIGH","CLOSE","VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("Свечной график", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = dataframe %>%
          ggplot(aes(x = TIME, y = CLOSE)) +
          geom_candlestick(aes(open = OPEN, high = HIGH, low = LOW, close = CLOSE),
                           colour_up = "darkgreen", colour_down = "darkred",
                           fill_up = "darkgreen", fill_down = "darkred") +
          labs(title = title1,
               y = "Цена, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "tradeprice"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        #Создадим новую фиктивную таблицу - копию orderlog в локальном пространстве (чтобы повторная инициация функции не нарушалась при задании другой периодичности)
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1))
        colnames(dataframe) <- c("SECCODE","TIME","TRADEPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График рыночной цены", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADEPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADEPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Рыночная цена, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1))
        colnames(dataframe) <- c("SECCODE","TIME","TRADEPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График рыночной цены", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADEPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADEPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Рыночная цена, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "returns"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1)) %>%
          mutate(RETURNS = c(0, log(tail(TRADEPRICE, -1)/head(TRADEPRICE, -1)))) %>%
          slice(2:length(TIME))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График доходности", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = RETURNS), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = RETURNS), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Доходность, %",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, TRADEPRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(TRADEPRICE = tail(TRADEPRICE, 1)) %>%
          mutate(RETURNS = c(0, log(tail(TRADEPRICE, -1)/head(TRADEPRICE, -1)))) %>%
          slice(2:length(TIME))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График доходности", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = RETURNS), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = RETURNS), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Доходность, %",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "ask"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","ASKPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены продажи", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = ASKPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = ASKPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена продажи, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","ASKPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены продажи", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = ASKPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = ASKPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена продажи, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "bid"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","BIDPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены покупки", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = BIDPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = BIDPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена покупки, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe) <- c("SECCODE","TIME","BIDPRICE")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График лучшей цены покупки", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = BIDPRICE), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = BIDPRICE), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Цена покупки, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "spread"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe1 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "S" & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe1) <- c("SECCODE","TIME","ASKPRICE")
        dataframe1$SECCODE <- NULL
        dataframe2 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe2) <- c("SECCODE","TIME","BIDPRICE")
        dataframe2$SECCODE <- NULL
        dataframe = inner_join(dataframe1, dataframe2, by = c("TIME" = "TIME"))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        dataframe <- dataframe %>%
          mutate(SPREAD = ASKPRICE - BIDPRICE)
        #Сформируем требуемый график
        title1 = paste("График bid-ask спреда", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = SPREAD), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = SPREAD), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Величина спреда, руб.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe1 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & TIME >= starttime & TIME <= endtime & PRICE > 0) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(ASKPRICE = min(PRICE))
        colnames(dataframe1) <- c("SECCODE","TIME","ASKPRICE")
        dataframe1$SECCODE <- NULL
        dataframe2 <- testlog %>%
          filter(ACTION == 1 & SECCODE == asset & BUYSELL == "B" & TIME >= starttime & TIME <= endtime) %>%
          select(SECCODE, TIME, PRICE) %>%
          group_by(SECCODE, TIME) %>%
          summarise(BIDPRICE = max(PRICE))
        colnames(dataframe2) <- c("SECCODE","TIME","BIDPRICE")
        dataframe2$SECCODE <- NULL
        dataframe = inner_join(dataframe1, dataframe2, by = c("TIME" = "TIME"))
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        dataframe <- dataframe %>%
          mutate(SPREAD = ASKPRICE - BIDPRICE)
        #Сформируем требуемый график
        title1 = paste("График bid-ask спреда", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = SPREAD), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = SPREAD), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Величина спреда, руб.",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "trades"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME) %>%
          group_by(TIME) %>%
          summarise(TRADES = length(TIME))
        colnames(dataframe) <- c("TIME", "TRADES")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График частоты торгов по", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADES), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADES), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Количество сделок",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME) %>%
          group_by(TIME) %>%
          summarise(TRADES = length(TIME))
        colnames(dataframe) <- c("TIME", "TRADES")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График частоты торгов по", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = TRADES), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = TRADES), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Количество сделок",
               x = "Время") +
          theme_tq()
      }
    }
    if (class == "vol"){
      #Проверка необходимой пользователю периодичности
      if (period == "H"){
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 2)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME, VOLUME) %>%
          group_by(TIME) %>%
          summarise(VOLUME = sum(VOLUME))
        colnames(dataframe) <- c("TIME", "VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H"))
        #Сформируем требуемый график
        title1 = paste("График объема торгов", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = VOLUME), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = VOLUME), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Объем торгов.",
               x = "Время") +
          theme_tq()
      } else {
        testlog <- orderlog
        testlog$TIME <- substr(testlog$TIME, 1, 4)
        #Сформируем таблицу с данными заданной периодичности
        dataframe <- testlog %>%
          filter(ACTION == 2 & SECCODE == asset & TIME >= starttime & TIME <= endtime) %>%
          select(TIME, VOLUME) %>%
          group_by(TIME) %>%
          summarise(VOLUME = sum(VOLUME))
        colnames(dataframe) <- c("TIME", "VOLUME")
        dataframe$TIME <- as.POSIXct(strptime(as.character(dataframe$TIME), format = "%H%M"))
        #Сформируем требуемый график
        title1 = paste("График объема торгов", asset, "в период с", starttime, "по", endtime, "с", period, "периодичностью")
        plot = ggplot(dataframe) +
          geom_line(aes(x = TIME, y = VOLUME), color = "deepskyblue3") +
          geom_point(aes(x = TIME, y = VOLUME), color = "dodgerblue4", size = 0.2) +
          labs(title = title1,
               y = "Объем торгов.",
               x = "Время") +
          theme_tq()
      }
    }
  }
  if (save == "yes"){
    ggsave("TSplot.pdf")
    output1 = paste(getwd(),"/TSplot.pdf", sep = "")
    output = paste("Полученный график сохранен в директории", output1)
    print(output)
  }
  return(plot)
}
