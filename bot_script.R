library(googlesheets4)
library(telegram.bot)
library(ggplot2)


# gs4_auth('') # авторизация к акку гугла на котором создана таблица
# dox <- gs4_create(name = "cafe budget",
#                   locale = "ru_RU") # создание таблицы при необходимости
# https://docs.google.com/spreadsheets/d/1s-hjeXCR4EaHV6h3_WfHRD_7PK4m-1do6_N2C0brCi8/edit#gid=0


bot <- Bot(token = "5698100631:AAFNE_rgLXRAzxLrhKwW9nF168_r9TfEirU")
# подключение к боту
chat_id <- unname(unlist((bot$getMe())[1]))
updates <- bot$getUpdates()

# создаём экземпляр класса Updater - основной класс обработки
updater <- Updater('5698100631:AAFNE_rgLXRAzxLrhKwW9nF168_r9TfEirU')

## метод для запуска 
start <- function(bot, update) {
  # отправляем приветсвие, сишнализирующее о начале работы
  bot$sendMessage(update$message$chat_id,
                  text = paste0(update$message$from$first_name, ', жду команд!'))
}
 
## Метод вызова обработки передаваемых данных
data <- function(bot, update, args){
  # Проверка взодящих значений
  if ((is.na(as.numeric(args[1]))) || (is.na(as.numeric(args[2]))) || length(args) < 2)
  {
    bot$sendMessage(chat_id = update$message$chat_id, 
                  text = "Расход/касса указаны неверно")
  }
  else
  {
    costs <- as.numeric(args[1])
    income <- as.numeric(args[2])
    coms <-  NA
    if(length(args) > 2)
    {
      coms <- paste(as.list(args[3:length(args)]), collapse = " ")
    }
    bot$sendMessage(chat_id = update$message$chat_id, 
                    text = "Данные получены!")
    
    gs4_auth('')
    dox <- as_sheets_id("1s-hjeXCR4EaHV6h3_WfHRD_7PK4m-1do6_N2C0brCi8") # подключение к таблице
    data <- data.frame(Date = Sys.Date(),
                       Costs = costs,
                       Income = income,
                       Coms = coms)
    
    sheet_append(data = data, dox, sheet = 'Accounting') # внесение данных в таблицу
    
    bot$sendMessage(chat_id = update$message$chat_id, 
                    text = "Данные успешно записаны!")
  }
}

report <- function(bot, update){
  gs4_auth('ivantrushin98@gmail.com')#Автоматическое подключение к аккаунту-собственнику
  dox <- as_sheets_id("1s-hjeXCR4EaHV6h3_WfHRD_7PK4m-1do6_N2C0brCi8") # подключение к таблице
  data <- range_read(dox, sheet = 'Accounting', col_names = TRUE)

  plot <-  ggplot(data = data, aes(x = data$Дата, y = (data$Касса - data$Расход)), ) +
                    geom_line() + 
                    geom_point() +
                    labs(title = "Выручка за всё время", 
                         x = "День",
                         y = "Выручка") + geom_smooth()
  
  bot$send_photo(chat_id = update$message$chat_id,
                 photo = ggsave("plot.png"))
  getwd()
  unlink("plot.png")
  
}

h_error <-  function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, 
                  text = "Я не знаю, что делать с этой коммандой :(")
}
  
  
MessageFilters$err <- BaseFilter(function(message) {
  
  # проверяем, встречается ли в тексте сообщения слова: привет, здравствуй, салют, хай, бонжур
 !grepl(x           = message$text, 
        pattern     = '/start|/report|/data',
        ignore.case = TRUE)
}
)


# создаём обработчики
h_start <- CommandHandler('start', start)
h_data <- CommandHandler('data', data, pass_args = TRUE)
h_report <- CommandHandler('report', report)
h_error <- MessageHandler(h_error, filters = MessageFilters$err)

# добавляем обработчики в диспетчер
updater <- updater +
    h_start +
    h_data +
    h_report +
    h_error

# запускаем бота
updater$start_polling()