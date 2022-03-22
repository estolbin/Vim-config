# Vim-config 

Содержит кофигурационные файлы для vim с поддержкой кириллицы.


файл .vrapperrc - содержит настройки для плагина vrapper. Для EDT.

При текущих настройках \_vimrc - файл конфигурации \_gvimrc не нужен. Все
данные прописаны в основном файле.

Чтобы все работало: расширение plug.vim надо скачать вручную и положить в
папку autoload, которая находится в каталоге с установленным vim.
После скачивания в vim запускаем команду PlugInstall.
[Ссылка на плагин](https://github.com/junegunn/vim-plug)

Чтобы работало отображение airline - надо добавить шрифты. Скачиваем шрифты,
пропатченные под powerline. [Ссылка на шрифты](https://github.com/powerline/fonts)

Добавил папку с пакетным файлом для конвертации с помощью pandoc.



## Установка gvim как редактора по умолчанию для коммитов

`git config --global core.editor "c:\tools\vim\vim82\gvim.exe' -f -i NONE"`

Предполагается что `gvim` установлен в папку `c:\tools\vim`

## Добавил переключение раскладки с помощью xkbswitch

Сначала устанавливаем [xkbswitch](https://github.com/DeXP/xkb-switch-win) для Windows
Потом добавляем плагин в .vimrc

## vim\_fundamentals
Курс по vim - оттуда пример настройки 


## Дополнения для русской раскладки
>Дело осталось за малым: надо установить в системе переменную окружения LANG. 
>В создании новой переменной нет ничего сложного, но на всякий случай привожу инструкцию. 
>Заходим в настройки системы `Win+Break -> дополнительные параметры системы -> Дополнительно -> Переменные среды -> Создать…`
>Вводим имя переменной LANG, значение ru_RU.UTF-8.
>
>Если по каким-то причинам вы не можете создавать переменные окружения, тогда можно добавить несколько строк в _vimrc:

set encoding=utf-8

" Отображение кириллицы во внутренних сообщениях программы
lan mes ru_RU.UTF-8

" Отображение кириллицы в меню
source $VIMRUNTIME/delmenu.vim
set langmenu=ru_RU.UTF-8
source $VIMRUNTIME/menu.vim
