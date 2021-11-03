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
