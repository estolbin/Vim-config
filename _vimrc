set nocompatible
syntax on
set smartindent
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab

set scrolloff=999
set encoding=utf-8


set number " show line numbers:
noremap <SPACE> <C-F>
set diffopt+=vertical
" To save with Ctrl S
nmap <C-S> :w<CR>
imap <C-S> <Esc>:w<CR>

" Spell check
function! ToggleSpell()
  if !exists("b:spell")
    setlocal spell spelllang=ru_RU
    let b:spell = 1
  else
    setlocal nospell
    unlet b:spell
  endif
endfunction

nmap <F4> :call ToggleSpell()<CR>
imap <F4> <Esc>:call ToggleSpell()<CR>

" shortcuts for moving between tabs
noremap <A-j> gT
noremap <A-k> gt

if has("gui_running")
  colorscheme desert
else
  colorscheme darkblue
endif

" Show line number, cursor position.
set ruler
" Display incomplete commands.
set showcmd
" To insert timestamp, press F3.
nmap <F3> a<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR> 

" Status line
set laststatus=2
set statusline=
set statusline+=%-3.3n\ " buffer number
set statusline+=%f\ " filename
set statusline+=%h%m%r%w " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
set statusline+=%= " right align remainder
set statusline+=0x%-8B " character value
set statusline+=%-14(%l,%c%V%) " line, character
set statusline+=%<%P " file position

set incsearch
set ignorecase
set visualbell

set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
"set keymap=russian-jcukenwin    " настраиваем переключение раскладок клавиатуры по C-^
"set iminsert=0                  " раскладка по умолчанию для ввода - английская
"set imsearch=0                  " раскладка по умолчанию для поиска - английская
"
"" переключение на русскую/английскую раскладку по ^f (Ctrl + F)
"cmap <silent> <C-F> <C-^>
"imap <silent> <C-F> <C-^>X<Esc>:call MyKeyMapHighlight()<CR>a<C-H>
"nmap <silent> <C-F> a<C-^><Esc>:call MyKeyMapHighlight()<CR>
"vmap <silent> <C-F> <Esc>a<C-^><Esc>:call MyKeyMapHighlight()<CR>gv
"
"" Переключение раскладок и индикация выбранной в данный момент раскладки -->
"" При английской раскладке статусная строка текущего окна будет синего цвета, а при русской - красного
"function MyKeyMapHighlight()
"	if &iminsert == 0
"		hi StatusLine ctermfg=DarkBlue guifg=DarkBlue
"    else
"        hi StatusLine ctermfg=DarkRed guifg=DarkRed
"    endif
"endfunction
"" Вызываем функцию, чтобы она установила цвета при запуске Vim'a
"call MyKeyMapHighlight()
"" При изменении активного окна будет выполняться обновление индикации текущей раскладки
"au WinEnter * :call MyKeyMapHighlight()
"" <--
