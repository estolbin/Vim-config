set nocompatible
syntax on
set smartindent
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab

set scrolloff=999
set encoding=utf-8

"set number " show line numbers:
set relativenumber "номер строк относительно курсора
set wrap linebreak nolist
set textwidth=80
set cursorline

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

"if has("gui_running")
"  eolorscheme desert
"else
"  colorscheme darkblue
"endif

" Show line number, cursor position.
set ruler
" Display incomplete commands.
set showcmd
" To insert timestamp, press F3.
nmap <F3> a<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR> 

" Status line
"set laststatus=2
"set statusline=
"set statusline+=%-3.3n\ " buffer number
"set statusline+=%f\ " filename
"set statusline+=%h%m%r%w " status flags
"set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
"set statusline+=%= " right align remainder
"set statusline+=0x%-8B " character value
"set statusline+=%-14(%l,%c%V%) " line, character
"set statusline+=%<%P " file position
"
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

"добавляем плагины в vim
filetype plugin indent on
set encoding=utf-8
"set nocompatible
"syntax enable

call plug#begin('C:\Users\stolbin.es\_vim\bundle')
Plug 'ErichDonGubler/vim-sublime-monokai'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'
Plug 'shime/vim-livedown'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

colorscheme sublimemonokai


if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.colnr = ' ㏇:'
let g:airline_symbols.colnr = ' ℅:'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = ' ␊:'
let g:airline_symbols.linenr = ' ␤:'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.maxlinenr = '㏑'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = 'Ɇ'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.colnr = ' :'
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ' :'
let g:airline_symbols.maxlinenr = '☰ '
let g:airline_symbols.dirty='⚡'

" old vim-powerline symbols
"let g:airline_left_sep = '⮀'
"let g:airline_left_alt_sep = '⮁'
"let g:airline_right_sep = '⮂'
"let g:airline_right_alt_sep = '⮃'
"let g:airline_symbols.branch = '⭠'
"let g:airline_symbols.readonly = '⭤'
"let g:airline_symbols.linenr = '⭡'

let g:airline_powerline_fonts = 1
let g:airline#extensions#keymap#enabled = 0
let g:airline_section_z = "\ue0a1:%l/%L Col:%c"
let g:Powerline_symbols='unicode'
let g:airline#extensions#xkblayout#enabled=0
let g:airline_theme='molokai'
"let g:airline_left_sep='>'
"let g:airline_right_sep='<'

noremap <leader>ld :LivedownToggle<CR>


"set guifont=Hack:h16
set guifont=Source\ Code\ Pro\ for\ Powerline:h10:cANSI
