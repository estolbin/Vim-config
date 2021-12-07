set guicursor=
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
"set relativenumber "номер строк относительно курсора
set number relativenumber
"set wrap 
set nowrap "added
set linebreak nolist
set textwidth=120

"установка разных курсоров
" SI - режим вставки, SR - замена, EI - нормальный режим
" 1 - мигающий, 2 - обычный, 3 - мигающее подчеркивание, 
" 4 - просто подчеркивание, 5 - мигающая верт черта, 6 - просто вертикальная черта
set ttimeoutlen=10
let &t_SI.="\e[5 q" 
let &t_SR.="\e[3 q"
let &t_EI.="\e[1 q"

set showmatch "show matching bracket

set omnifunc=syntaxcomplete#Complete

set nofoldenable
set foldmethod=syntax

au BufRead,BufNewFile *.h set ft=c
au BufRead,BufNewFile *.cpp set ft=c
au BufRead,BufNewFile *.ini set ft=dosini

if(has('win32') || has('win64'))
    let g:isWin = 1
else
    let g:isWin = 0
endif

if has('gui_running')
    let g:isGUI = 1
else
    let g:isGUI = 0
endif

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
"noremap <A-j> gT
noremap <leader>j gT
"noremap <A-k> gt
noremap <leader>k gt

" Show line number, cursor position.
set ruler
" Display incomplete commands.
set showcmd
" To insert timestamp, press F3.
"nmap <F3> a<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
"imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR> 

"For c++ compile
"autocmd BufNewFile *.cpp execute "0r ~/.vim/template/".input("Template name: ").".cpp"
"compile
map <F5> :!g++ % -o %:r -static <CR> 
"compile and run
map <F9> :!g++ % -o %:r -static && %:r <CR>

set incsearch
set ignorecase
set visualbell

"set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
""if has('win32') || has('win64') || has('win16')
if g:isWin
    set keymap=russian-jcukenwin
    set iminsert=0
    set imsearch=0
endif
""Попробуем повесить переключение раскладки в vim по winkey
"noremap! <C-Space> <C-^>

let g:XkbSwitchEnabled = 1
let g:XkbSwitchIMappings = ['ru']
let g:XkbSwitchSkipIMappings =
    \ {'cpp' :['.', '>', ':', '{<CR>', '/*', '/*<CR>']}

"добавляем плагины в vim
filetype plugin indent on
set encoding=utf-8

call plug#begin('C:\Users\stolbin.es\_vim\bundle')
Plug 'ErichDonGubler/vim-sublime-monokai'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline-themes'
Plug 'aserebryakov/vim-todo-lists'
Plug 'preservim/nerdtree'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'lyokha/vim-xkbswitch'
call plug#end()

"colorscheme sublimemonokai
colorscheme ayu

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

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

let g:airline_powerline_fonts = 1
let g:airline#extensions#keymap#enabled = 0
let g:airline_section_z = "\ue0a1:%l/%L Col:%c"
let g:Powerline_symbols='unicode'
let g:airline#extensions#xkblayout#enabled=0
"let g:airline_theme='molokai'
let g:airline_theme='ayu_dark'

"noremap <leader>ld :LivedownToggle<CR>

"for todo-list
let g:VimTodoListsCustomsKeyMapper = 'VimTodoListsCustomMappings'

function! VimTodoListsCustomMappings()
    nnoremap <buffer> s :VimTodoListsToggleItem<CR>
    nnoremap <buffer> <Space> :VimTodoListsToggleItem<CR>
    noremap <buffer> <leader>e :silent call VimTodoListsSetItemMode()<CR>
endfunction

"Dates to todo list
let g:VimTodoListsDatesEnabled = 1

"if has("gui_running")
if g:isGUI
    set lines=35 columns=150
    if g:isWin
"        au GUIEnter * sim ~x
    endif
else
    if exists("+lines")
        set lines=50
    endif
    if exists("+columns")
        set columns=100
    endif
endif

"autocmd VimEnter * call libcallnr("gvimfullscreen_64.dll", "ToggleFullScreen", 1)

map <F11> <Esc>:call libcallnr("gvimfullscreen_64.dll", "ToggleFullScreen", 0)<CR>

set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

set guifont=Source\ Code\ Pro\ for\ Powerline:h10:cANSI

"abbreviation for lines
iab <expr> -- repeat('-', 80)

"редактирование .vimrc
nnoremap ,v :source $MYVIMRC<CR>
nnoremap ,e :edit $MYVIMRC<CR>

"my simple todo in markdown
"inoremap ,td - [ ]
nnoremap <leader>td i - [ ]<Esc>

"перенос строк
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

"скобки
inoremap { {}<Left>
inoremap {<CR> {<CR>}<Esc>O
inoremap {{ }
inoremap {} {}
inoremap ( ()<Left>
autocmd FileType html, htm, xml inoremap < <><Left>

let mapleader = " "
nnoremap <leader>pv :Vex<CR>
nnoremap <leader><CR> :so $MYVIMRC<CR>
nnoremap <C-p> :GFiles<CR>
nnoremap <leader>pf :Files<CR>
nnoremap <C-j> :cnext<CR>
nnoremap <C-k> :cpreg<CR>
vnoremap <leader>p "_dP
vnoremap <leader>y "+y
" Копирование в системный буфер обмена
nnoremap <leader>y "+y
" Копирование всего файла в системный буфер обмена
nnoremap <leader>Y gg"+yG

" Перемещение строк
"vnoremap J :m '>+1<CR>gv=gv
"vnoremap K :m '<-2<CR>gv=gv

"NERDTree toggle
nmap <F6> :NERDTreeToggle<CR>

"для сохранения размеров окна и позиции
"set sessionoptions+=resize,winpos
"autocmd VIMEnter * :source C:/tmp/session.vim
"autocmd VIMLeave * :mksession! C:/tmp/session.vim
"Не сохраяет power line....
