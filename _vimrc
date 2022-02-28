set guicursor=
set nocompatible
syntax on
set smartindent
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab

set backspace=indent,eol,start

set termguicolors

set ttyfast     " faster redrawing
set lazyredraw  " only redraw when necessary

set scrolloff=999
set encoding=utf-8

" установка русской локали для вим
set langmenu=ru_ru
set helplang=ru,en

"set number " show line numbers:
"set relativenumber "номер строк относительно курсора
set number relativenumber
set wrap breakindent
set nowrap "added
set linebreak nolist
set textwidth=120
set wildmode=longest,list,full wildmenu

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

au BufRead,BufNewFile *.h set ft=cpp
au BufRead,BufNewFile *.cpp set ft=cpp
au BufRead,BufNewFile *.ini set ft=dosini

let s:is_win = (has('win32') || has('win64'))

if s:is_win
    set shell=cmd.exe
    set shellcmdflag=/c
    set encoding=utf-8
    "set shellslash
endif

if s:is_win
    let s:data_dir = expand('$LOCALAPPDATA/vimrc')
endif

" save and load views automaticly
set viewoptions=cursor,folds
let &viewdir = s:data_dir.'/view'
call mkdir(&viewdir,'p')
augroup vimrc
  autocmd BufWritePost *
        \ if expand('%') != '' && &buftype !~ 'nofile'
        \|  mkview
        \|endif
  autocmd BufRead *
        \ if expand('%') != '' && &buftype !~ 'nofile'
        \|  silent loadview
        \|endif
augroup END

let s:is_gui = has('gui_running')

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
noremap <leader>j gT
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
map <F5> :!g++ % -o %:r -static -lgdi32<CR> 
"compile and run
map <F9> <esc>:w <CR> :!g++ % -o %:r -static -lgdi32 && %:r <CR>
map <F10> :!g++ -o %:r % glad.c -lglfw3 -lopengl32 -lgdi32 -fpermissive -static && %:r <CR>

set incsearch
set ignorecase
set visualbell

"set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
if s:is_win
    set keymap=russian-jcukenwin
    set iminsert=0
    set imsearch=0
endif
""Попробуем повесить переключение раскладки в vim по winkey
"noremap! <C-Space> <C-^>

let g:XkbSwitchEnabled = 1
let g:XkbSwitchLib = 'c:\tools\vim\vim82\libxkbswitch64.dll'
let g:XkbSwitchIMappings = ['ru']
let g:XkbSwitchIMappingsTr = {
          \ 'ru':
          \ {'<': 'qwertyuiop[]asdfghjkl;''zxcvbnm,.`/'.
          \       'QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?~@#$^&|',
          \  '>': 'йцукенгшщзхъфывапролджэячсмитьбюё.'.
          \       'ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,Ё"№;:?/'}
          \ }
let g:XkbSwitchDynamicKeymap = 1
let g:XkbSwitchKeymapNames = {'ru': 'russian-jcukenwin'}


autocmd BufEnter * let b:XkbSwitchNLayout = 'us'
autocmd BufNewFile * let b:XkbSwitchNLayout = 'us'
"let g:XkbSwitchSkipIMappings = \ {'cpp' :['.', '>', ':', '{<CR>', '/*', '/*<CR>']}

if s:is_win && has('gui_runnung')
  language messages ru_RU.UTF-8
  set encoding=utf-8
endif

"добавляем плагины в vim
filetype plugin indent on

" test for org-mode vim
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

Plug 'junegunn/vim-journal'
Plug 'junegunn/goyo.vim'

Plug 'dkarter/bullets.vim'

Plug 'jceb/vim-orgmode' "the org-mode once
"helps plugins for orgmode
Plug 'vim-scripts/utl.vim'
Plug 'tpope/vim-repeat'
Plug 'yegappan/taglist'
Plug 'preservim/tagbar'
Plug 'tpope/vim-speeddating'
Plug 'chrisbra/NrrwRgn'
Plug 'mattn/calendar-vim'
Plug 'inkarkat/vim-SyntaxRange'

Plug 'tibabit/vim-templates' "templates for vim
call plug#end()

let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'text',
            \ 'gitcommit',
            \ 'scratch'
            \ ]

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
if s:is_gui
    set lines=35 columns=150
    if s:is_win
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


"for full screen gvim
if has('gui_running') && has('libcall')
  let g:MyVimLib = $VIMRUNTIME.'/gvimfullscreen.dll'
  function ToggleFullScreen()
    call libcallnr(g:MyVimLib, "ToggleFullScreen", 0)
  endfunction

  "Alt+Enter
  map <A-Enter> <Esc>:call ToggleFullScreen()<CR>

  let g:VimAlpha = 240
  function! SetAlpha(alpha)
    let g:VimAlpha = g:VimAlpha + a:alpha
    if g:VimAlpha < 180
      let g:VimAlpha = 180
    endif
    if g:VimAlpha > 255
      let g:VimAlpha = 255
    endif
    call libcallnr(g:MyVimLib, 'SetAlpha', g:VimAlpha)
  endfunction

  "Shift+Y
  nmap <s-y> <Esc>:call SetAlpha(3)<CR>
  "Shift+T
  nmap <s-t> <Esc>:call SetAlpha(-3)<CR>

  let g:VimTopMost = 0
  function! SwitchVimTopMostMode()
    if g:VimTopMost == 0
      let g:VimTopMost = 1
    else
      let g:VimTopMost = 0
    endif
    call libcallnr(g:MyVimLib, 'EnableTopMost', g:VimTopMost)
  endfunction
  "Shift+R
  nmap <s-r> <Esc>:call SwitchVimTopMostMode()<CR>
endif  

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

let mapleader = ","
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


"NERDTree toggle
nmap <F6> :NERDTreeToggle<CR>


let g:python3_host_prog='c:\Python38\python.exe'

function! TransparentBackground()
    highlight Normal guibg=NONE ctermbg=NONE
    highlight LineNr guibg=NONE ctermbg=NONE
    set fillchars+=vert:\|
    highlight VertSplit gui=NONE guibg=NONE guifg=#444444 cterm=NONE ctermbg=NONE ctermfg=gray
endfunction

augroup MyColors
"    autocmd ColorScheme * call TransparentBackground()
augroup END

"setup for orgmode
let g:org_todo_keywords=['TODO', 'NEXT', '|', 'DONE']
let g:org_todo_keyword_faces=[['NEXT', 'cyan']]
let g:org_agenda_files="d:\orgmode\index.org"

"для сохранения размеров окна и позиции
"set sessionoptions+=resize,winpos
"autocmd VIMEnter * :source C:/tmp/session.vim
"autocmd VIMLeave * :mksession! C:/tmp/session.vim
"Не сохраяет power line....
"
"some settings from: http://github.com/mhinz/vim-galore
"and http:/github.com/mhinz/dotfiles

" vim: sw=2 sts=2 tw=0 fdm=marker
