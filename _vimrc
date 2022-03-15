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
Plug 'tibabit/vim-templates' "templates for vim
"Outliner (test)
Plug 'nvie/vim-rst-tables'
Plug 'insanum/votl'
" Plugins from Bryan Jenks
Plug 'vim-scripts/restore_view.vim'
Plug 'vimwiki/vimwiki', {'branch':'dev'}
Plug 'godlygeek/tabular'
call plug#end()

let items = ["<bar>", "\\", "/", ":", ".", "*", "_" ]
for item in items
  exe "nnoremap yi".item." T".item."yt".item
  exe "nnoremap ya".item." F".item."yf".item
  exe "nnoremap ci".item." T".item."ct".item
  exe "nnoremap ca".item." F".item."cf".item
  exe "nnoremap di".item." T".item."dt".item
  exe "nnoremap da".item." F".item."df".item
  exe "nnoremap vi".item." T".item."vt".item
  exe "nnoremap va".item." F".item."vf".item
endfor

set guicursor=
set nocompatible
filetype plugin on
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
set number relativenumber
set wrap breakindent
set nowrap "added
set linebreak nolist
set textwidth=120
set wildmode=longest,list,full wildmenu
set incsearch
set ignorecase
set visualbell
set showmatch "show matching bracket
set omnifunc=syntaxcomplete#Complete
" set foldmethod=syntax
set foldmethod=indent
set foldlevel=99
set diffopt+=vertical
" To save with Ctrl S
" Show line number, cursor position.
set ruler
" Display incomplete commands.
set showcmd

set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

set guifont=Source\ Code\ Pro\ for\ Powerline:h10:cANSI
"установка разных курсоров
" SI - режим вставки, SR - замена, EI - нормальный режим
" 1 - мигающий, 2 - обычный, 3 - мигающее подчеркивание, 
" 4 - просто подчеркивание, 5 - мигающая верт черта, 6 - просто вертикальная черта
set ttimeoutlen=10
let &t_SI.="\e[5 q" 
let &t_SR.="\e[3 q"
let &t_EI.="\e[1 q"

nmap <C-S> :w<CR>
imap <C-S> <Esc>:w<CR>
" shortcuts for moving between tabs
noremap <leader>j gT
noremap <leader>k gt

set splitbelow splitright
" shortcutting split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" open new split in a semantic way
nnoremap c<C-h> :lefta vsp new<CR>
nnoremap c<C-j> :bel sp new<CR>
nnoremap c<C-k> :abo sp new<CR>
nnoremap c<C-l> :rightb vsp new<CR>
" closing splits in a same way
nnoremap <C-q> :q<CR>
nnoremap <S-Q> :only<CR>

"compile
map <F5> :!g++ % -o %:r -static -lgdi32<CR> 
"compile and run
map <F9>  :!g++ % -o %:r -static -lgdi32 && %:r <CR>
"compile and run for OpenGL
map <F10> :!g++ -o %:r % glad.c -lglfw3 -lopengl32 -lgdi32 -fpermissive -static && %:r <CR>

au BufRead,BufNewFile *.h set ft=cpp
au BufRead,BufNewFile *.cpp set ft=cpp
au BufRead,BufNewFile *.ini set ft=dosini

let s:is_win = (has('win32') || has('win64'))
let s:is_gui = has('gui_running')

if s:is_win
    set shell=cmd.exe
    set shellcmdflag=/c
    set encoding=utf-8
    "set shellslash
    let s:data_dir = expand('$LOCALAPPDATA/vimrc')
endif

if s:is_win && s:is_gui
  language messages ru_RU.UTF-8
  set encoding=utf-8
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

" noremap <SPACE> <C-F>

" Spell check {{{
function! ToggleSpell()
  if !exists("b:spell")
    setlocal spell spelllang=ru_RU
    let b:spell = 1
  else
    setlocal nospell
    unlet b:spell
  endif
endfunction
"}}}

nmap <F4> :call ToggleSpell()<CR>
imap <F4> <Esc>:call ToggleSpell()<CR>

" To insert timestamp, press F3.
"nmap <F3> a<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
"imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR> 

" Commenting blocks of code.
augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
  autocmd FileType sh,ruby,python   let b:comment_leader = '# '
  autocmd FileType conf,fstab       let b:comment_leader = '# '
  autocmd FileType tex              let b:comment_leader = '% '
  autocmd FileType mail             let b:comment_leader = '> '
  autocmd FileType vim              let b:comment_leader = '" '
augroup END
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>      


"set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
if s:is_win
    set keymap=russian-jcukenwin
    set iminsert=0
    set imsearch=0
endif

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

" autocmd BufEnter * let b:XkbSwitchNLayout = 'us'
autocmd BufEnter * let b:XkbSwitchNLayout = '1'
" autocmd BufNewFile * let b:XkbSwitchNLayout = 'us'
autocmd BufNewFile * let b:XkbSwitchNLayout = '1'
"let g:XkbSwitchSkipIMappings = \ {'cpp' :['.', '>', ':', '{<CR>', '/*', '/*<CR>']}


"добавляем плагины в vim


" let g:bullets_enabled_file_types = [
"             \ 'markdown',
"             \ 'text',
"             \ 'gitcommit',
"             \ 'scratch'
"             \ ]

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

if s:is_gui
    set lines=35 columns=150
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


"abbreviation for lines
iab <expr> -- repeat('-', 80)

"редактирование .vimrc
" nnoremap <leader>v :source $MYVIMRC<CR>
autocmd BufWritePost _vimrc source %
nnoremap <leader>e :edit $MYVIMRC<CR>

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

"let mapleader = ","
"for outliner
"let maplocalleader = ","

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

" setting for vim wiki
let g:vimwiki_global_ext = 0
let g:vimwiki_ext2syntax = {'.md': 'markdown','.markdown': 'markdown','.mdown': 'markdown'}
let g:vimwiki_root = 'd:\vimwiki'
let g:vimwiki_listsyms = ' ✗.○●✓'
" let g:vimwiki_list = [
"             \{'path': 'd:\vimwiki\projects', 'syntax': 'markdown', 'ext': 'md'},
"             \{'path': 'd:\vimwiki', 'syntax': 'markdown', 'ext': '.md'}]



" vim: sw=2 sts=2 tw=0 fdm=marker
