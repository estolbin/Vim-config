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
set wrap linebreak nolist
set textwidth=120
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

" Show line number, cursor position.
set ruler
" Display incomplete commands.
set showcmd
" To insert timestamp, press F3.
nmap <F3> a<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR> 

"For c++ compile
"autocmd BufNewFile *.cpp execute "0r ~/.vim/template/".input("Template name: ").".cpp"
"compile
map <F5> :!g++ % -o %:r -static <CR> 
"compile and run
map <F9> :!g++ % -o %:r -static && %:r <CR>

set incsearch
set ignorecase
set visualbell

set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz

"добавляем плагины в vim
filetype plugin indent on
set encoding=utf-8

call plug#begin('C:\Users\stolbin.es\_vim\bundle')
Plug 'ErichDonGubler/vim-sublime-monokai'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'
"Plug 'shime/vim-livedown'
Plug 'vim-airline/vim-airline-themes'
"плагин для org-mode
"Plug 'jceb/vim-orgmode'
"Plug 'axvr/org.vim'
"Plug 'dhruvasagar/vim-dotoo'
Plug 'aserebryakov/vim-todo-lists'
Plug 'preservim/nerdtree'
call plug#end()

colorscheme sublimemonokai


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
let g:airline_theme='molokai'

noremap <leader>ld :LivedownToggle<CR>

"for todo-list
let g:VimTodoListsCustomsKeyMapper = 'VimTodoListsCustomMappings'

function! VimTodoListsCustomMappings()
    nnoremap <buffer> s :VimTodoListsToggleItem<CR>
    nnoremap <buffer> <Space> :VimTodoListsToggleItem<CR>
    noremap <buffer> <leader>e :silent call VimTodoListsSetItemMode()<CR>
endfunction

"Dates to todo list
let g:VimTodoListsDatesEnabled = 1

if has("gui_running")
    set lines=35 columns=150
else
    if exists("+lines")
        set lines=50
    endif
    if exists("+columns")
        set columns=100
    endif
endif

set guioptions-=m
set guioptions-=T

set guifont=Source\ Code\ Pro\ for\ Powerline:h10:cANSI

"abbreviation for lines
iab <expr> -- repeat('-', 80)

"редактирование .vimrc
nnoremap ,v :source $MYVIMRC<CR>
nnoremap ,e :edit $MYVIMRC<CR>

"my simple todo in markdown
inoremap ,td - [ ]

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
autocmd FileType html, htm inoremap < <><Left>

"select all
"map <C-a> gg v G
"map <C-c> "+y
"map <C-v> "+p

"NERDTree toggle
nmap <F6> :NERDTreeToggle<CR>

"для сохранения размеров окна и позиции
"set sessionoptions+=resize,winpos
"autocmd VIMEnter * :source C:/tmp/session.vim
"autocmd VIMLeave * :mksession! C:/tmp/session.vim
"Не сохраяет power line....
