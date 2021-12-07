" File: works.vim
" Author: Eugene Stolbin

if exists('g:works_done')
    finish
endif
let g:works_done = 1

fun! s:SetCurrentDate()
    exe "normal ggO"
    call append(line(".")-1,strftime("%Y-%m-%d"))
    startinsert
endfun


au BufRead *.works set filetype=works
au BufRead *.works set shiftwidth=2
au BufRead *.works set softtabstop=2
au BufRead *.works set tabstop=2
" auto insert data in file on open
au BufRead *.works call <SID>SetCurrentDate()


" Create a new item
nnoremap <silent> <Plug>(simple-works-done-new) O<C-R>=strftime("%Y-%m-%d")<CR>
inoremap <silent> <Plug>(simple-works-done-new) <C-R>=strftime("%Y-%m-%d")<CR>

" Add a new client
nnoremap <silent> <Plug>(simple-works-done-new-client) i-<space> 
inoremap <silent> <Plug>(simple-works-done-new-client) -<space>

if g:works_done
    nmap <F3> <Plug>(simple-works-done-new)
    imap <F3> <Plug>(simple-works-done-new)

    nmap <localleader>A <Plug>(simple-works-done-new-client)
    imap <localleader>A <Plug>(simple-works-done-new-client)
endif


