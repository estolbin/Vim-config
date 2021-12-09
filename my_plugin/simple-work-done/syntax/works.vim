if exists("b:current_syntax")
    finish
endif

let b:current_syntax="works"

syntax match DateWorks  '\d\{2,4\}-\d\{2\}-\d\{2\}' 
syntax match Client     '\-\s.\+$'
syntax match Todone     '\s\{2,4\}.\+$'
syntax match HLine      '\-\{80\}'

highlight default link DateWorks    PreProc
highlight default link Client       Constant
highlight default link Todone       Statement
highlight default link HLine        Comment
