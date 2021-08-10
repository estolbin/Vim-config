@echo off
set filename= %~n1.pdf
set inp= %1
pandoc %inp% -o %filename% --pdf-engine=xelatex --variable mainfont="Times New Roman"
