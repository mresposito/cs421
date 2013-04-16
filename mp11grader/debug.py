import subprocess, os


print "make \n"

cmd = "ocamlc -o grader mp11.ml"
os.system( cmd )

print "run \n"

cmd = "./grader > out"
os.system( cmd )

os.system ( "cat latexHeader.tex out latexFooter.tex > compile.tex" )
os.system ( "pdflatex -interaction=nonstopmode compile.tex" )
os.system ( "open compile.pdf" )
