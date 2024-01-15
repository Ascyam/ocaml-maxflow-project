# Ocaml project
As a part of our computer science course, we created in Ocaml a Ford-Fulkerson algo in order to use it in a moneysharing application. This consists of balancing expenses made within a group.

## Project Stages
 - Minimal acceptable project (doing Ford-fulkerson): finished
 - Medium project (have the expenditure balance graph): finished
 - Better project (enhance the medium project + implementing the max-flow min-cost algorithm.): not started

## How to test
1/ To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.

2/ Open a terminal on your computer and clone the repository wherever you want by using the command:
```
git clone https://github.com/Ascyam/ocaml-maxflow-project.git
```

3/ Then open a terminal in the folder and open VSCode, the following features can be used and were given by the teacher:

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save


A makefile provides some useful commands:
 - `make build` to compile. This creates an ftest.native executable
 - `make demo` to run the `ftest` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).

4/ In the project, you can see a money folder in which there is a moneysharing file with a particular syntax. The first group of lines is for the persons in the group and the next group of lines is for the expenses. You can modify it but you need to follow the syntax to avoid mistakes:

```
% This is a moneysharing.

p 1 Alan 0.0
p 2 Imad 0.0
p 3 Hazem 0.0
p 4 Mongi 0.0
p 5 Ben 0.0

d 1 44.0 
d 2 20.0 
d 5 20.0 

% Dans cet exemple, Alan Imad Hazem Mongi et Ben ont effectués 3 dépenses qui
% concernaient tout le monde et qui ont été payés par Alan, Imad et Ben.
% End of moneysharing
```
