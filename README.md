# Resources related to building compilers with SML/NJ in ECE 553 at Duke

[Advice from another compiler course's instructor](http://www.cs.cornell.edu/courses/cs4120/2018sp/project/how-to-lose.html)

## Useful Links

[Course Website](https://adhilton.pratt.duke.edu/ece-553-compiler-construction)  
[Textbook Website](https://www.cs.princeton.edu/~appel/modern/ml/project.html)  

[ASCII table](http://www.bluesock.org/~willg/dev/ascii.html)

### Lexical Analysis

[ML-Lex manual](https://www.smlnj.org/doc/ML-Lex/manual.html)  
[User’s Guide to ML-Lex and ML-Yacc](http://www.cs.tufts.edu/comp/181/ug.pdf)  
[ML-Yacc](https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html)  

---

### SML

[Standard ML of New Jersey User's Guide](http://www.smlnj.org/doc/)   
[Syntax Cheat Sheet](http://rigaux.org/language-study/syntax-across-languages-per-language/SML.html)   
[Cheat Sheet 2](http://www.cs.cornell.edu/courses/cs312/2003fa/handouts/cheatsheet.pdf)  
[Errors Meanings](http://flint.cs.yale.edu/cs421/smlnj/doc/errors.html)   
[SML mode Ref](https://www.smlnj.org/doc/Emacs/sml-mode.html)  
[The SML/NJ Compilation and Library Manager](http://smlnj.org/doc/CM/new.pdf)  
[Style Guide](https://www.cs.tufts.edu/comp/105-2017f/handouts/mlstyle.pdf)  

[Data Structures and Functional Programming Using SML](http://www.cs.cornell.edu/courses/cs312/2008sp/)  
[Importing other modules](https://www.classes.cs.uchicago.edu/archive/2006/fall/15300-1/handouts/sml-load.pdf)  
[Tutorial from CMSC 336: Type Systems for Programming Languages](http://ttic.uchicago.edu/~pl/classes/CMSC336-Winter08/lectures/lec-sml.pdf)  
[Strcuture](https://www.cs.cmu.edu/~rwh/introsml/modules/sigstruct.htm)   
[Functor Examples](https://gist.github.com/tyrinwu/51d8251c35b35cf908b3254599d4e720)   
[exception](https://www.cs.cmu.edu/~rwh/introsml/core/exceptions.htm)  

### Related Courses

[Yale's Compiler course](http://flint.cs.yale.edu/cs421/)  
[15-417 HOT Compilation Spring](http://www.cs.cmu.edu/~crary/hotc/)  
[Introduction to Type Systems](https://www.cs.cmu.edu/afs/cs/academic/class/15814-f03/www/)  
[CMSC 336: Type Systems for Programming Languages](http://ttic.uchicago.edu/~pl/classes/CMSC336-Winter08/)  
[CSCE 434-500: Compiler Design](https://parasol.tamu.edu/~rwerger/Courses/434/)  
[CS 4610 - Programming Languages - Lectures](https://web.eecs.umich.edu/~weimerw/2009-4610/lectures.html)  <- focusing on motivation  
[Compiler course from Northwestern University](http://users.eecs.northwestern.edu/~jesse/course/csg262-sp08/)  
[Programming Language](https://www.cs.tufts.edu/comp/105-2017f/)  
[Concepts in Programming Languages](https://www.cl.cam.ac.uk/teaching/1011/ConceptsPL/)  
[Compiler IIT](https://www.cse.iitm.ac.in/~krishna/cs3300/)  
[Compiler NYU](https://cs.nyu.edu/courses/spring13/CSCI-GA.2130-001/)  
[Compiler Design II](http://web.cecs.pdx.edu/~apt/cs322/)  
[CMSC 22610 Implementation of Computer Languages - I](https://www.classes.cs.uchicago.edu/archive/2007/winter/22610-1/)  
[Compiler in Cornell](http://www.cs.cornell.edu/courses/cs4120/2018sp/)  
[Programming Language](https://courses.cs.washington.edu/courses/cse341/17sp/)  
[CS320:Compiling Techniques at Princeton](http://www.cs.princeton.edu/courses/archive/spr16/cos320/)

### Parsing, grammar, all that good stuff...

[Right-most derivation?](https://cs.stackexchange.com/questions/54814/different-between-left-most-and-right-most-derivation)

### Things I wished I knew

1. If a function, say A, wants to call function B, B must be defined physically above A.
2. To access parts of a tuple, use `#N`. (N > 0) e.g. #1 (3, 4) will give you 3.

### Other things...

[ML for the Working Programmer, 2nd Edition](https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html)  
[Online OCaml book: Real World OCaml](https://realworldocaml.org/v1/en/html/index.html)  
[SML exercises](http://exercism.io/languages/sml/about)
