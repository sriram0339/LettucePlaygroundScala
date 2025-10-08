# Lettuce Playground in Scala

This project visualizes the Lettuce programming language interpreter we have
built for the course _Principles of Programming Languages_ (CSCI 3155, CU
Boulder). Our goal is to build a simple GUI-based Scala application that can
parse programs written in the Lettuce syntax and help the student step through
the interpretation and see what the various parts of the program evaluate to as
we step through.

![Screenshot](/images/screenshot-b.png "Screenshot of the tool")

## Run

If you have scala build tools installed, simply type:

~~~
sbt run
~~~

Currently, this is setup to run under 
  - sbt version 1.11.6
  - scala version 3.3.6


Slightly older versions of sbt and scala may also work.

Alternatively, you can import it into your IDE such as IntelliJ with scala plugin or VSCode with Metals as a SBT project and lanuch it from there.



## Note


More documentation on the lettuce language: [CSCI 3155 Notebooks](https://github.com/sriram0339/csci3155_notebooks)

We use a trampolined CEK interpreter was inspired by a summer PhD reading of this book by Felleisen, Findler and Flatt: [https://mitpress.mit.edu/9780262062756/semantics-engineering-with-plt-redex/](https://mitpress.mit.edu/9780262062756/semantics-engineering-with-plt-redex/)


## Author
  
  This code was authored by <a href="https://www.cs.colorado.edu/~srirams> Sriram Sankaranarayanan</a> and distributed under
  the GPLV3 license.  Questions and comments can be sent directly to Sriram.
  
  
  
  
