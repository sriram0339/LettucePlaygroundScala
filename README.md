# Lettuce Playground in Scala

This project visualizes the Lettuce programming language interpreter we have
built for the course _Principles of Programming Languages_ (CS 3155, CU
Boulder). Our goal is to build a simple GUI-based Scala application that can
parse programs written in the Lettuce syntax and help the student step through
the interpretation and see what the various parts of the program evaluate to as
we step through.

![Screenshot]
(https://github.com/sriram0339/LettucePlaygroundScala/tree/master/images/screenshot-b.png)

## Run

If you have scala build tools installed, simply type:

~~~
sbt run
~~~

Currently, this is setup to run under 
  - sbt version 1.3.13
  - scala version 2.12.7
  - Java JDK 1.8 
  
**Known Issue:** This does not work under JDK 1.14.


Alternatively, you can import it into your IDE such as IntelliJ with scala plugin as a STB project and lanuch it from there.



## Note

Code is currently under development. Expect lots of errors and changes.
More documentation on the lettuce language and syntax will be added.
We will also add some documentation on the "trampolined" (CEK) interpreter
we have used to perform the step wise execution. 

## Author
  
  This code was authored by Sriram Sankaranarayanan and distributed under
  the GPLV3 license.  Questions and comments can be sent directly to Sriram.
  
  
  
  
