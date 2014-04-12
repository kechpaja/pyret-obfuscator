pyret-obfuscator
================

A simple code obfuscator for the Pyret language. 

Does Source-to-Source transformation to replace identifiers, lift literal values to the top, insert a few nonce branches in if statements, and remove all doc strings and comments. 

At the moment, this program only handles a subset of the language, but with luck it will eventually be extended to handle the entire AST. 

You can run the program by running 

raco pyret obfs.arr <program file> <output file>
