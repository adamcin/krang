# krang
A possibly abortive attempt at writing a C->llvm IR compiler in Rust, just for learning a bit of all of it.

## The Goal
Produce `.ll` files from `.c` and `.h` files, such that the following is subsequently possible:

```
$ llvm-as hello-world.ll -o hello-world.bc
$ llc -filetype=obj hello-world.bc -o hello-world.o
$ clang hello-world.o -o hello-world
$ ./hello-world
Hello, world!
```
