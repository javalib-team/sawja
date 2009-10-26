============
Introduction
============

*Jaws* is a library written in *OCaml*, relying on the *Javalib* to
provide a high level representation of *Java* byte-code programs.
Whereas *Javalib* is dedicated to class per class loading, *Jaws*
introduces a notion of program thanks to control flow algorithms. For
instance, a program can be loaded using various algorithms like *Class
Reachability Analysis* (a variant of *CHA* algorithm) or *Rapid Type
Analysis* (*RTA*). For now, *RTA* is the best compromise between
loading time and precision of the call graph. A version of *XTA* is
coming soon.

In *Jaws*, classes and interfaces are represented by interconnected
nodes belonging to a common hierarchy. For example, given a class
node, it's easy to access its super class, its implemented interfaces
or its children classes. The next chapters will give more information
about the nodes and program data structures.

Moreover, *Jaws* provides a stack-less intermediate representation, not
far from *SSA*, called *JBir*. This representation opens the way to
many analyses which can be built upon it more naturally, better than
with the byte-code representation (e.g. *Live Variable Analysis*).
