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

Moreover, *Jaws* provides a stack-less intermediate representation of
code, not far from *SSA*, called *JBir*. This representation opens the
way to many analyses which can be built upon it more naturally, better
than with the byte-code representation (e.g. *Live Variable
Analysis*). *Jaws* also provides functions to map a program using
a particular code representation to another.

===================
Global architecture
===================

In this section, we present the different modules of *Jaws* and how
they interact together. While reading the next sections, we recommend you
to have a look at *Jaws* API at the same time.

*JProgram* module
-----------------

This module defines:

  - the types representing the class hierarchy.
  - the program structure.
  - some functions to access classes, methods and fields (similar to
    *Javalib* functions).
  - some functions to browse the class hierarchy.
  - a large set of program manipulations.

Classes and interfaces are represented by **class_node** and
**interface_node** record types, respectively. These types are
parametrized by the code representation type, like in *Javalib*.
These types are private and cannot be modified by the user.
The only way to create them is to use the functions
**make_class_node** and **make_interface_node** with consistent
arguments. In practice, you will never need to build them because the
class hierarchy is automatically generated when loading a program. You
only need a read access to these record fields.

The program structure contains:

  - a map of all the classes referenced in the loaded program. These
    classes are linked together through the node structure.
  - a map of parsed methods. This map depends on the algorithm used to
    load the program (*CRA*, *RTA*, ...).
  - a static lookup method. Given the calling class name, the calling
    method signature, the invoke kind (virtual, static, ...), the
    invoked class name and method signature, it returns a set of
    potential (**class_name** * **method_signature**) that may be
    called.

*JCRA*, *JRTA* and *JRRTA* modules
----------------------------------

Each of these modules implements a function **parse_program** (the
signature varies) which returns a program parametrized by the
**Javalib.jcode** representation.

In *RTA*, the function **parse_program** takes at least, as
parameters, a class-path and a program entry point. The
**default_entrypoints** value represents the methods that are always
called by *Sun JVM* before any program is launched.

In *CRA*, the function **parse_program** takes at least, as
parameters, a class-path and a list of classes acting as entry points.
The **default_classes** value represents the classes that are always
loaded by *Sun JVM*.

*JRRTA* is a refinement of *RTA*. It first calls *RTA* and then prunes
the call graph.

*JNativeStubs* module
---------------------

This module allows to define stubs for native methods, containing
information about native method calls and native object allocations.
Stubs can be stored in files, loaded and merged. The format to
describe stubs looks like:

::

  Method{type="Native" class="Ljava/lang/String;"
         name="intern" signature="()Ljava/lang/String;"}{
    VMAlloc{
      "Ljava/lang/String;"
      "[C"
    }
  }

  Method{type="Native" class="Ljava/io/UnixFileSystem;"
         name="getLength" signature="(Ljava/io/File;)J"}{
    Invokes{
      Method{type="Java" class="Ljava/lang/String;"
             name="getBytes" signature="(Ljava/lang/String;)[B"}
    }
  }

*JRTA* admits a stub file as optional argument to handle native
methods.

*JControlFlow* module
---------------------

*JBir* module
-------------

*JPrintHtml* module
-------------------

========
Tutorial
========

