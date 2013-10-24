Introduction
============
*HeapInspector* is a program to get a dump of a Java heap at the initialization
of the *JVM* (before the CLINIT of the class containing the main method).

This is used to know precisely the objects and fields early initialized by the
*JVM*. It is a C library which uses *JVMTI* and can be loaded in the JVM during
the execution of an analysed program. The analyzed program is important
because, it has been observed that the virtual machine initialization is
dependent on this program (even if no lines of this program have been run at the
inspection time).

Results are dumped into a text file using a precise format which can be then
used by other programs.

How to compile
==============

Set compilation environment
---------------------------
*JAVA_HOME* must be set to a jvm dir. We uses the *JVMTI* c header available in
the include subdir.

Compile
-------
~~~~~
make
~~~~~

Set installation environment
----------------------------
*LD_LIBRARY_PATH* must include the directory where libHeapInspector.so is
located in order to easily load the library in the *JVM*

Have a try
----------
Get a dump for the minimal program:
~~~~~
make run
~~~~~

Get a dump for personalized program
-----------------------------------
java -agentlib:HeapInspector="<pkg>.<MainClass>" <pkg>.<MainClass>

How to read/use the dump
========================

The first part of the dump represents classes. For each initialized static
field, we get its value. If it is an object, the value will be an index to the
object. After the static fields, we get a list of objects for a class and in
this list we can see the value of each instantiated field. The second part is
about the arrays: for each array type, it dumps for each instance, every
element of the array.

Sawja can use dumps to initialize an analysis state with information from
the dump. It includes a dump parser in sawja/src/heap_parser.

Here is the full grammar of the dump:

~~~~~
all:="~~CLASSES~~\n" class_desc* "~~ARRAYS~~\n" array_class_desc* 

//a class description has static fields and instances
class_desc := (class "{\n" field_desc* object_desc* "}\n")* 

object_desc:= object_tag "{\n" field_desc*"\n}"

class := "class" JAVA_CLASS_SIGNATURE

object_tag := INT // Object unique identifier

//static is printed for static field
field_desc := "(static)"? type_and_val

var_name := STRING

type_and_val := (class var_name "=" object_tag ";" 
       | "bool" var_name "=" INT ";" 
       | "byte" var_name "=" INT ";" 
       | "short" var_name "=" INT ";" 
       | "char" var_name "=" INT ";" 
       | "int" var_name "=" INT ";" 
       | "long" var_name "=" LONG ";" 
       | "float" var_name "=" HEX_VAL ";" 
       | "double" var_name "=" HEX_VAL ";" ) []*

array_class_desc := class "[]"* "{\n" array_desc* "}\n"

array_desc := object_tag "[" array_size "]{\n" array_el* "}\n"
//object_tag contains the tag of the object representing the array.
//array_size represente the size of the current array.

array_el := "[" array_index "]=" array_val  ";\n"

array_val := INT 
The semantic of the int depend of the kind of value of the array (for array of
objects, it is an index to the object, for primitive array, it is the primitive
value).

array_size := INT
//représente la taille d'un tableau

array_index := INT
//represent an index in the array
~~~~~

