(* This file is part of SAWJA Copyright (c)2010 Laurent Hubert (CNRS)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.  *)

(** Refine the call graph of a program using the XTA class analysis.
    Beware than XTA on a large program may take some time (around a
    minute for 10,000 methods, including the run-time), specially if
    you have not compiled the library with the BDD support. For more
    information on XTA, you can read the article of Tip and Palsberg:
    {{:http://www.cs.ucla.edu/~palsberg/paper/oopsla00.pdf}Scalable
    Propagation-Based Call Graph Construction Algorithms}.  For an
    overview of class analyses in general (including RTA and XTA), you
    can check at Barbara G. Ryder's lecture on
    {{:http://people.cs.vt.edu/ryder/516/sp03/lectures/ClassAnal-4-304.pdf}Class
    Analyses} *)

open Javalib_pack

(** [get_XTA_program ~native_throwable field_analysis program
    entry_points] returns the same program where the control flow
    function has been improved with the result of an XTA analysis.
    The parameter [field_analysis] specifies which sensitivity should
    be used for abstraction of fields. [`Field] is more precise than
    [`Class] which is more precise than [`Global]. The optional
    parameter [native_throwable] is the list of subclasses of
    [java.lang.Throwable] that could be instantiated and thrown by the
    VM, if not specified it includes {!default_native_throwable},
    these classes will be considered as instantiated in methods with a
    handler when needed.*)
val get_XTA_program :
  ?native_throwable:JBasics.class_name list -> 
  [< `Field | `Class | `Global ] ->
  JCode.jcode JProgram.program ->
  JBasics.class_method_signature list -> JCode.jcode JProgram.program

(** Subclasses of classes RuntimeException and Error that could be
    instantiated and thrown by the JVM (cf. JVM Spec 1.5 §2.16.4).*)
val default_native_throwable : JBasics.class_name list
