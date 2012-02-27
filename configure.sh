#!/bin/bash

###
### A configuration script for Sawja
###
###     Provide a "local" configuration option
###     Detect ocamlfind
###     Determine whether javalib and buddy are installed
###     Check for recode
###     Set the debug flag
###     Select the camlp4 package and camlp4o syntax 
###     Infer the destdir value from the localdest flag
###     Infer the ocamlopt flag value from the debug flag
###     Write the variables to the Makefile.config file
###     
###     
### Copyright (c)2010 Florent Kirchner
### Copyright (c)2010 Vincent Monfort
###     
### This program is free software: you can redistribute it and/or
### modify it under the terms of the GNU General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
### 
### This program is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
### 
### You should have received a copy of the GNU General Public
### License along with this program.  If not, see 
### <http://www.gnu.org/licenses/>.
### 


# The directory for local installations. Leave it empty for a global install.
LOCALDEST=
# The destdir argument to "ocamlfind install" (depends on LOCALDEST)
DESTDIR=
# The ocamlpath variable for the compiler to locate the locally-installed
# packages (depends on LOCALDEST)
OCAMLPATH=
# The path to ocamlfind
FINDER=`which ocamlfind`
# The path to recode (used to fix accents in the documentation)
RECODEBIN=`which recode`
# The perl executable
PERL=
# The debug flag
DEBUG=yes
# The shared option flag
SHARED=
# The path to the Javalib libraries
JAVALIB=
# Set to yes if you have the Buddy BDD library or to no if you do not have the
# library or do not want to use it
BUDDY=no
# The ocamlopt flags (depends on DEBUG)
OPT_FLAGS=
# The ocamlc flags (depends on DEBUG)
FLAGS="-w Aer"

# The camlp4o pretty-printer
PP=




# Differentiated error numbers make for easier bug hunting. Hopefully we won't
# have to use them.
E_MAKERERROR=83
E_SCRIPTERROR=84

#
# The msg recursive function takes care of the pretty-printing.
# It uses "fmt" to stick to 75 characters columns.
#
function msg() 
{
  if [ $# -eq 2 ]; then
    if [ $1 = "err" ]; then
      echo ""
      echo "! configure error: $2." | fmt >&2
      exit $E_MAKERERROR
    elif [ $1 = "inf" ]; then
      echo "* $2." | fmt
      return 0
    fi
  elif [ $# -eq 3 ]; then
    if [ $1 = "ser" ]; then
      echo ""
      echo "! script error ($2): $3. Please file a bug." | fmt >&2
      exit $E_SCRIPTERROR
    fi
    msg "ser" "msg" "unexpected message type"
  else
    msg "ser" "msg" "incorrect number of message arguments"
  fi
}


#
# The push function takes an atom and a variable that contains a list, and
# performs the corresponding push.
#
# For instance, if LIST=bar\ baz, then after 'push foo LIST', LIST=foo\ bar\ baz.
#
function push ()
{
  if [ $# -ne 2 ]; then
    msg "ser" "push" "incorrect number of message arguments"
  fi
  atom=$1
  list=$2
  if [ -z "${!list}" ]; then
    eval $list=$atom
  else
    eval $list="$atom\ ${!list}"
  fi
  return 0
}


#
# Macro function to print a usage message.
#
function print_usage()
{
  echo -e "
Sawja configure.sh
Usage: `basename $0` [-l [PATH|default]] [-d [yes|no|prof]] [-b [yes|no]] [-h]
Options:
  -l PATH \t Perform a local installation at PATH.
  \t\t This also needs to be the Javalib installation directory
  -d FLAG \t Use the debug flag when compiling (default: yes).
  -b FLAG \t Compile Sawja to use the Buddy BDD library (default: no).
  -h  \t\t Print this message and exit."
}
#   -s  \t\t Complile a dynamically loadable plugin (cmxs).


#
# The option parsing function. Uses getopts, a bash built-in function.
#
while getopts "d:b:l:h" opt
do
  case $opt in 
    h   ) print_usage
          exit 0;;
    b   ) BUDDY="$OPTARG";;
    d   ) DEBUG=$OPTARG;;
    l   ) case "$OPTARG" in
            default)    tmpdest="`pwd`/../javalib/lib";;
            *)          tmpdest="$OPTARG";;
          esac
          LOCALDEST=`(cd $tmpdest && pwd) 2>/dev/null`
          if [ $? != 0 ]; then
            msg "inf" "Local installation, but directory $tmpdest was not found"
            echo -n "  Creating directory... "
            mkdir -p $tmpdest/stublibs
            echo "done."
            LOCALDEST=`(cd $tmpdest && pwd)` # This one can't fail!
          fi
          msg "inf" "Local installation, at $LOCALDEST"
          # For the rest of this configure, set OCAMLPATH to $LOCALDEST
          # NB: only children of this script are in the scope of 'export'.
          export OCAMLPATH=$LOCALDEST;;
    s   ) SHARED="sawja.cmxs"
           msg "inf" "Plugin version of sawja will be generated at compilation (ocamlopt -shared)";;
    *   ) msg "err" "unrecognized option '$OPTARG'. Type '`basename $0` -h' to list available options";;
  esac
done

shift $(($OPTIND - 1))

case $DEBUG in
    yes | YES | y | Y) DEBUG=yes OPT_FLAGS="-g" FLAGS="-g $FLAGS"
	msg "inf" "Debug flag set to yes";;
    prof | PROF | p | P) DEBUG=prof OPT_FLAGS="-g -p -noassert -ccopt -O3" FLAGS="-g $FLAGS"
	msg "inf" "Debug flag set to prof";;
    no | NO | n | N) OPT_FLAGS="-g -noassert -ccopt -O3" FLAGS="-g -noassert $FLAGS"
	msg "inf" "Debug flag set to no";;
    *)    msg "err" "debug option $DEBUG is not recognized"
esac

case $BUDDY in
    yes | YES | y | Y)
	BUDDY=yes
	msg "inf" "Buddy flag set to yes"
	location=`$FINDER query buddy 2>/dev/null`
	if [ $? = 0 ]; then
	    msg "inf" "Package buddy found at $location"
	    INCLUDE="-package javalib,buddy"
	else
	    if [ -e /usr/local/include/bdd.h ] && [ -e /usr/local/lib/libbdd.a ]; then
		if [ "$LOCALDEST" ]; then
		    msg "err" "Package buddy for caml not found. 
 Please compile and install buddycaml with the following command:
    make buddycaml && make installbuddycaml
 (WARNING: if you use a Machintosh, you must have applied the patch ./buddycaml/patch on BuDDy sources before installing it)" 
		else
		    msg "err" "Package buddy for caml not found. 
 Please compile and install buddycaml with the following command:
    make buddycaml && sudo make installbuddycaml
 (WARNING: if you use a Machintosh, you must have applied the patch ./buddycaml/patch on BuDDy sources before installing it)" 
		fi
	    else
		msg "err" "In order to use buddy, you have to compile the BuDDy library (http://sourceforge.net/projects/buddy/) after applying the patch ./buddycaml/patch on source files (if you use a Macintosh) and to install it.
 If you already installed it, see README file in ./buddycaml/ to compile and install the ocaml binding for BuDDy."
	    fi
	fi;;
    no | NO | n | N)
	msg "inf" "Buddy flag set to no. Sawja won't use the buddy BDD package"
	INCLUDE="-package javalib";;
    *)  msg "err" "buddy BDD library option $BUDDY is not recognized"
esac

#
# Check Ocamlfind and print the global installation directory if relevant.
#
if [ $FINDER ]; then
  msg "inf" "Ocamlfind found at $FINDER"
else
  msg "err" "ocamlfind not found. Ocamlfind is part of the Findlib package management library, and is required to install Javalib/Sawja.

Use your system's software packaging tools to install Findlib, or download it from:
http://www.camlcity.org/archive/programming/findlib.html"
fi

if [ -z $LOCALDEST ]; then 
  msg "inf" "System-wide installation, in `$FINDER printconf destdir`" 
fi

#
# Check Ocaml version and add the correct flag in function
#
V=`$FINDER ocamlc -version`
OCAML_VERSION=${V:0:4}

if [ -z $OCAML_VERSION ] || [[ "$OCAML_VERSION" < "3.11" ]]; then
  FLAGS="$FLAGS -dtypes"
else
  FLAGS="$FLAGS -annot"
fi


#
# Check Perl
#
PERL=`which perl`
if [ $? = 0 ]; then
  msg "inf" "Perl found at $PERL"
else
  msg "err" "Perl not found.

Use your system's software packaging tools to install Perl, or download it from:
http://www.perl.org/get.html"
fi

#
# Check for Javalib and Buddy.
#
JAVALIB=`$FINDER query javalib 2>/dev/null`
if [ $? = 0 ]; then
  msg "inf" "Package javalib found at $JAVALIB"
else
  msg "err" "Package javalib not found: check your installation. In particular, if you performed a non-global installation of Javalib, use the -l flag to specify its location"  
fi

#
# Check Recode
#
RECODEBIN=`which recode`
if [ $? = 0 ]; then
  msg "inf" "Recode found at $RECODEBIN"
  RECODE="-pp \"$RECODEBIN UTF-8..Latin-1 <\""
else
  msg "inf" "Recode not found, proceeding anyway"
fi


#
# Check camlp4 / camlp4o
#
location=$FINDER query camlp4 2>/dev/null
if [ $? != 0 ]; then
  msg "inf" "Package camlp4 found at $location"
else
  msg "err" "Package camlp4 not found."  
fi

cp4=`which camlp4o`
if [ $? != 0 ]; then
  msg "err" "No camlp4o executable found"
fi
msg "inf" "Camlp4o found at $cp4"

PP="-package camlp4 -syntax `basename $cp4`"


#
# Infer the value of the DESTDIR and OPT_FLAGS variables
#
if [ -n "$LOCALDEST" ]; then
  DESTDIR="-destdir $LOCALDEST"
fi

#
# Output variables to the Makefile.config file
# TODO: move the Makefile.config.example out of the way (in src?)
#
makeconfig=`pwd`/Makefile.config
makeconfigtemplate=`pwd`/Makefile.config.example
# Copy the Makefile.config from template and add a warning header
msg "inf" "Writing $makeconfig"
echo "  Creating from $makeconfigtemplate."
# Header
echo "# WARNING: this file was automatically generated by `basename $0`." > $makeconfig
echo "# Edit at your own risk." >> $makeconfig
echo -n "  ."
# Configuration variables
echo "" >> $makeconfig
echo "# Variables detected at configure-time" >> $makeconfig
for var in FLAGS OPT_FLAGS LOCALDEST FINDER PERL RECODE DEBUG SHARED JAVALIB BUDDY INCLUDE PP; do
  echo "$var=${!var}" >> $makeconfig
done
echo -n "."
# The rest from template
echo "" >> $makeconfig
echo "# Variables from template at: " >> $makeconfig
echo "# $makeconfigtemplate" >> $makeconfig
cat $makeconfigtemplate >> $makeconfig
echo -n "."
echo " done."


#
# Tell the user what to do next: compile and install Sawja 
#
SAWJA=`$FINDER query sawja 2>/dev/null`
ALR_INST=$?
echo ""
echo "WHAT'S NEXT: all dependencies are satisfied." | fmt
if [ $ALR_INST = 0 ]; then
	echo " A version of Sawja is already installed."
	echo " Compile, remove and install Sawja with the following commands:" | fmt
    else echo " Compile and install Sawja with the following commands:" | fmt
    fi
if [ "$LOCALDEST" ]; then
    if [ $ALR_INST = 0 ]; then
	echo "    make && make remove install"
    else echo "    make && make install"
    fi   
else
    if [ $ALR_INST = 0 ]; then
	echo "    make && sudo make remove install"
    else echo "    make && sudo make install"
    fi   
fi
echo ""
echo "More details can be found in the installation documentation (INSTALL or http://javalib.gforge.inria.fr/sawja-doc.html)." | fmt

exit 0
