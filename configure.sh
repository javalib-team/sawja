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
###     
### This program is free software: you can redistribute it and/or
### modify it under the terms of the GNU Lesser General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
### 
### This program is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Lesser General Public License for more details.
### 
### You should have received a copy of the GNU Lesser General Public
### License along with this program.  If not, see 
### <http://www.gnu.org/licenses/>.
### 
### This file: began on         march-24-2010,
###            last updated on  .
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
PERL=`which perl`
# The debug flag
DEBUG=yes
# The path to the Javalib libraries
JAVALIB=
# Set to yes if you have the Buddy BDD library or to no if you do not have the
# library or do not want to use it
BUDDY=no
# The ocamlopt flags (depends on DEBUG)
OPT_FLAGS=

# The camlp4o pretty-printer
PP=

# The following variables are constants
FLAGS="-g -w Ae -annot"


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
      exit 1
    elif [ $1 = "inf" ]; then
      echo "* $2." | fmt
      return 0
    fi
  elif [ $# -eq 3 ]; then
    if [ $1 = "ser" ]; then
      echo ""
      echo "! script error ($2): $3. Please file a bug." | fmt >&2
      exit 1
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
# The option parsing function. Uses getopt, a more full-featured command than
# the getopts bash built-in function.
#
tmpopt=`getopt -o h --long help,debug:,buddy:,local:: -n \`basename $0\` -- "$@"`
# Check the getopt return code
if [ $? != 0 ]; then 
   msg "err" "option parse error"
fi

eval set -- "$tmpopt"

#for arg do echo '--> '"\`$arg'" ; done
while true ; do
  case "$1" in
    --local) 
    # local has an optional argument. An empty quote is generated if this option
    # is not exercised.
      case "$2" in
        "") tmpdest="`pwd`/../javalib/lib";;
        *)  tmpdest="$2";;
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
      export OCAMLPATH=$LOCALDEST
      shift 2;;
    --buddy) 
        BUDDY="$2"
        msg "inf" "Request the Buddy BDD library? '$BUDDY'"
        shift 2;;
    --debug) 
        DEBUG="$2"
        msg "inf" "Debug flag set to '$DEBUG'"
        shift 2;;
    -h|--help) 
        echo "Usage: `basename $0` [--local[=PATH]] [--help] [--buddy=yes|no] [--debug=yes|no|prof]"
        exit 0;;
    --) shift; break;;
    *) msg "ser" "option parsing" "unrecognized argument '$1'";;
  esac
done


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
# Check Perl
#
if [ $PERL ]; then
  msg "inf" "Ocamlfind found at $PERL"
else
  msg "err" "perl not found. 

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
  msg "err" "Package javalib not found: check your installation. In particular, if you performed a non-global installation of Javalib, use the --local flag to specify its location"  
fi

if [ $BUDDY = "yes" ]; then
  location=`$FINDER query buddy 2>/dev/null`
  if [ $? = 0 ]; then
    msg "inf" "Package buddy found at $location"
    INCLUDE="-package javalib,buddy"
  else
    msg "err" "Package buddy not found"  
  fi
else
  msg "inf" "Sawja won't use the buddy BDD package"
  INCLUDE="-package javalib"
fi


#
# Check Recode
#
if [ $RECODEBIN ]; then
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
if [ -z "$cp4" ]; then
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

case $DEBUG in
  yes)  OPT_FLAGS="-g";;      
  prof) OPT_FLAGS="-g -p -noassert -ccopt -O3";;
  no)   OPT_FLAGS="-g -noassert -ccopt -O3";;
  *)    msg "err" "debug option $DEBUG is not recognized"
esac


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
# Constants
echo "" >> $makeconfig
echo "# Configuration constants" >> $makeconfig
for var in FLAGS; do
  echo "$var=${!var}" >> $makeconfig
done
echo -n "."
# Configuration variables
echo "" >> $makeconfig
echo "# Variables detected at configure-time" >> $makeconfig
for var in OPT_FLAGS LOCALDEST FINDER PERL RECODE DEBUG JAVALIB BUDDY INCLUDE PP; do
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
echo ""
echo "WHAT'S NEXT: all dependencies are satisfied. Compile and install Sawja with the following commands:" | fmt
if [ "$LOCALDEST" ]; then
  echo "    make && make install"
else
  echo "    make && sudo make install"
fi
echo ""
echo "More details can be found in the installation documentation (INSTALL or http://javalib.gforge.inria.fr/sawja-doc.html)." | fmt

exit 0
