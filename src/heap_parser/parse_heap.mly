/*
 * This file is part of SAWJA
 * Copyright (c)2013 Pierre Vittet (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

%{
  (*header*)
open Javalib_pack
open JBasics
open ParserType
open JType


%}

/*declaration of terminal and non terminal symbol.*/
%token <string> Ident /*ClassId */
%token <int> ArrayIdx
/*Every number is parsed as a int64 and then casted to the correct type.*/
%token <int64> Num HexNum 
%token Open Close Eq SemiCol Class Static TLong TShort TDouble TFloat TInt TBool TByte TChar
%token ArrayType ClassPart ArrayPart Eof

%start input
%type <ParserType.parsed_heap> input

%%
/*Grammar rules*/

input:
  | ClassPart classPart ArrayPart arrayPart Eof {{hp_class=$2;hp_array=$4;}}
;

/* ################ CLASSPART ################ */
classPart:      
  /*empty*/     {JBasics.ClassMap.empty}
  | classPart class_desc  {add_class $1 $2}
;

class_desc:   
  Class Ident Open static_fields instances Close  {gen_class $2 $4 $5}
;

static_fields:
  /*empty*/   {[]}
  | static_fields static_field {$2::$1}
;

static_field:
      Static field {$2} /*Ident is a var name*/
;

instances:
  /*empty*/ {[]}
  | Num Open instance_fields Close instances {(gen_instance $1 $3)::$5}
;

instance_fields:
   /*empty*/   {[]}
  | instance_fields field {$2::$1}
;
 

field:
    | TLong Ident Eq Num SemiCol{{fname=$2;fvalue= VLong($4)}}
    | TShort Ident Eq Num SemiCol{{fname=$2;fvalue= VShort(Int64.to_int($4))}}
    | TDouble Ident Eq HexNum SemiCol{{fname=$2;fvalue= VDouble(Int64.float_of_bits($4))}}
    | TFloat Ident Eq HexNum SemiCol{{fname=$2;fvalue= VFloat(Int64.float_of_bits($4))}}
    | TInt Ident Eq Num SemiCol{{fname=$2;fvalue= VInt(Int64.to_int32($4))}}
    | TBool Ident Eq Num SemiCol{{fname=$2;fvalue= VBool(Int64.to_int($4))}}
    | TByte Ident Eq Num SemiCol{{fname=$2;fvalue= VByte(Int64.to_int($4))}}
    | TChar Ident Eq Num SemiCol{{fname=$2;fvalue= VChar(Int64.to_int($4))}}
    | Ident Ident Eq Num SemiCol/*first Ident is a class type, second Ident is a var name*/
      {
      {fname=$2;fvalue= VObject((JBasics.make_cn $1), (raw_2_ref $4))}
      } 

    /*array type*/
    | TLong arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Long, raw_2_ref $5);}}
    | TShort arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Short, raw_2_ref $5);}}
    | TDouble arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Double, raw_2_ref $5);}}
    | TFloat arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Float, raw_2_ref $5);}}
    | TInt arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Int, raw_2_ref $5);}}
    | TBool arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Bool , raw_2_ref $5);}}
    | TByte arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Byte, raw_2_ref $5);}}
    | TChar arrayDec Ident Eq Num SemiCol {{fname=$3; fvalue= VArray(TBasic `Char, raw_2_ref $5);}}
    | Ident arrayDec Ident Eq Num SemiCol{
        {fname=$3; 
         fvalue= VArray(TObject (TClass(JBasics.make_cn $1)), raw_2_ref $5);}
      }

;

arrayDec:
    | ArrayType {}
    | ArrayType arrayDec {}
;

/* ################ CLASSPART ################ */
arrayPart:
  /*empty*/ {ObjectMap.empty}
  | arrayPart ar_class_desc {finalize_class_ar $2 $1}

ar_class_desc:
  |  Class TInt arrayTyp Open instances_ar Close {gen_class_ar "int" $3 $5}
  |  Class TLong arrayTyp Open instances_ar Close {gen_class_ar "long" $3 $5}
  |  Class TShort arrayTyp Open instances_ar Close {gen_class_ar "short" $3 $5}
  |  Class TDouble arrayTyp Open instances_ar Close {gen_class_ar "double" $3 $5}
  |  Class TFloat arrayTyp Open instances_ar Close {gen_class_ar "float" $3 $5}
  |  Class TBool arrayTyp Open instances_ar Close {gen_class_ar "bool" $3 $5}
  |  Class TByte arrayTyp Open instances_ar Close {gen_class_ar "byte" $3 $5}
  |  Class TChar arrayTyp Open instances_ar Close {gen_class_ar "char" $3 $5}
  |  Class Ident arrayTyp Open instances_ar Close {gen_class_ar $2 $3 $5}

/*return the depth of the array*/
arrayTyp:
  /**/ {0}
  | arrayTyp ArrayType {$1+1} 
  
instances_ar:
  | /*empty*/ {Ptmap.empty}
  | instances_ar instance_ar  {add_instance_ar $2 $1}

instance_ar:
  | Num ArrayIdx Open ar_elements Close {gen_instance_ar $1 $2 $4}

ar_elements:
  | /*empty*/ {[]}
  | ar_elements ar_element {$2::$1}

ar_element:
  | ArrayIdx Eq Num SemiCol {($1,$3)}
  | ArrayIdx Eq HexNum SemiCol {($1, $3)}


%%
    (*trailer*)
