/*
  This file is part of SAWJA
  Copyright (c)2013 Pierre Vittet (INRIA)
 
  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of
  the License, or (at your option) any later version.
 
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
 
  You should have received a copy of the GNU General Public
  License along with this program.  If not, see
  <http://www.gnu.org/licenses/>.
*/


/*
 * This program allows to dump the heap of an empty java program after the
 * initiation phase of the standard library. It allows to get a concret
 * initial heap as a starting point for an initial abstract one.
 *
 * The dumped file is divided into 2 section:
 * A ~CLASS~ section which contains for each class every static field of the
 * class and every instance with the value of their fields. If the field is an
 * object the identifier of the reference is given. 
 *
 * An ~ARRAY~ section which dump values of every array.  
 *
 */

#include <stdlib.h>
#include <jvmti.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include <search.h>


enum type
{
  INT,
  FLOAT,
  DOUBLE,
  INT64,
  OBJECT,
  ARRAY
};

typedef union 
{
  jint int_v;
  jfloat float_v;
  jfloat double_v;
  jlong int64_v;
  jobject object_v; 
} value;

enum HeapKind
{
  REFERENCE_FIELD,
  REFERENCE_ARRAY_ELEMENT,
  REFERENCE_STATIC_FIELD,
  PRIMITIVE_ARRAY_ELEMENTS,
  REFERENCE_CLASS,
};


typedef struct 
{
  enum type typ;
  long nb_el;
  value * array;
}tmp_jvalue_ar;

typedef union _tmp_jvalue
{
  jvalue simple_val;
  tmp_jvalue_ar array_val;
} tmp_jvalue;

/* ######################## GLOBAL VARIABLE ######################## */

JavaVM* cur_vm;
jvmtiEnv * jvmti_cur_env;
JNIEnv * jni_cur_env;

/*Contain the main class of the running java program.*/
char * gbl_main_class;


/* ######################## DEBUG FUNCTIONS ######################## */

void print_jvmti_error(jvmtiError err)
{
  switch (err){
    case JVMTI_ERROR_NONE:
      printf("JVMTI_ERROR_NONE\n");
      break;
    case JVMTI_ERROR_NULL_POINTER:
      printf("JVMTI_ERROR_NULL_POINTER\n");
      break;
    case JVMTI_ERROR_OUT_OF_MEMORY:
      printf("JVMTI_ERROR_OUT_OF_MEMORY\n");
      break;
    case JVMTI_ERROR_ACCESS_DENIED:
      printf("JVMTI_ERROR_ACCESS_DENIED\n");
      break;
    case JVMTI_ERROR_UNATTACHED_THREAD:
      printf("JVMTI_ERROR_UNATTACHED_THREAD\n");
      break;
    case JVMTI_ERROR_INVALID_ENVIRONMENT:
      printf("JVMTI_ERROR_INVALID_ENVIRONMENT\n");
      break;
    case JVMTI_ERROR_WRONG_PHASE :
      printf("JVMTI_ERROR_WRONG_PHASE \n");
      break;
    case JVMTI_ERROR_INTERNAL:
      printf("JVMTI_ERROR_INTERNAL\n");
      break;
    case JVMTI_ERROR_INVALID_PRIORITY :
      printf("JVMTI_ERROR_INVALID_PRIORITY \n");
      break;
    case JVMTI_ERROR_THREAD_NOT_SUSPENDED:
      printf("JVMTI_ERROR_THREAD_NOT_SUSPENDED\n");
      break;
    case JVMTI_ERROR_THREAD_SUSPENDED :
      printf("JVMTI_ERROR_THREAD_SUSPENDED \n");
      break;
    case JVMTI_ERROR_THREAD_NOT_ALIVE:
      printf("JVMTI_ERROR_THREAD_NOT_ALIVE\n");
      break;
    case JVMTI_ERROR_CLASS_NOT_PREPARED:
      printf("JVMTI_ERROR_CLASS_NOT_PREPARED\n");
      break;
    case JVMTI_ERROR_NO_MORE_FRAMES:
      printf("JVMTI_ERROR_NO_MORE_FRAMES\n");
      break;
    case JVMTI_ERROR_OPAQUE_FRAME:
      printf("JVMTI_ERROR_OPAQUE_FRAME\n");
      break;
    case JVMTI_ERROR_DUPLICATE:
      printf("JVMTI_ERROR_DUPLICATE\n");
      break;
    case JVMTI_ERROR_NOT_FOUND:
      printf("JVMTI_ERROR_NOT_FOUND\n");
      break;
    case JVMTI_ERROR_NOT_MONITOR_OWNER:
      printf("JVMTI_ERROR_NOT_MONITOR_OWNER\n");
      break;
    case JVMTI_ERROR_INTERRUPT:
      printf("JVMTI_ERROR_INTERRUPT\n");
      break;
    case JVMTI_ERROR_UNMODIFIABLE_CLASS:
      printf("JVMTI_ERROR_UNMODIFIABLE_CLASS\n");
      break;
    case JVMTI_ERROR_NOT_AVAILABLE:
      printf("JVMTI_ERROR_NOT_AVAILABLE\n");
      break;
    case JVMTI_ERROR_ABSENT_INFORMATION :
      printf("JVMTI_ERROR_ABSENT_INFORMATION \n");
      break;
    case JVMTI_ERROR_INVALID_EVENT_TYPE:
      printf("JVMTI_ERROR_INVALID_EVENT_TYPE\n");
      break;
    case JVMTI_ERROR_NATIVE_METHOD:
      printf("JVMTI_ERROR_NATIVE_METHOD\n");
      break;
    case JVMTI_ERROR_CLASS_LOADER_UNSUPPORTED:
      printf("JVMTI_ERROR_CLASS_LOADER_UNSUPPORTED\n");
      break;
    case JVMTI_ERROR_INVALID_THREAD :
      printf("JVMTI_ERROR_INVALID_THREAD \n");
      break;
    case JVMTI_ERROR_INVALID_FIELDID:
      printf("JVMTI_ERROR_INVALID_FIELDID\n");
      break;
    case JVMTI_ERROR_INVALID_LOCATION:
      printf("JVMTI_ERROR_INVALID_LOCATION\n");
      break;
    case JVMTI_ERROR_INVALID_CLASS:
      printf("JVMTI_ERROR_INVALID_CLASS\n");
      break;
    case JVMTI_ERROR_TYPE_MISMATCH:
      printf("JVMTI_ERROR_TYPE_MISMATCH\n");
      break;
    case JVMTI_ERROR_INVALID_SLOT:
      printf("JVMTI_ERROR_INVALID_SLOT\n");
      break;
    case JVMTI_ERROR_MUST_POSSESS_CAPABILITY:
      printf("JVMTI_ERROR_MUST_POSSESS_CAPABILITY\n");
      break;
    case JVMTI_ERROR_INVALID_THREAD_GROUP :
      printf("JVMTI_ERROR_INVALID_THREAD_GROUP \n");
      break;
    case JVMTI_ERROR_INVALID_MONITOR :
      printf("JVMTI_ERROR_INVALID_MONITOR \n");
      break;
    case JVMTI_ERROR_ILLEGAL_ARGUMENT :
      printf("JVMTI_ERROR_ILLEGAL_ARGUMENT \n");
      break;
    case JVMTI_ERROR_INVALID_TYPESTATE :
      printf("JVMTI_ERROR_INVALID_TYPESTATE \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_VERSION :
      printf("JVMTI_ERROR_UNSUPPORTED_VERSION \n");
      break;
    case JVMTI_ERROR_INVALID_CLASS_FORMAT :
      printf("JVMTI_ERROR_INVALID_CLASS_FORMAT \n");
      break;
    case JVMTI_ERROR_CIRCULAR_CLASS_DEFINITION :
      printf("JVMTI_ERROR_CIRCULAR_CLASS_DEFINITION \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_ADDED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_ADDED \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_SCHEMA_CHANGED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_SCHEMA_CHANGED \n");
      break;
    case JVMTI_ERROR_FAILS_VERIFICATION :
      printf("JVMTI_ERROR_FAILS_VERIFICATION \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_HIERARCHY_CHANGED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_HIERARCHY_CHANGED \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_DELETED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_DELETED \n");
      break;
    case JVMTI_ERROR_NAMES_DONT_MATCH :
      printf("JVMTI_ERROR_NAMES_DONT_MATCH \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_CLASS_MODIFIERS_CHANGED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_CLASS_MODIFIERS_CHANGED \n");
      break;
    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_MODIFIERS_CHANGED :
      printf("JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_MODIFIERS_CHANGED \n");
      break;
    case JVMTI_ERROR_INVALID_OBJECT:
      printf("JVMTI_ERROR_INVALID_OBJECT\n");
      break;

    case JVMTI_ERROR_INVALID_METHODID:
      printf("JVMTI_ERROR_INVALID_METHODID\n");
      break;
  }
  return;

}


typedef enum err
{
  NO_ERROR,
  UNTAGGED_OBJ,
  JVMTI_ERR, //TODO: find how integrate JVMTI errors
  JVMTI_ERR_ON_IDX,
  JVMTI_ERR_ON_TAG,
  JVMTI_ERR_ON_SIG,
  JVMTI_ERR_ON_FIELDID,
  JVMTI_ERR_ON_FIELD,
  JVMTI_ERR_ON_INSTANCE,
  JVMTI_ERR_ON_CLASS,
  JVMTI_ERR_ON_ALLOC,
  JVMTI_ERR_ON_DEALLOC,
  JVMTI_ERR_ON_ARRAY,
  INVALID_NULL,
  BAD_CLASS,
  NOT_AN_ARRAY,
} hi_error;

/* ######################## UTILITIES FUNCTION ################### */

jvmtiError alloc(size_t size, void * ptr)
{
  return (*jvmti_cur_env)->Allocate(jvmti_cur_env, size, (unsigned char **)ptr);
}

jvmtiError dealloc(void * ptr)
{
  return (*jvmti_cur_env)->Deallocate(jvmti_cur_env, (unsigned char *)ptr);
}

/*
 * object_from_tag, put the object represented by the tag in obj_res_ptr.
*/
hi_error object_from_tag(jlong tag, jobject ** obj_res_ptr)
{
  jvmtiError err;
  jint * count_ptr;
  jobject * result_ptr;
  err= alloc(sizeof(jint), &count_ptr);
  if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  err = (*jvmti_cur_env)->GetObjectsWithTags(jvmti_cur_env, 1, &tag, 
                                             count_ptr, &result_ptr,
                                             NULL);
  if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}

  if(result_ptr==NULL || count_ptr == 0){
      return JVMTI_ERR_ON_TAG;
  }
  err= dealloc(count_ptr);
  if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  (*obj_res_ptr) = result_ptr;
  return NO_ERROR;
}


/*
 * Put the class of the object pointed by obj_ptr in class_ptr.
*/
hi_error class_from_object(jobject * obj_ptr, jclass ** class_ptr)
{
  jclass c_res;
  if(obj_ptr != NULL){
      c_res = (*jni_cur_env)->GetObjectClass(jni_cur_env, (*obj_ptr));
  }
  else
    {
      printf("invalid class from object, trying on a null pointer.");
      return INVALID_NULL;
    }
  memcpy((*class_ptr),&c_res, sizeof(jclass));
  return NO_ERROR;
}

/*
 * class_obj represent an object of class java.lang.Class. Especially this
 * object handle the static field of the class it represents. This method use
 * introspection to put the class represented by the instance in the class
 * argument.
 *
 * Some string 'hacking' are needed in order to transform name returned by the
 * getCanonicalName method (from java.lang.Class) which has the form
 * 'package1.package2.ExtClass.IntClass'. It needs to be transformed to
 * something like 'package1/package2/ExtClass$IntClass' with ExtClass.IntClass
 * an internal class.
 */
hi_error class_from_class_object(jobject * class_obj, jclass * class)
{
  // 1: Get the name of the class object using java.lang.Class.getCanonicalName
  jclass jlclass =  (*jni_cur_env)->FindClass(jni_cur_env, "java/lang/Class");
  jclass jlpackage=  (*jni_cur_env)->FindClass(jni_cur_env, "java/lang/Package");
  jmethodID idGetName= 
    (*jni_cur_env)->GetMethodID(jni_cur_env, jlclass, "getCanonicalName",
                                "()Ljava/lang/String;");
  // We also need to get the package name to handle internal classes.
  jmethodID idGetPkg=
    (*jni_cur_env)->GetMethodID(jni_cur_env, jlclass,
                                "getPackage","()Ljava/lang/Package;");
  jmethodID idGetPkgName=(*jni_cur_env)->GetMethodID(jni_cur_env, jlpackage, "getName","()Ljava/lang/String;");
  jstring strclassName = (jstring)
    (*jni_cur_env)->CallObjectMethod(jni_cur_env, *class_obj, idGetName);
  if(strclassName== NULL){
      //means local or anonymous class
      return BAD_CLASS;
  }
  jobject pkg= (*jni_cur_env)->CallObjectMethod(jni_cur_env, *class_obj,
                                                idGetPkg);
  if(pkg== NULL || idGetPkgName == NULL){
      //means local or anonymous class
      return BAD_CLASS;
  }
  jstring strPkg= (jstring)
    (*jni_cur_env)->CallObjectMethod(jni_cur_env, pkg, idGetPkgName);
  const char* cstrClass= 
    (*jni_cur_env)->GetStringUTFChars(jni_cur_env, strclassName, 0);
  const char* cstrPkg=
    (*jni_cur_env)->GetStringUTFChars(jni_cur_env, strPkg, 0);
  size_t pkg_nb_char= strlen(cstrPkg);
  //cstrPkg is in the form 'package1.package2' and must be transformed to fully
  //qualified name ('package2/package2').
  char fq_cstr[256];
  strcpy(fq_cstr, cstrClass);
  char * tmp_fq_cstr = fq_cstr;
  int nb_char=0;
  while(*tmp_fq_cstr!='\0'){
      if(*tmp_fq_cstr=='.'){
          if(nb_char<=pkg_nb_char){
              *tmp_fq_cstr='/';
          }
          else{//this is for internal class.
              *tmp_fq_cstr='$';
          }
      }
      nb_char++;
      tmp_fq_cstr=fq_cstr+nb_char;
  }
  // 2: get the class from the class name
  (*class) =  (*jni_cur_env)->FindClass(jni_cur_env, fq_cstr);
  // 3: free and release 
  (*jni_cur_env)->ReleaseStringUTFChars(jni_cur_env, strclassName, cstrClass);
  (*jni_cur_env)->ReleaseStringUTFChars(jni_cur_env, strPkg, cstrPkg);
  return NO_ERROR;
}



hi_error internalSig_2_userSig_rec(char* internal_sig, char ** user_sig, 
                                   int array_depth)
{
  char * tmp;
  int i = 0;
  switch (internal_sig[0]){
    case('['):
      return internalSig_2_userSig_rec(internal_sig+1, user_sig, array_depth+1);
    case('Z'):
      strcpy(*user_sig,"bool");
      break;
    case('B'):
      strcpy(*user_sig,"byte");
      break;
    case('C'):
      strcpy(*user_sig,"char");
      break;
    case('S'):
      strcpy(*user_sig,"short");
      break;
    case('I'):
      strcpy(*user_sig,"int");
      break;
    case('J'):
      strcpy(*user_sig,"long");
      break;
    case('F'):
      strcpy(*user_sig,"float");
      break;
    case('D'):
      strcpy(*user_sig,"double");
      break;
    case('L'):
      tmp = internal_sig+1;
      while(tmp[i]!='\0'){
          if(tmp[i]=='/'){
              (*user_sig)[i]='.';
          }
          else{
              (*user_sig)[i]=tmp[i];
          }
          i++;
      }
      (*user_sig)[i-1]='\0';
      break;
    default:
      return JVMTI_ERR_ON_SIG;
  }
  //add array symbol '[]'
  tmp=*user_sig;
  while(*tmp!= '\0'){
      tmp++;
  }
  while(array_depth!=0){
      tmp[0]='[';
      tmp[1]=']';
      tmp=tmp+2;
      array_depth--;
  }
  *tmp='\0';
  return NO_ERROR;
}

hi_error internalSig_2_userSig(char* internal_sig, char ** user_sig)
{
  return internalSig_2_userSig_rec(internal_sig, user_sig,0);
}

hi_error userSig_2_internalSig(char * user_sig, char ** internal_sig)
{
  char * tmp;
  int i = 0;

  if(strcmp(user_sig, "bool")==0){
      (*internal_sig)[0]='Z';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "byte")==0){
      (*internal_sig)[0]='B';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "char")==0){
      (*internal_sig)[0]='C';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "short")==0){
      (*internal_sig)[0]='S';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "int")==0){
      (*internal_sig)[0]='I';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "long")==0){
      (*internal_sig)[0]='J';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "float")==0){
      (*internal_sig)[0]='F';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  if(strcmp(user_sig, "double")==0){
      (*internal_sig)[0]='D';
      (*internal_sig)[1]='\0';
      return NO_ERROR;
  }
  else{
      tmp = user_sig;
      (*internal_sig)[i]='L';
      i++;
      while((*tmp) !='\0'){
          if((*tmp)=='.'){
              (*internal_sig)[i]='/';
          }
          else{
              (*internal_sig)[i]=*tmp;
          }
          tmp++;
          i++;
      }
      (*internal_sig)[i]=';';
      (*internal_sig)[i+1]='\0';
  }
  return NO_ERROR;
}

//*name_ptr allocated in the function, should be freed by the user.
hi_error signature_from_class(jclass* cl, char** name_ptr)
{
  jvmtiError jvmti_err;
  hi_error err;
  char * tmp_name_ptr=NULL;
  jvmti_err=(*jvmti_cur_env)->GetClassSignature(jvmti_cur_env, (*cl),&tmp_name_ptr,NULL);
  jvmti_err = alloc(sizeof(char)*512, name_ptr);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  if((tmp_name_ptr)==NULL || *tmp_name_ptr=='\n'){
      return JVMTI_ERR_ON_SIG;
  }
  if(jvmti_err!=JVMTI_ERROR_NONE){
      print_jvmti_error(jvmti_err);
      return JVMTI_ERR_ON_SIG;
  }
  err=internalSig_2_userSig(tmp_name_ptr, name_ptr);
  if(err!=NO_ERROR){return err;}
  jvmti_err=dealloc(tmp_name_ptr);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  return NO_ERROR;
}

jlong generate_tag()
{
  static jlong tag=0;
  tag = tag+1;
  return tag;
}

//TODO: handle error cases.
jint get_array_size(jobject * array_obj)
{
  jclass jarray = (*jni_cur_env)->FindClass(jni_cur_env,
                                            "java/lang/reflect/Array");
  jmethodID idGetLen= 
    (*jni_cur_env)->GetStaticMethodID(jni_cur_env, jarray, "getLength", "(Ljava/lang/Object;)I");
  jint len = (*jni_cur_env)->CallStaticIntMethod(jni_cur_env, jarray, idGetLen,
                                                 *array_obj);
  return len;
}

enum HeapKind jvmtiRefKind2RefKind(jvmtiHeapReferenceKind jkind)
{
  switch(jkind){
    case(JVMTI_HEAP_REFERENCE_FIELD):
      return REFERENCE_FIELD;
    case(JVMTI_HEAP_REFERENCE_ARRAY_ELEMENT):
      return REFERENCE_ARRAY_ELEMENT;
    case(JVMTI_HEAP_REFERENCE_STATIC_FIELD):
      return REFERENCE_STATIC_FIELD;
    case(JVMTI_HEAP_REFERENCE_CLASS):
      return REFERENCE_CLASS;
    default:
      return -1;
  }
}

enum type jvmtiTyp2Typ(jvmtiPrimitiveType jtype)
{
  switch (jtype){
    case(JVMTI_PRIMITIVE_TYPE_BOOLEAN):
    case(JVMTI_PRIMITIVE_TYPE_BYTE):
    case(JVMTI_PRIMITIVE_TYPE_CHAR):
    case(JVMTI_PRIMITIVE_TYPE_SHORT):
    case(JVMTI_PRIMITIVE_TYPE_INT):
      return INT;
    case(JVMTI_PRIMITIVE_TYPE_LONG):
      return INT64;
    case(JVMTI_PRIMITIVE_TYPE_FLOAT):
      return FLOAT;
    case(JVMTI_PRIMITIVE_TYPE_DOUBLE):
      return DOUBLE;
    default:
      fprintf(stderr,"Unvalid type detected.");//error stream
      exit(1);
  }
}

  //*sig allocated in the fonction, should be freed by the user.
  //that is to match the signature_from_class method behaviour.
hi_error signature_from_primitive(jvmtiPrimitiveType jtype, char ** sig)
{
  jvmtiError jvmti_err;
  hi_error err;
  char * tmp_sig;
  jvmti_err = alloc(sizeof(char)*512, sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  jvmti_err=alloc(sizeof(char)*16, &tmp_sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  switch(jtype){
    case(JVMTI_PRIMITIVE_TYPE_BOOLEAN):
      (tmp_sig)[0]='Z';
      break;
    case(JVMTI_PRIMITIVE_TYPE_BYTE):
      (tmp_sig)[0]='B';
      break;
    case(JVMTI_PRIMITIVE_TYPE_CHAR):
      (tmp_sig)[0]='C';
      break;
    case(JVMTI_PRIMITIVE_TYPE_SHORT):
      (tmp_sig)[0]='S';
      break;
    case(JVMTI_PRIMITIVE_TYPE_INT):
      (tmp_sig)[0]='I';
      break;
    case(JVMTI_PRIMITIVE_TYPE_LONG):
      (tmp_sig)[0]='J';
      break;
    case(JVMTI_PRIMITIVE_TYPE_FLOAT):
      (tmp_sig)[0]='F';
      break;
    case(JVMTI_PRIMITIVE_TYPE_DOUBLE):
      (tmp_sig)[0]='D';
      break;
  }
  (tmp_sig)[1]='\0';
  err=internalSig_2_userSig(tmp_sig, sig);
  if(err!=NO_ERROR){return err;}
  jvmti_err=dealloc(tmp_sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}


  return NO_ERROR;
}

//*sig allocated in the fonction, should be freed by the user.
//that is to match the signature_from_class method behaviour.
hi_error array_sig_from_primitive(jvmtiPrimitiveType jtype, char ** sig)
{
  jvmtiError jvmti_err;
  hi_error err;
  char * tmp_sig;
  jvmti_err = alloc(sizeof(char)*512, sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  jvmti_err=alloc(sizeof(char)*16, &tmp_sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  switch(jtype){
    case(JVMTI_PRIMITIVE_TYPE_BOOLEAN):
      (tmp_sig)[1]='Z';
      break;
    case(JVMTI_PRIMITIVE_TYPE_BYTE):
      (tmp_sig)[1]='B';
      break;
    case(JVMTI_PRIMITIVE_TYPE_CHAR):
      (tmp_sig)[1]='C';
      break;
    case(JVMTI_PRIMITIVE_TYPE_SHORT):
      (tmp_sig)[1]='S';
      break;
    case(JVMTI_PRIMITIVE_TYPE_INT):
      (tmp_sig)[1]='I';
      break;
    case(JVMTI_PRIMITIVE_TYPE_LONG):
      (tmp_sig)[1]='J';
      break;
    case(JVMTI_PRIMITIVE_TYPE_FLOAT):
      (tmp_sig)[1]='F';
      break;
    case(JVMTI_PRIMITIVE_TYPE_DOUBLE):
      (tmp_sig)[1]='D';
      break;
  }
  (tmp_sig)[0]='[';
  (tmp_sig)[2]='\0';
  err=internalSig_2_userSig(tmp_sig, sig);
  if(err!=NO_ERROR){return err;}
  jvmti_err=dealloc(tmp_sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}

  return NO_ERROR;

}
value jvmtiValue2Value(jvmtiPrimitiveType jtype, tmp_jvalue jval)
{
  value val;
  switch (jtype){
    case(JVMTI_PRIMITIVE_TYPE_BOOLEAN):
      val.int_v=jval.simple_val.z;
      break;
    case(JVMTI_PRIMITIVE_TYPE_BYTE):
      val.int_v=jval.simple_val.b;
      break;
    case(JVMTI_PRIMITIVE_TYPE_CHAR):
      val.int_v=jval.simple_val.c;
      break;
    case(JVMTI_PRIMITIVE_TYPE_SHORT):
      val.int_v=jval.simple_val.s;
      break;
    case(JVMTI_PRIMITIVE_TYPE_INT):
      val.int_v=jval.simple_val.i;
      break;
    case(JVMTI_PRIMITIVE_TYPE_LONG):
      val.int64_v=jval.simple_val.j;
      break;
    case(JVMTI_PRIMITIVE_TYPE_FLOAT):
      val.float_v=jval.simple_val.f;
      break;
    case(JVMTI_PRIMITIVE_TYPE_DOUBLE):
      val.double_v=jval.simple_val.d;
      break;
    default:
      fprintf(stderr,"Unvalid type detected.");//error stream
      exit(1);
  }   
  return val;
}

tmp_jvalue_ar jvmtiArrayEl2arrayEl(jvmtiPrimitiveType jtype, jlong nb_el, 
                                   const void * jvmtiArrayEl)
{
  tmp_jvalue_ar new_array;
  new_array.typ=jvmtiTyp2Typ(jtype);
  new_array.nb_el=(long)nb_el;
  new_array.array=(value *) malloc(sizeof(value)*nb_el);
  jboolean* ar_z=(jboolean*) jvmtiArrayEl;
  jbyte* ar_b=(jbyte*) jvmtiArrayEl;
  jshort* ar_s=(jshort*) jvmtiArrayEl;
  jint* ar_i=(jint*) jvmtiArrayEl;
  jchar* ar_c=(jchar*) jvmtiArrayEl;
  jlong* ar_l=(jlong*) jvmtiArrayEl;
  jfloat* ar_f=(jfloat*) jvmtiArrayEl;
  jdouble* ar_d=(jdouble*) jvmtiArrayEl;
  long i=0;
  while(i< nb_el){
      switch (jtype){
        case(JVMTI_PRIMITIVE_TYPE_BOOLEAN):
          new_array.array[i].int_v=ar_z[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_BYTE):
          new_array.array[i].int_v=ar_b[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_CHAR):
          new_array.array[i].int_v=ar_c[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_SHORT):
          new_array.array[i].int_v=ar_s[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_INT): 
          new_array.array[i].int_v=ar_i[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_LONG): 
          new_array.array[i].int64_v=ar_l[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_FLOAT):
          new_array.array[i].float_v=ar_f[i];
          break;
        case(JVMTI_PRIMITIVE_TYPE_DOUBLE): 
          new_array.array[i].double_v=ar_d[i];
          break;
      }
      i++;

  }
  return new_array;


}

void print_sig_from_class(jclass * cl)
{
  char * sig;
  signature_from_class(cl,&sig);
  printf ("sig: %s\n", sig);
}


/* ######################## FIELD INTERFACE ######################## */
typedef struct
{
  jboolean is_static; //boolean
  jint id;
  jfieldID* fid;
  char * fname;
  jboolean is_array;
  enum type typ;
  char * signature;
  value val;
} field_info;

typedef struct
{
  //jint key; // == field_info.id
  field_info* field;

} fieldMap;

hi_error get_field_name(jclass cl, jfieldID fid, char ** name)
{
  jvmtiError err;
  err = (*jvmti_cur_env)->GetFieldName(jvmti_cur_env,cl,fid,name, NULL,
                                       NULL);
  if(err!=JVMTI_ERROR_NONE){
      printf("fail\n");
      print_jvmti_error(err);
      return JVMTI_ERR_ON_FIELDID;
  }
  return NO_ERROR;
}

//return static signature of a field.
hi_error get_field_signature(jclass cl, jfieldID fid, char ** sig)
{
  jvmtiError jvmti_err;
  hi_error err;
  char * tmp;
  jvmti_err = (*jvmti_cur_env)->GetFieldName(jvmti_cur_env,cl,fid,NULL, &tmp,
                                       NULL);
  if(jvmti_err!=JVMTI_ERROR_NONE){
      printf("fail\n");
      print_jvmti_error(jvmti_err);
      return JVMTI_ERR_ON_FIELDID;
  }
  jvmti_err= alloc(sizeof(char)*(strlen(tmp)), sig);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_FIELDID;}
  err=internalSig_2_userSig(tmp, sig);
  if(err!=NO_ERROR){return err;}

  return NO_ERROR;
}


/*type used to construct a set of interface directly or indirectly implemented
 * by a class.*/
typedef struct
{
  char * name;
  jclass cl;

} interface_info;

hi_error get_interface_set(jclass cur_cl, interface_info ** array, int * nb_el)
{
  hi_error err;
  jclass * inter_ptr;
  jint nb_inter;
  interface_info * i_info;
  char * sig;
  jvmtiError jvmti_err;
  if(cur_cl == NULL){
      return NO_ERROR;
  }
  (*jvmti_cur_env)->GetImplementedInterfaces(jvmti_cur_env,
                                             cur_cl,
                                             &nb_inter,
                                             &inter_ptr);
  int i =0;
  int j=0;
  bool already_present=false;
  while(i< nb_inter){
      err =signature_from_class(&(inter_ptr[i]), &sig);
      if(err!=NO_ERROR){return err;}
      jvmti_err=alloc(sizeof(interface_info), &i_info);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
      i_info->name=sig;
      i_info->cl=inter_ptr[i];
      //we add only if it is not already present in the array
      already_present=false;
      for (j=0; j<*nb_el; j++){
          if(strcmp (array[j]->name, sig) == 0){
              already_present = true;
              break;
          }
      }
      if(!already_present)
        {
          array[*nb_el]=i_info;
          *nb_el=*nb_el+1;
        }
      i++;
  }
  //do same for superinterface
  i = 0;
  while (i < nb_inter){
      err=get_interface_set(inter_ptr[i],array,nb_el);
      if(err!=NO_ERROR){return err;}
      i++;
  }
  return NO_ERROR;
}

/*Return the number of implemented interface fields for an object of class
 * cur_cl.*/
hi_error interface_fields_count(jclass cur_cl, jint * count)
{
  jvmtiError err;
  jint nb_inter;
  jclass * inter_ptr;
  jint tmp_field_count;
  jint field_count=0;
  jfieldID * field_ar;/*needed but not really used, we are only interested in
                        the count.*/

  (*jvmti_cur_env)->GetImplementedInterfaces(jvmti_cur_env,
                                             cur_cl,
                                             &nb_inter,
                                             &inter_ptr);
  while(nb_inter!=0){
      err=(*jvmti_cur_env)->GetClassFields(jvmti_cur_env,
                                           inter_ptr[nb_inter-1],
                                           &tmp_field_count, &field_ar);
      if(err!=JVMTI_ERROR_NONE){
          return JVMTI_ERR_ON_IDX;
      }
      field_count=field_count+tmp_field_count;
      //we to call the function recursively for extended interface
      interface_fields_count(inter_ptr[nb_inter-1], &tmp_field_count);
      field_count=field_count+tmp_field_count;
      nb_inter--;
      err=dealloc(field_ar);
      if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  }
  *count=field_count;
  dealloc(inter_ptr);
  return NO_ERROR;
}


/*
 * The idx taken by fieldId_from_idx is an idx of a field considering that the
 * fields of herited classes have previous value. 
 * For more see:
 * http://docs.oracle.com/javase/6/docs/platform/jvmti/jvmti.html#jvmtiHeapReferenceInfoField
 *
 * This function returns a field index in the current class only (compatible
 * with the GetClassFields function).
 *
 */ 
hi_error get_fieldId(jint glob_idx, jclass cur_cl, jfieldID* res_id)
{
  jfieldID * field_ar;
  jint nb_fields;
  jvmtiError jvmti_err;
  hi_error err;
  jclass superclasses[100];
  interface_info* interface_set[100]; 
  int nb_inter=0;
  jclass tmp_cl=cur_cl;



  //first step: get every super class of the class
  jint sc_count=0;
  if(glob_idx<0){
      printf("neg before inter\n");
  }
  //create interface set 
  while(tmp_cl != NULL){
      //generate superclass list
      superclasses[sc_count]=tmp_cl;
      //about interface
      err=get_interface_set(tmp_cl, interface_set, &nb_inter);
      if(err!=NO_ERROR){return err;}
      tmp_cl = (*jni_cur_env)->GetSuperclass(jni_cur_env, tmp_cl);
      sc_count++;
  }
  //take in account fields from the interface set
  int i;
  for (i=0; i< nb_inter; i++){
      jvmti_err=(*jvmti_cur_env)->GetClassFields(jvmti_cur_env,
                                                 interface_set[i]->cl,
                                                 &nb_fields, &field_ar);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_FIELD;}
      glob_idx=glob_idx-nb_fields;
      dealloc(interface_set[i]->name);
      dealloc(interface_set[i]);
  }

  if(glob_idx<0){
      printf("Error: negative index \n");
      return JVMTI_ERR_ON_FIELD;

  }
  //second step: starting from the higer class, get the fieldId corresponding
  //to given index.
  while(sc_count>0){
      jvmti_err=(*jvmti_cur_env)->GetClassFields(jvmti_cur_env, 
                                                 superclasses[sc_count-1],
                                                 &nb_fields,
                                                 &field_ar);
      if(jvmti_err!=JVMTI_ERROR_NONE){
          return JVMTI_ERR_ON_IDX;
      }
      if (glob_idx< nb_fields ){
          memcpy(res_id,field_ar+glob_idx,sizeof(jfieldID));
          jvmti_err=dealloc(field_ar);
          if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
          return NO_ERROR;
      }
      else{
          glob_idx=glob_idx-nb_fields;
          sc_count--;
      }
      jvmti_err=dealloc(field_ar);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  }
  res_id=NULL;
  print_sig_from_class(&cur_cl);
  return JVMTI_ERR_ON_FIELD;
}

/*
 * Return the jFieldId correspond to the given classe and field index.
 * res_id must be Deallocated by the caller.
 */
hi_error fieldId_from_idx(jclass * cl, jint idx, jfieldID ** res_id)
{
  jfieldID * field_ar;
  jint * nb_field_ar;
  jvmtiError jerr;
  hi_error err;
  jboolean is_interface;

  jerr=alloc(sizeof(jint), &nb_field_ar);
  jerr=(*jvmti_cur_env)->GetClassFields(jvmti_cur_env, (*cl), nb_field_ar, &field_ar);
  if(jerr!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_FIELDID;
  }
  if(jerr!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_FIELDID;
  }
  jerr= (*jvmti_cur_env)->IsInterface(jvmti_cur_env, *cl,&is_interface);
  if(jerr!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_FIELDID;}
  err=get_fieldId(idx, *cl, *res_id);
  if(err!=NO_ERROR){return err;}
  dealloc(nb_field_ar);
  //maybe Deallocate individually each element of the array.
  dealloc(field_ar);
  return NO_ERROR;
}

int compare_field_key(const void *l, const void *r)
{
  const field_info*lm = l;
  const field_info*lr = r;
  return lm->id - lr->id;
}

//return:
//  true if the field has been added
//  false if the field was already present
bool add_field( fieldMap** map, field_info* fi)
{
  fieldMap*r = tsearch(fi, (void **)(map), &compare_field_key);
  if(r->field!=fi)
    {
      //element was already in the map, no need to add
      return false;
    }
  return true;
}

/*Function called by free_fields and to be run on each node.*/
void free_fields_action(const void* curNode, const VISIT v, const int depth)
{
  fieldMap* f;
  jvmtiError err;
  if(v == endorder){
      f=(fieldMap*) curNode;
      err=dealloc(f->field->fname);
      if(err!=JVMTI_ERROR_NONE)
        {printf("warning: error while deallocating fields.");}
      err=dealloc(f->field->fid);
      if(err!=JVMTI_ERROR_NONE)
        {printf("warning: error while deallocating fields.");}
      err=dealloc((f->field));
      if(err!=JVMTI_ERROR_NONE){
          printf("warning: error while deallocating fields.");
      }
  }
}

hi_error free_fields(fieldMap* map)
{
  twalk(map, &free_fields_action);
  return NO_ERROR;
}

/* ######################## INSTANCE INTERFACE ######################## */

typedef struct
{
  jlong tag;// The tag is a long value typically used to store a unique
            // identifier or pointer to object information.
  fieldMap* fields;
} instance_info;

typedef struct
{
  //jlong key;// == instance_info.tag
  instance_info * info;
} instanceMap;


int compare_instance_key(const void *l, const void *r)
{
    const instance_info *lm = l;
    const instance_info *lr = r;
    return lm->tag - lr->tag;
}

/*Return a pointer to the corresponding instance of the map [map] or NULL if
   not found.*/
hi_error get_or_add_instance(jlong tag, instanceMap** map, instanceMap ** retMap)
{
  instance_info* find_i;
  jvmtiError err;
  err=alloc(sizeof(instance_info), &find_i);
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_INSTANCE;
  }
  find_i->tag=tag;
  find_i->fields=NULL;
  instanceMap*r = tsearch(find_i, (void **)(map), &compare_instance_key); 
  if (r->info!=find_i){
      dealloc(find_i);
      if(err!=JVMTI_ERROR_NONE){
          return JVMTI_ERR_ON_INSTANCE;
      }
  }
  if(retMap!=NULL){
      (*retMap)=r;
  }
  return NO_ERROR;
}


/*Function called by free_fields and to be run on each node.*/
void free_instances_action(const void* curNode, const VISIT v, const int depth)
{
  jvmtiError err;
  instanceMap* i;
  if(v == endorder){
      i= (instanceMap* ) curNode;
      if(i->info->fields!=NULL){
          free_fields(i->info->fields);
      }
      err=dealloc(i->info);

      if(err!=JVMTI_ERROR_NONE){
          printf("warning: error while deallocating instances.");
      }
  }
}

hi_error free_instances(instanceMap* map)
{
  twalk(map, free_instances_action);
  return NO_ERROR;
}


/* ######################## CLASS INTERFACE ######################## */
typedef struct
{
  char * name;
  instanceMap* instanceMap;
  fieldMap* staticFieldMap;
} class_info;

typedef struct
{
  //char * key;
  class_info * info;
} classMap;

//global classMap
void *gbl_classMap=NULL;

//

int compare_class_key(const void *l, const void *r)
{
  const class_info *lm = l;
  const class_info *lr = r;
  return strcmp(lm->name, lr->name);
}

/*Return a pointer to the corresponding class_info or NULL if not found. 
 * Caution: in the case the class is already registered, we free name!!!
 */
hi_error get_or_add_class(char* name, class_info** retMap)
{
  class_info* find_class;
  jvmtiError err;
  err=alloc(sizeof(class_info), &find_class);
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_ALLOC;
  }
  //find_class->key = name;
  find_class->name=name;
  find_class->instanceMap=NULL;
  find_class->staticFieldMap=NULL;
  classMap*r = tsearch(find_class, &gbl_classMap, &compare_class_key); 
  if(r->info!=find_class){
      err=dealloc(find_class);
      if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  }
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_CLASS;
  }
  (*retMap)=r->info;
  return NO_ERROR;
}


/*Function called by free_fields and to be run on each node.*/
void free_classes_action(const void * curNode, const VISIT v, const int depth){
    classMap * c;
    jvmtiError err;
    if(v == endorder){
        c=(classMap *) curNode;
        err= dealloc(c->info->name);
        if(err!=JVMTI_ERROR_NONE)
          {printf("warning: error while unallocating class map.");}
        free_instances(c->info->instanceMap);
        if(err!=JVMTI_ERROR_NONE)
          {printf("warning: error while unallocating class map.");}
        free_fields(c->info->staticFieldMap);
        if(dealloc(c->info)!=JVMTI_ERROR_NONE){
            printf("warning: error while unallocating class map.");
        }
    }
}

hi_error free_classes(classMap* map){
    twalk(map, &free_classes_action);
    return NO_ERROR;
}


/* ######################## ARRAY ELEMENT ######################## */

typedef struct
{
  jlong tag;
  jobject * referer_obj;
  jint index;
  enum type typ;
  value val;
} array_el_info;

typedef struct _array_el_list
{
  array_el_info * value;
  struct _array_el_list * next;
} array_el_list;

typedef struct
{
  jlong tag;
  jint size;
  array_el_list * lst;
}array_info;

typedef struct
{
  //char * key;
  array_info* info;
} arrayMap;

typedef struct 
{
  char * sig;
  arrayMap* map;
} array_class_info ;

typedef struct
{
  array_class_info* info;
} arrayClassMap;

int compare_ar_cl_key(const void *l, const void *r)
{
  const array_class_info *lm = l;
  const array_class_info *lr = r;
  return strcmp(lm->sig, lr->sig);
}


int compare_ar_key(const void *l, const void *r)
{
  const array_info *lm = l;
  const array_info *lr = r;
  return lm->tag - lr->tag;
}


int compare_ar_el_key(const void *l, const void *r)
{
  const array_el_info *lm = l;
  const array_el_info *lr = r;
  return lm->tag - lr->tag;
}


//global arrayMap (pointer over an arrayClassMap)
void *gbl_arrayMap=NULL;


//*retMap is set to a pointer to the searched array object or to NULL if it is
//not found.
//CAUTION: because of the way the callback are done, null does not necessary
//means that the array does not exist but it can also means that it is a 0
//cases array.
hi_error get_array(jlong tag, array_info **retMap)
{
  jvmtiError err;
  array_info* find_ar;
  err=alloc(sizeof(array_info), &find_ar);
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_INSTANCE;
  }
  find_ar->tag=tag;
  find_ar->lst=NULL;
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_ALLOC;
  }
  arrayMap*r = tfind(find_ar, &gbl_arrayMap, &compare_ar_key);
  //dealloc 
  err=dealloc(find_ar);
  if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  if (r!= NULL){
      (*retMap)=r->info;
  }
  else{
      (*retMap)=NULL;
  }
  return NO_ERROR;
}

hi_error get_or_add_array(arrayMap** cmap, jlong referer_tag, jobject *
                          referer_obj, array_info**retMap)
{
  jvmtiError err;
  array_info* find_ar;
  err=alloc(sizeof(array_info), &find_ar);
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_INSTANCE;
  }
  find_ar->tag=referer_tag;
  find_ar->size=get_array_size(referer_obj);
  find_ar->lst=NULL;
  arrayMap*r = tsearch(find_ar, (void **) cmap, &compare_ar_key);
  if(r->info!=find_ar){
      //dealloc it if already present
      err=dealloc(find_ar);
      if(err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  }
  (*retMap)=r->info;
  return NO_ERROR;

}

hi_error get_or_add_array_class(char ** sig, array_class_info ** retMap )
{
  jvmtiError jvmti_err;
  array_class_info * find_ar;
  jvmti_err=alloc(sizeof(array_class_info), &find_ar);
  if(jvmti_err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_INSTANCE;
  }
  find_ar->sig= *sig;
  find_ar->map=NULL;
  arrayClassMap*r = tsearch(find_ar, &gbl_arrayMap, &compare_ar_cl_key);
  if(r->info!=find_ar){
      //dealloc it if already present
      jvmti_err=dealloc(find_ar);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  }
  (*retMap)=r->info;
  return NO_ERROR;
}

hi_error reverse_array_list(array_el_list * lst, array_el_list ** new_ptr)
{
  if(lst==NULL)
      return NO_ERROR;
  array_el_list * prev;
  array_el_list * next;
  array_el_list * cur;
  prev=NULL;
  cur=lst;
  next=lst->next;
  while(next!=NULL){
      cur->next=prev;
      prev=cur;
      cur=next;
      next=next->next;

  }
  cur->next=prev;
  *new_ptr=cur;
  return NO_ERROR;

}

hi_error add_array_element(array_el_info * ar_el, array_info **map)
{
  jvmtiError err;
  array_el_list * new_lst;
  err=alloc(sizeof(array_el_info), &new_lst);
  if(err!=JVMTI_ERROR_NONE){
      return JVMTI_ERR_ON_ALLOC;
  }
  new_lst->next=(*map)->lst;
  new_lst->value=ar_el;

  (*map)->lst =new_lst;
  return NO_ERROR;
}

void free_arrays_list(array_el_list* ar_l)
{
  array_el_list* ar_l_tmp;
  jvmtiError err;
  if(ar_l !=NULL){
      err=dealloc(ar_l->value->referer_obj);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
  }
  while (ar_l!=NULL){
      ar_l_tmp=ar_l;
      ar_l=ar_l->next;
      err=dealloc(ar_l_tmp->value);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
      err=dealloc(ar_l_tmp);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
  }
}

void free_arrays_action(const void* curNode, const VISIT v, const int depth)
{
  jvmtiError err;
  arrayMap * ar;
  if(v ==endorder){
      ar= (arrayMap*) curNode;
      free_arrays_list(ar->info->lst);
      err=dealloc(ar->info);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
  }
}

hi_error free_arrays(arrayMap* map)
{
  twalk(map, free_arrays_action);
  return NO_ERROR;
}

void free_array_class_action(const void* curNode, const VISIT v, const int depth)
{
  jvmtiError err;
  arrayClassMap* cl;
  if(v == endorder){
      cl= (arrayClassMap*) curNode;
      err=dealloc(cl->info->sig);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
      free_arrays(cl->info->map);
      err=dealloc(cl->info);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
      err=dealloc(cl);
      if(err!=JVMTI_ERROR_NONE){printf("Array deallocation error.\n");}
  }
}

hi_error free_array_class(arrayClassMap* map)
{
  twalk(map, free_array_class_action);
  return NO_ERROR;
}

//Set res to true if obj is an array. Use the java introspection to answer.
hi_error is_array(jobject * obj, jboolean * res)
{
  jclass jlclass =  (*jni_cur_env)->FindClass(jni_cur_env, "java/lang/Class");
  jclass jlObject=  (*jni_cur_env)->FindClass(jni_cur_env, "java/lang/Object");

  jmethodID idGetClass= 
    (*jni_cur_env)->GetMethodID(jni_cur_env, jlObject,"getClass",
                                "()Ljava/lang/Class;");
  jmethodID idIsAr= (*jni_cur_env)->GetMethodID(jni_cur_env, jlclass,
                                                "isArray","()Z");
  jobject classObj= 
    (*jni_cur_env)->CallObjectMethod(jni_cur_env, *obj, idGetClass);

  jboolean jres= 
    (*jni_cur_env)->CallBooleanMethod(jni_cur_env, classObj, idIsAr);
  (*res = jres);
  return NO_ERROR;
}


/* ######################## PRINTERS ######################## */

//structure used to represents exactly the byte of a float
typedef union 
{
  float f;
  unsigned int u;
} ufloat;

typedef union 
{
  double d;
  unsigned long long u;
} udouble;

char * typ2str(enum type t){
    switch(t){
      case (INT):
        return "INT";
      case (FLOAT):
        return "FLOAT";
      case(DOUBLE):
        return "DOUBLE";
      case (INT64):
        return "LONG";
      case (OBJECT):
        return "OBJECT";
      case (ARRAY):
        return "ARRAY";

    }
    return "";
}

void print_array_el(array_el_info * el)
{
  ufloat uf;
  udouble ud;

  switch(el->typ){
    case (INT):
      if(el->val.int_v!=0){
          printf("\t\t\t[%d]= %d; \n", el->index, el->val.int_v);
      }
      break;
    case (FLOAT):
      uf.f=el->val.float_v;
      printf("\t\t\t[%d]= 0X%x; \n", el->index, uf.u);
      break;
    case(DOUBLE):
      ud.d=el->val.double_v;
      printf("\t\t\t[%d]= 0X%llx; \n", el->index, ud.u);
      break;
    case (INT64):
      if(el->val.int64_v!=0){
          printf("\t\t\t[%d]= %ld; \n", el->index, el->val.int64_v);
      }
      break;
    case(OBJECT):
    case(ARRAY):
      printf("\t\t\t[%d]= %ld; \n", el->index, el->tag);
      break;
  }
}


void print_array(const void * node, const VISIT visit, int depth)
{
  arrayMap* cur_array= (arrayMap*) node;
  if (visit == preorder || visit == leaf){
      printf("\t%ld[%d]{\n", cur_array->info->tag, cur_array->info->size);
      array_el_list * cur_el= cur_array->info->lst;
      while(cur_el!=NULL){
          print_array_el(cur_el->value);
          cur_el=cur_el->next;
      }
      printf("\t}\n");
  }
}

void print_array_class(const void * node, const VISIT visit, int depth)
{
  arrayClassMap* cur_array= (arrayClassMap*) node;
  if (visit == preorder || visit == leaf){
      printf("class %s{\n", cur_array->info->sig);
      twalk(cur_array->info->map, print_array);
      printf("}\n");
  }
}


void print_field(const void * node, const VISIT visit, int depth)
{
  jlong referee_tag;
  if (visit == preorder || visit == leaf){
      fieldMap * cur_field= (fieldMap*) node;
      if(cur_field->field->is_static){
          printf("\t\tstatic ");
      }
      else{
          printf("\t\t");
      }
      ufloat uf;
      udouble ud;
      switch(cur_field->field->typ){
        case (INT):
          printf("%s %s = %d;\n", cur_field->field->signature,
                 cur_field->field->fname, cur_field->field->val.int_v);
          break;
        case (FLOAT):
          uf.f=cur_field->field->val.float_v;
          printf("%s %s = 0X%x;\n", cur_field->field->signature,
                 cur_field->field->fname, uf.u);
          break;
        case (DOUBLE):
          ud.d=cur_field->field->val.double_v;
          printf("%s %s = 0X%llx;\n", cur_field->field->signature,
                 cur_field->field->fname, ud.u);
          break;
        case (INT64):
#ifdef _LP64 //64 bits
          printf("%s %s = %ld;\n", cur_field->field->signature,
                 cur_field->field->fname, cur_field->field->val.int64_v);
#else
          printf("%s %s = %lld;\n", cur_field->field->signature,
                 cur_field->field->fname, cur_field->field->val.int64_v);
#endif
          break;
        case (OBJECT):
        case (ARRAY):
          (*jvmti_cur_env)->GetTag(jvmti_cur_env,
                                   cur_field->field->val.object_v,
                                   &referee_tag);
#ifdef _LP64 //64 bits
          printf("%s %s = %ld;\n", cur_field->field->signature,
                 cur_field->field->fname, referee_tag);
#else
          printf("%s %s = %lld;\n", cur_field->field->signature,
                 cur_field->field->fname, referee_tag);
#endif
          break;
      }
  }
}


void print_instance(const void * node, const VISIT visit, int depth)
{
  instanceMap * cur_inst = (instanceMap *) node;
  if (visit == preorder || visit == leaf){
#ifdef _LP64 //64 bits
      printf("\t%ld{\n", cur_inst->info->tag);
#else
      printf("\t%lld{\n", cur_inst->info->tag);
#endif
      twalk(cur_inst->info->fields, print_field);
      printf("\t}\n");

  }
}

void print_class(const void * node, const VISIT visit, int depth)
{
  classMap* cur_class = (classMap *) node;
  if (visit == preorder || visit == leaf){
      printf("class %s{\n", cur_class->info->name);
      twalk(cur_class->info->staticFieldMap, print_field);
      twalk(cur_class->info->instanceMap, print_instance);
      printf("}\n");
  }
}


void printer()
{
  printf("~~CLASSES~~\n");
  twalk(gbl_classMap, print_class);
  printf("~~ARRAYS~~\n");
  twalk(gbl_arrayMap, print_array_class);

}

/* ######################## PROGRAM START ######################## */

void check_jvmti_error(jvmtiEnv *jvmti, jvmtiError errnum, const char *str)
{
  if ( errnum != JVMTI_ERROR_NONE ) {
      char *errnum_str;

      errnum_str = NULL;
      (void)(*jvmti)->GetErrorName(jvmti, errnum, &errnum_str);
      printf("ERROR: JVMTI: %d(%s): %s\n", errnum, (errnum_str==NULL?"Unknown":errnum_str), (str==NULL?"":str));
  }
}

jvmtiHeapCallbacks hpCallBack;


jint get_array_id_from_ref_info(const jvmtiHeapReferenceInfo * info)
{
  return info->array.index;
}


jint get_field_id_from_ref_info(const jvmtiHeapReferenceInfo * info)
{
  return info->field.index;
}


/* ########## Structure to collect data during callback############## */


/*
 * Contain the info returned by the callback before there transformation and
 * integration to the map (classMap, InstanceMap, FieldMap...).
 */
typedef struct 
{
  enum HeapKind refer_kind;
  jint index;//in the case of an array element reference it is the index in
                 //the array. For field, it is the field index.
  jlong tag; //equal to 0 when the value is not an object.
  jlong referer_tag;
  bool is_object; //for an array, it qualified the object of the array.
  jvmtiPrimitiveType typ; //only useful when not an object
                          //for an array of primitive, represent the type of the
                          //primitive.
  tmp_jvalue value; //only useful when not an object
}tmp_ref_info;

struct _tmp_ref_list
{
  tmp_ref_info* data;
  struct _tmp_ref_list* next;
};

typedef struct _tmp_ref_list tmp_ref_list;

void ref_list_init(tmp_ref_list* ref_list)
{
  ref_list=NULL;
}

//add a new element [to_add ]to the given tmp_ref_list [ref_list].
tmp_ref_list* ref_list_add(tmp_ref_list* ref_list, tmp_ref_info* to_add)
{
  //normal add
  tmp_ref_list* new_lst = (tmp_ref_list*) malloc(sizeof (tmp_ref_list));
  new_lst->data = to_add;
  new_lst->next = ref_list;
  return new_lst;
}



void ref_list_iter(tmp_ref_list* lst, hi_error (*fun) (tmp_ref_info*))
{
  while(lst!=NULL)
    {
      fun(lst->data);
      lst = lst->next;
    }
}

void free_value(tmp_ref_info * info)
{
  switch(info->refer_kind){
    case(PRIMITIVE_ARRAY_ELEMENTS):
      dealloc(info->value.array_val.array);
      break;
    default:
      break;
  }
}

void free_ref_list(tmp_ref_list* ref_list)
{
  while(ref_list!=NULL){
      free_value(ref_list->data);
      free(ref_list->data);
      tmp_ref_list * next= ref_list->next;
      free(ref_list);
      ref_list=next;
  }
}

/* ######## End of structure to collect data during callback ######### */


hi_error check_reference_fields(tmp_ref_info* tmp_ref)
{
  hi_error err;
  jvmtiError jvmti_err;
  jobject * referee_obj=NULL; //only not null if the value is an object. 
                              //Allocated by object_from_tag method.
  jobject * referer_obj;      //Allocated by object_from_tag method.
  jfieldID* fieldId;
  jclass * referee_class=NULL; //only not null if the value is an object
  class_info* c_info=NULL;
  instanceMap * imap;
  jclass * referer_class;
  field_info* fi;
  char * name;
  char * referer_sig;
  char * referee_sig; //object dynamic signature
  char * referee_st_sig; //static field signature
  jboolean is_array=false;
  array_class_info * c_array; //used if referee is an array
  array_info * arrayMap; //used if referee is an array
  //do allocation first
  jvmti_err=alloc(sizeof(jfieldID),&fieldId);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  jvmti_err=alloc(sizeof(jclass),&referer_class);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  jvmti_err=alloc(sizeof(char)*256,&name);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}

  err= object_from_tag(tmp_ref->referer_tag, &referer_obj);
  if(err!=NO_ERROR){return err;}
  err= class_from_object(referer_obj, &referer_class);
  if(err!=NO_ERROR){return err;}
  err =signature_from_class(referer_class, &referer_sig);
  if(err== JVMTI_ERR_ON_SIG){
      //anonymous class error
      return NO_ERROR;
  }
  if(err!=NO_ERROR){return err;}

  if(tmp_ref->is_object){
      jvmti_err=alloc(sizeof(jclass),&referee_class);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
      err= object_from_tag(tmp_ref->tag, &referee_obj);
      if(err!=NO_ERROR){return err;}
      err=class_from_object(referee_obj,&referee_class);
      if(err!=NO_ERROR){return err;}
      err =signature_from_class(referee_class, &referee_sig);
      if(err!=NO_ERROR){return err;}
      jvmti_err= (*jvmti_cur_env)->IsArrayClass(jvmti_cur_env, *referee_class, &is_array);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  }
  else{
      err=signature_from_primitive(tmp_ref->typ, &referee_sig);
      if(err!=NO_ERROR){return err;}
  }
  err=fieldId_from_idx(referer_class,tmp_ref->index, &fieldId);
  if(err!=NO_ERROR){return err;}
  jvmti_err=alloc(sizeof(field_info), &fi);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  fi->is_static= false;
  fi->id=tmp_ref->index;
  fi->fid=fieldId;
  if(tmp_ref->is_object){
      fi->typ=OBJECT;
      fi->val.object_v=(*referee_obj);
  }
  else{
      fi->typ=jvmtiTyp2Typ(tmp_ref->typ);
      fi->val=jvmtiValue2Value(tmp_ref->typ, tmp_ref->value);
  }
  err = get_field_name(*referer_class, *fieldId, &name);
  if (err!=NO_ERROR){return err;}
  err = get_field_signature(*referer_class, *fieldId, &referee_st_sig);
  if (err!=NO_ERROR){return err;}
  fi->fname=name;
  fi->is_array=is_array;
  fi->signature=referee_st_sig;
  //if the field is an object (and not an array), put the instance
  if(!(is_array) && (tmp_ref->is_object)){
      err= get_or_add_class(referee_sig,&c_info);
      if(err!=NO_ERROR){return err;}
      err= get_or_add_instance(tmp_ref->tag,&(c_info->instanceMap), &imap);
      if(err!=NO_ERROR){return err;}
  }
  else{
      //Add array which are referenced as field but do not have any elements
      //This is needed to get 0 elements array
      if(is_array){
          err= get_or_add_array_class(&referee_sig, &c_array);
          if(err!=NO_ERROR){return err;}
          err = get_or_add_array(&(c_array->map), tmp_ref->tag,
                                 referee_obj,&arrayMap);
      }
  }
  //put in map the field
  err= get_or_add_class(referer_sig,&c_info);
  if(err!=NO_ERROR){return err;}
  err= get_or_add_instance(tmp_ref->referer_tag,&(c_info->instanceMap), &imap);
  if(err!=NO_ERROR){return err;}
  if(!add_field(&(imap->info->fields), fi)){
      //if fi was already present, dealloc it
      jvmti_err=dealloc(fi->fname);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
      jvmti_err=dealloc(fi);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  }
  if(err!=NO_ERROR){return err;}
  //free what is no more used
  jvmti_err=dealloc(referer_class);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}
  jvmti_err=dealloc(referer_obj);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}

  return NO_ERROR;
}

hi_error check_static_reference_fields(tmp_ref_info* tmp_ref)
{
  hi_error err;
  jvmtiError jvmti_err;
  jobject * referee_obj = NULL; //only not null if the value is an object
                                //Allocated by object_from_tag method.
  jobject * referer_obj;        //Allocated by object_from_tag method.
  jfieldID* fieldId;
  jclass * referee_class = NULL; //only not null if the value is an object
  class_info* c_info;
  jclass referer_class;
  field_info* fi;
  char * name;
  char * referer_sig=NULL;
  char * referee_sig=NULL;//dynamic sig
  char * referee_st_sig=NULL;//static sig
  jboolean is_array=false;
  array_class_info * c_array; //used if referee is an array
  array_info * arrayMap; //used if referee is an array

  //do allocation first
  jvmti_err=alloc(sizeof(jfieldID),&fieldId);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}


  /*The referer_obj is an instance of the class java.lang.Class. The
    function class_from_class_object allows to get from this object a
    reference to the represented classe.
  */
  err= object_from_tag(tmp_ref->referer_tag, &referer_obj);
  if(err!=NO_ERROR){return err;}
  err=class_from_class_object(referer_obj, &referer_class);
  if(err!=NO_ERROR){return err;}
  err =signature_from_class(&referer_class, &referer_sig);
  if(err == JVMTI_ERR_ON_SIG)
    { //anonymous class error
      return NO_ERROR;
    }
  if(err!=NO_ERROR){return err;}
  jvmti_err=alloc(sizeof(char)*256,&name);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}

  if(tmp_ref->is_object){
      jvmti_err=alloc(sizeof(jclass),&referee_class);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
      err= object_from_tag(tmp_ref->tag, &referee_obj);
      if(err!=NO_ERROR){return err;}
      err=class_from_object(referee_obj,&referee_class);
      if(err!=NO_ERROR){return err;}
      err =signature_from_class(referee_class, &referee_sig);
      if(err!=NO_ERROR){return err;}
      jvmti_err= (*jvmti_cur_env)->IsArrayClass(jvmti_cur_env, *referee_class, &is_array);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  }
  else{
      err=signature_from_primitive(tmp_ref->typ, &referee_sig);
      if(err!=NO_ERROR){return err;}
  }
  err=fieldId_from_idx(&referer_class,tmp_ref->index, &fieldId);
  if(err!=NO_ERROR){return err;}
  jvmti_err=alloc(sizeof(field_info), &fi);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  fi->is_static= true;
  fi->id=tmp_ref->index;
  fi->fid=fieldId;
  if(tmp_ref->is_object){
      fi->typ=OBJECT;
      fi->val.object_v=(*referee_obj);
  }
  else{
      fi->typ=jvmtiTyp2Typ(tmp_ref->typ);
      fi->val=jvmtiValue2Value(tmp_ref->typ, tmp_ref->value);
  }
  err = get_field_name(referer_class, *fieldId, &name);
  if (err!=NO_ERROR){return err;}
  err = get_field_signature(referer_class, *fieldId, &referee_st_sig);
  if (err!=NO_ERROR){return err;}
  fi->fname=name;
  fi->is_array=is_array;
  fi->signature=referee_st_sig;
  //if the field is an object (and not an array), put the instance
  if(!(is_array) && (tmp_ref->is_object)){
      err= get_or_add_class(referee_sig,&c_info);
      if(err!=NO_ERROR){return err;}
      err= get_or_add_instance(tmp_ref->tag,&(c_info->instanceMap),NULL);
      if(err!=NO_ERROR){return err;}
  }
  else{
      //Add array which are referenced as field but do not have any elements
      //This is needed to get 0 elements array
      if(is_array){
          err= get_or_add_array_class(&referee_sig, &c_array);
          if(err!=NO_ERROR){return err;}
          err = get_or_add_array(&(c_array->map), tmp_ref->tag,
                                 referee_obj,&arrayMap);
      }
  }
  //put the field in map
  err= get_or_add_class(referer_sig,&c_info);
  if(err!=NO_ERROR){return err;}
  if(!add_field(&(c_info->staticFieldMap), fi))
    {
      //if fi was already present, dealloc it
      jvmti_err=dealloc(fi);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_FIELD;}
    }
  if(err!=NO_ERROR){return err;}
  //dealloc
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_DEALLOC;}

  return NO_ERROR;
}

hi_error check_reference_array_element(tmp_ref_info* tmp_ref)
{
  hi_error err;
  jobject * referee_obj;    //Allocated by object_from_tag method.
  jobject * referer_obj;    //Allocated by object_from_tag method.
  array_class_info * array_cl;
  jclass * referer_class;
  char * referer_sig;
  array_info * array;
  array_el_info * ar_el_info;
  jvmtiError jvmti_err;
  jboolean referee_is_ar;
  class_info* c_info=NULL;

  char * referee_sig; //object dynamic signature
  jclass * referee_class=NULL; //only not null if the value is an object
  
  //feed ar_el_info
   err= object_from_tag(tmp_ref->tag, &referee_obj);
  if(err!=NO_ERROR){return err;;}
  err= object_from_tag(tmp_ref->referer_tag, &referer_obj);
  if(err!=NO_ERROR){return err;;}
  jvmti_err=alloc(sizeof(jclass),&referer_class);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}


  jvmti_err=alloc(sizeof(array_el_info), &ar_el_info);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}

  ar_el_info->tag=tmp_ref->tag;
  ar_el_info->referer_obj=referer_obj;
  ar_el_info->index=tmp_ref->index;
  err=is_array(referee_obj, &referee_is_ar);
  if(err!=NO_ERROR){return err;}
  if(referee_is_ar){
      ar_el_info->typ=ARRAY;
  }
  else{
      ar_el_info->typ=OBJECT;
  }
  ar_el_info->val.object_v=(*referee_obj);
  //put the represented instance in the map
  err=class_from_object(referer_obj,&referer_class);
  if(err!=NO_ERROR){return err;}
  err =signature_from_class(referer_class, &referer_sig);
  if(err == JVMTI_ERR_ON_SIG)
    { //anonymous class error
      return NO_ERROR;
    }
  if(err!=NO_ERROR){return err;}
  if(!referee_is_ar){

      jvmti_err=alloc(sizeof(jclass),&referee_class);
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
      err= object_from_tag(tmp_ref->tag, &referee_obj);
      if(err!=NO_ERROR){return err;}
      err=class_from_object(referee_obj,&referee_class);
      if(err!=NO_ERROR){return err;}
      err =signature_from_class(referee_class, &referee_sig);
      if(err!=NO_ERROR){return err;}


      err= get_or_add_class(referee_sig,&c_info);
      if(err!=NO_ERROR){return err;}
      err= get_or_add_instance(tmp_ref->tag,&(c_info->instanceMap), NULL);
      if(err!=NO_ERROR){return err;}
      err=dealloc(referee_class);
      if(err!=NO_ERROR){return err;}
  }
  //put the add element in its array
  err=get_or_add_array_class(&referer_sig, &array_cl);
  if(err!=NO_ERROR){return err;}
  err=get_or_add_array(&(array_cl->map),tmp_ref->referer_tag, referer_obj, &array);
  if(err!=NO_ERROR){return err;}
  err= add_array_element (ar_el_info,&array);
  if(err!=NO_ERROR){return err;}
  dealloc(referer_class);
  return NO_ERROR;
}

hi_error check_primitive_array(tmp_ref_info * tmp_ref)
{
  hi_error err;
  array_el_info ** ar_result;
  tmp_jvalue_ar  tmp_array= tmp_ref->value.array_val;
  long ar_size = tmp_array.nb_el;
  jobject * referer_obj;      //Allocated by object_from_tag method.
  array_info * array;
  jvmtiError jvmti_err;
  int i =0;
  char * sig=NULL;
  array_class_info * array_cl;

  jvmti_err = alloc(sizeof(array_el_info*)*ar_size, &ar_result);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  err= object_from_tag(tmp_ref->referer_tag, &referer_obj);
  if(err!=NO_ERROR){return err;}
  err= array_sig_from_primitive(tmp_ref->typ, &sig);
  if(err!=NO_ERROR){return err;}
  for (i = ar_size-1; i >= 0; i--){
      jvmti_err = alloc(sizeof(array_el_info), &(ar_result[i]));
      if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
      ar_result[i]->tag=-1; //no tag for primitive
      ar_result[i]->referer_obj=referer_obj;
      ar_result[i]->index=i;
      ar_result[i]->typ=jvmtiTyp2Typ(tmp_ref->typ);
      ar_result[i]->val=tmp_array.array[i];
      err=get_or_add_array_class(&sig, &array_cl);
      if(err!=NO_ERROR){return err;}
      err=get_or_add_array(&(array_cl->map), tmp_ref->referer_tag, referer_obj, &array);
      if(err!=NO_ERROR){return err;}
      err= add_array_element (ar_result[i],&array);
      if(err!=NO_ERROR){return err;}
  }
  jvmti_err=dealloc(ar_result);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR;}
  return NO_ERROR;
}

hi_error check_reference_class(tmp_ref_info * tmp_ref){
  hi_error err;
  jvmtiError jvmti_err;
  jobject * referee_obj=NULL; 
  jclass * referee_class=NULL; 
  char * referee_sig; //object dynamic signature
  class_info* c_info=NULL;
  instanceMap * imap;

  err= object_from_tag(tmp_ref->tag, &referee_obj);
  if(err!=NO_ERROR){return err;}
  jvmti_err=alloc(sizeof(jclass),&referee_class);
  if(jvmti_err!=JVMTI_ERROR_NONE){return JVMTI_ERR_ON_ALLOC;}
  err=class_from_object(referee_obj,&referee_class);
  if(err!=NO_ERROR){return err;}
  err =signature_from_class(referee_class, &referee_sig);
  if(err!=NO_ERROR){return err;}

  err= get_or_add_class(referee_sig,&c_info);
  if(err!=NO_ERROR){return err;}
  err= get_or_add_instance(tmp_ref->tag,&(c_info->instanceMap), &imap);
  if(err!=NO_ERROR){return err;}
  err=dealloc(referee_class);
  if(err!=NO_ERROR){return err;}


  return NO_ERROR;
}

hi_error check_ref(tmp_ref_info* tmp_ref)
{
  hi_error err=NO_ERROR;

  switch (tmp_ref->refer_kind){
    case REFERENCE_FIELD:
      err=check_reference_fields(tmp_ref);
      break;
    case REFERENCE_STATIC_FIELD:
      err=check_static_reference_fields(tmp_ref);
      if(err ==BAD_CLASS){
          //can be because of anonymous class
          return NO_ERROR;
      }
      break;
    case REFERENCE_ARRAY_ELEMENT:
      err=check_reference_array_element(tmp_ref);
      break;
    case PRIMITIVE_ARRAY_ELEMENTS:
      err=check_primitive_array(tmp_ref);
      break;
    case REFERENCE_CLASS:
      err= check_reference_class(tmp_ref); 
      break;
    default:
      break;
  }
  if(err!=NO_ERROR){goto error_hdl;};
  return NO_ERROR;

error_hdl:
  printf("An error has been throwed while trying to follow reference.");
  switch(err){
    case UNTAGGED_OBJ: printf("Untagged Object.");
                       break;
    case JVMTI_ERR_ON_IDX: printf("JVMTI index error");
                           break;
    case JVMTI_ERR_ON_TAG: printf("JVMTI tag error");
                           break;
    case JVMTI_ERR_ON_SIG: printf("JVMTI signature error");
                           break;
    case JVMTI_ERR_ON_FIELDID: printf("JVMTI fieldID error");
                               break;
    case JVMTI_ERR_ON_FIELD: printf("JVMTI field error");
                             break;
    case JVMTI_ERR_ON_INSTANCE: printf("JVMTI instance error");
                                break;
    case JVMTI_ERR_ON_ALLOC: printf("JVMTI alloc error");
                             break;
    case JVMTI_ERR_ON_DEALLOC: printf("JVMTI deallocation error");
                               break;
    case JVMTI_ERR: printf("JVMTI error");
                    break;
    case JVMTI_ERR_ON_CLASS: printf("JVMTI class error");
                             break;
    case JVMTI_ERR_ON_ARRAY: printf("JVMTI array error");
                             break;
    case INVALID_NULL: printf("JVMTI invalid null");
                       break;
    case BAD_CLASS: printf("Bad Class");
                    break;
    case NOT_AN_ARRAY: printf("Not an array error");
                       break;

    default:
                       printf("Unknown error!");
                       break;
  }
  exit(1);
}

//Contains every data collected during the callback
tmp_ref_list* collected_ref;

/*Called when heap is iterating on object's references
 * This is not possible to access envrionment variables from theses callback.
 * So we store data in a temporary tmp_ref_info structure which will then be
 * transformed.
 * */
jint JNICALL hp_reference_callback (jvmtiHeapReferenceKind reference_kind, 
                                    const jvmtiHeapReferenceInfo* reference_info, 
                                    jlong class_tag, 
                                    jlong referrer_class_tag, 
                                    jlong size, 
                                    jlong* tag_ptr, 
                                    jlong* referrer_tag_ptr, 
                                    jint length, 
                                    void* user_data)
{
  jint id;
  tmp_ref_info* i ;
  jlong tag;
  jlong referer_tag;

  switch (reference_kind){
    case JVMTI_HEAP_REFERENCE_CLASS:
    case JVMTI_HEAP_REFERENCE_FIELD:
    case JVMTI_HEAP_REFERENCE_STATIC_FIELD:
    case JVMTI_HEAP_REFERENCE_ARRAY_ELEMENT:
      i= (tmp_ref_info*)malloc(sizeof(tmp_ref_info));
      i->is_object=true;
      i->typ=-1;
      i->refer_kind=jvmtiRefKind2RefKind(reference_kind);
      if(reference_kind== JVMTI_HEAP_REFERENCE_ARRAY_ELEMENT ){
          id=get_array_id_from_ref_info(reference_info);
          i->index=id;
      }
      else{
          if(reference_kind == JVMTI_HEAP_REFERENCE_FIELD || 
             reference_kind == JVMTI_HEAP_REFERENCE_STATIC_FIELD)
            {
              id=get_field_id_from_ref_info(reference_info);
              i->index=id;
            }
          else {
              i->index=0;
          }
      }
      if((*tag_ptr) == 0){
          tag=generate_tag();
          i->tag=tag;
          (*tag_ptr)=tag;
      }
      else{
          i->tag=(*tag_ptr);
      }
      if((*referrer_tag_ptr) == 0){
          referer_tag=generate_tag();
          i->referer_tag=referer_tag;
          (*referrer_tag_ptr)=referer_tag;
      }
      else{
          i->referer_tag=(*referrer_tag_ptr);
      }
      collected_ref=ref_list_add(collected_ref, i);
      break;
    default:
      break;
  }
  return JVMTI_VISIT_OBJECTS;

}

jint JNICALL hp_primitive_field_callback (jvmtiHeapReferenceKind kind, 
                                          const jvmtiHeapReferenceInfo* info, 
                                          jlong object_class_tag, 
                                          jlong* object_tag_ptr, 
                                          jvalue value, 
                                          jvmtiPrimitiveType value_type, 
                                          void* user_data)
{
  tmp_ref_info * i;
  jlong tag;
  switch (kind){
    case JVMTI_HEAP_REFERENCE_FIELD:
    case JVMTI_HEAP_REFERENCE_STATIC_FIELD:
      i= (tmp_ref_info*)malloc(sizeof(tmp_ref_info));
      i->is_object=false;
      i->typ=value_type;
      i->value.simple_val=value;
      i->refer_kind=jvmtiRefKind2RefKind(kind);
      i->index=get_field_id_from_ref_info(info);
      if((*object_tag_ptr) == 0){
          tag=generate_tag();
          i->referer_tag=tag;
          (*object_tag_ptr)=tag;
      }
      else{
          i->referer_tag=(*object_tag_ptr);
      }
      i->tag=0; //never used.
      collected_ref=ref_list_add(collected_ref, i);
      break;
    default:
      break;
  }
  return JVMTI_VISIT_OBJECTS;
}

jint JNICALL hp_primitive_array_callback(jlong class_tag, 
                                         jlong size, //in bytes
                                         jlong* tag_ptr, 
                                         jint element_count, 
                                         jvmtiPrimitiveType element_type, 
                                         const void* elements, 
                                         void* user_data)
{
  tmp_ref_info * i;
  jlong tag;

  i= (tmp_ref_info*)malloc(sizeof(tmp_ref_info));
  i->is_object=false;
  i->typ=element_type;
  i->refer_kind=PRIMITIVE_ARRAY_ELEMENTS;
  i->index=-1;//unused
  if((*tag_ptr) == 0){
      tag=generate_tag();
      i->referer_tag=tag;
      (*tag_ptr)=tag;
  }
  else{
      i->referer_tag=(*tag_ptr);
  }
  i->tag=0; //never used.
  i->value.array_val=jvmtiArrayEl2arrayEl(element_type,element_count,elements);
  collected_ref=ref_list_add(collected_ref, i);
  return JVMTI_VISIT_OBJECTS;
}

void free_gbl_struct()
{
  //free collected_ref
  free_ref_list(collected_ref);
  free_classes(gbl_classMap);
  free_array_class(gbl_arrayMap);
}

bool init_found = false;

void JNICALL callbackMethodEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method)
{
  char *methodName;
  char *methodSignature;
  char *declaringClassName;
  jclass declaring_class;
  jvmtiThreadInfo thread_info;
  jvmtiError err;
  /*Set current env (we only analyse this environment).*/
  jni_cur_env=jni_env;
  jvmti_cur_env=jvmti_env;


  err = (*jvmti_cur_env)->GetMethodName(jvmti_cur_env, method, &methodName,
                                        &methodSignature, NULL);
  err = (*jvmti_cur_env)->GetMethodDeclaringClass(jvmti_cur_env, method,
                                                  &declaring_class);
  err = (*jvmti_cur_env)->GetClassSignature(jvmti_cur_env, declaring_class,
                                            &declaringClassName, NULL);
  err = (*jvmti_cur_env)->GetThreadInfo(jvmti_cur_env, NULL, &thread_info);

  if (err == JVMTI_ERROR_NONE && strcmp(thread_info.name,"DestroyJavaVM") 
      && !init_found){
      if(((!strcmp(methodName,"<clinit>")) && !(strcmp(declaringClassName,gbl_main_class))) ||
         ((!strcmp(methodName,"main")) && !(strcmp(declaringClassName,gbl_main_class)))) {
          init_found = true;
          gbl_classMap=NULL;
          gbl_arrayMap=NULL;
          //set the heap callback used when following references.
          hpCallBack.heap_reference_callback=&hp_reference_callback;
          hpCallBack.primitive_field_callback=&hp_primitive_field_callback;
          hpCallBack.array_primitive_value_callback=&hp_primitive_array_callback;

          collected_ref=NULL;
          err = (*jvmti_cur_env)->FollowReferences(jvmti_cur_env,0, NULL, NULL, 
                                                   (&hpCallBack),NULL);
          //data have been put in tmp_ref_list, now transform to put in the
          //main map.
          ref_list_iter(collected_ref, check_ref);
          //print
          printer();
          //free data
          free_gbl_struct();
          //we can now exit program execution
          //prog_exit();
          dealloc(declaringClassName);
          dealloc(methodSignature);
          dealloc(methodName);
          /*Exit the jvm*/
          exit(0);
      }
  }
  dealloc(declaringClassName);
  dealloc(methodSignature);
  dealloc(methodName);

}

JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *jvm, char *options, void *reserved)
{
  jvmtiError error;
  jint res;
  jvmtiEventCallbacks callbacks;
  jvmtiCapabilities capa;
  

  /*  We need to first get the jvmtiEnv* or JVMTI environment */
  jvmtiEnv *jvmti; 
  res = (*jvm)->GetEnv(jvm, (void **) &jvmti, JVMTI_VERSION_1_0);

  if (res != JNI_OK || jvmti == NULL) {
    /* This means that the VM was unable to obtain this version of the
     *   JVMTI interface, this is a fatal error.
     */
    printf("ERROR: Unable to access JVMTI Version 1 (0x%x),"
	   " is your J2SE a 1.5 or newer version?"
	   " JNIEnv's GetEnv() returned %d\n",
	   JVMTI_VERSION_1, res);
    
  }
  cur_vm=jvm;
  if(((strcmp (options, "help"))==0) || 
     (options == NULL) || 
     (strcmp(options, "")==0))
    {
      printf("HeapInspector is a program allowing to dump the initial heap of a java program. \nIt comes as an agent embedded in the JVM while running the program we want to analyze. \nIt needs to take the main program class as argument so a typical run is: \n\njava -agentlib:HeapInspector=\"pkg.MainClass\" pkg.MainClass\n\n");
      return JNI_OK;
    }
  
  gbl_main_class=(char*)malloc(sizeof(char)*255);
  if(gbl_main_class==NULL){
      printf("JVMTI memory error (allocation failed).\n"); 
      exit(1);
  }
  userSig_2_internalSig(options, &gbl_main_class);

  (void)memset(&capa, 0, sizeof(jvmtiCapabilities));
  capa.can_access_local_variables= 1;
  capa.can_generate_method_entry_events = 1;
  capa.can_generate_vm_object_alloc_events = 1;
  capa.can_tag_objects= 1;
  
  error = (*jvmti)->AddCapabilities(jvmti, &capa);
  check_jvmti_error(jvmti, error, "Unable to get necessary JVMTI capabilities.");
  
  
  (void)memset(&callbacks, 0, sizeof(callbacks));
  callbacks.MethodEntry = &callbackMethodEntry; /* JVMTI_EVENT_METHOD_ENTRY */
  
  error = (*jvmti)->SetEventCallbacks(jvmti, &callbacks, (jint)sizeof(callbacks));
  check_jvmti_error(jvmti, error, "Cannot set jvmti callbacks");
  
  /* At first the only initial events we are interested in are VM
   *   initialization, VM death, and Class File Loads.
   *   Once the VM is initialized we will request more events.
   */
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE, JVMTI_EVENT_METHOD_ENTRY, (jthread)NULL);
  check_jvmti_error(jvmti, error, "Cannot set event notification");


  /* We return JNI_OK to signify success */
  return JNI_OK;
}

JNIEXPORT void JNICALL Agent_OnUnload(JavaVM *vm)
{
  if (!init_found)
    {
      printf("Main method has not been found. Check that you have given a correct class as argument.\nYou can get more heap with HeapInspector=help.\n");

    }
}
