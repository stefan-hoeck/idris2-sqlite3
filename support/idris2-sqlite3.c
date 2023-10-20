#include <stdlib.h>
#include <stdio.h>
#include <sqlite3.h>

void* newptr(){
    return malloc(sizeof(void*));
}

void* deref(void** ptr){
  return *ptr;
}

void ptr_free(void* ptr){
  free(ptr);
}

char* getString(void* str) {
    return (char*)str;
}

void* mkString(char* str) {
    return (void*)str;
}

void copy_buffer(int length, unsigned char* buf, unsigned char* bytes) {
  int i;
  for (i = 0; i < length; i++) {
    buf[i] = bytes[i];
  }
}


void sqlver(void) {
    printf("%s\n", sqlite3_libversion());
}

void* null(void) {
    return NULL;
}

int isNull(void* ptr) {
    return ptr==NULL;
}

int bind_buffer(sqlite3_stmt* stmt, int index, const void* buf, int length) {
    return sqlite3_bind_blob(stmt, index, buf, length, SQLITE_TRANSIENT);
}

int bind_text(sqlite3_stmt* stmt, int index, const char* str) {
    return sqlite3_bind_text(stmt, index, str, -1, SQLITE_TRANSIENT);
}
