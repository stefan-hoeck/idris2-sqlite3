void* newptr()

void* deref(void** ptr)

void ptr_free(void* ptr)

char* getString(void* str)

void* mkString(char* str)

void sqlver(void)

void* null(void)

int isNull(void* ptr)

void copy_buffer(int length, unsigned char* buf, unsigned char* bytes)

int bind_buffer(sqlite3_stmt* stmt, int index, const void* buf, int length)

int bind_text(sqlite3_stmt* stmt, int index, const char* str)
