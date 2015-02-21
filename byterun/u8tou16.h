#ifdef HAS_WINAPI_UTF16

#ifndef WCHAR
typedef unsigned short WCHAR;
#endif

int is_valid_utf8(const char *s);
WCHAR* ansi_to_utf16(const char * str);
WCHAR* utf8_to_utf16(const char * str);
WCHAR* to_utf16(const char* str);
char * utf16_to_utf8(const WCHAR* str);

typedef WCHAR CRT_CHAR;
typedef WCHAR* CRT_STR;
#define CRT_(func) _w ## func
#define WINAPI_(func) func ## W
#define Crt_str_val(v) to_utf16(String_val(v))
#define caml_copy_crt_str caml_copy_utf16

#else

typedef char CRT_CHAR;
typedef char* CRT_STR;
#define CRT_(func) func
#define WINAPI_(func) func ## A
#define Crt_str_val(v) caml_strdup(String_val(v))
#define caml_copy_crt_str caml_copy_string

#endif

#define Crt_str_free(x) caml_stat_free(x)
