#ifndef WCHAR
typedef unsigned short WCHAR;
#endif

int is_valid_utf8(const char *s);
unsigned char * ansi_to_utf16(const char * str);
unsigned char * utf8_to_utf16(const char * str);
unsigned char * utf16_to_utf8(const unsigned char * str);
