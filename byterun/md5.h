/* MD5 message digest */

#ifndef _md5
#define _md5


#include "mlvalues.h"
#include "io.h"

value md5_string P((value str, value ofs, value len));
value md5_chan P((value vchan, value len));

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

void MD5Init P((struct MD5Context *context));
void MD5Update P((struct MD5Context *context, unsigned char *buf,
               unsigned len));
void MD5Final P((unsigned char digest[16], struct MD5Context *ctx));
void MD5Transform P((uint32 buf[4], uint32 in[16]));


#endif
