/* MD5 message digest */

#ifndef _md5
#define _md5


#include "mlvalues.h"
#include "io.h"

value md5_string (value str, value ofs, value len);
value md5_chan (value vchan, value len);

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

void MD5Init (struct MD5Context *context);
void MD5Update (struct MD5Context *context, unsigned char *buf, unsigned len);
void MD5Final (unsigned char *digest, struct MD5Context *ctx);
void MD5Transform (uint32 *buf, uint32 *in);


#endif
