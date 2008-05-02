/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of Objective Caml            */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the Objective Caml source tree. */
/*                                                                     */
/***********************************************************************/
#define TKANIM_VERSION "1.0"
/* #define TKANIM_DEBUG */

#include <tk.h>
#include <string.h>

/*
 * The format record for the Animated GIF file format:
 */

static int      FileMatchGIF _ANSI_ARGS_((FILE *f, char *fileName,
                    char *formatString, int *widthPtr, int *heightPtr));
static int      FileReadGIF  _ANSI_ARGS_((Tcl_Interp *interp,
                    FILE *f, char *fileName, char *formatString));

#define INTERLACE               0x40
#define LOCALCOLORMAP           0x80
#define BitSet(byte, bit)       (((byte) & (bit)) == (bit))
#define MAXCOLORMAPSIZE         256
#define CM_RED                  0
#define CM_GREEN                1
#define CM_BLUE                 2
#define MAX_LWZ_BITS            12
#define LM_to_uint(a,b)         (((b)<<8)|(a))
#define ReadOK(file,buffer,len) (fread(buffer, len, 1, file) != 0)

/*
 * Prototypes for local procedures defined in this file:
 */

static int              DoExtension _ANSI_ARGS_((FILE *fd, int label,
                            int *transparent, int *delay, int *loop));
static int              GetCode _ANSI_ARGS_((FILE *fd, int code_size,
                            int flag));
static int              GetDataBlock _ANSI_ARGS_((FILE *fd,
                            unsigned char *buf));
static int              LWZReadByte _ANSI_ARGS_((FILE *fd, int flag,
                            int input_code_size));
static int              ReadColorMap _ANSI_ARGS_((FILE *fd, int number,
                            unsigned char buffer[3][MAXCOLORMAPSIZE]));
static int              ReadGIFHeader _ANSI_ARGS_((FILE *f, int *widthPtr,
                            int *heightPtr));
static int              ReadImage _ANSI_ARGS_((Tcl_Interp *interp,
                            char *imagePtr, FILE *fd, int len, int height,
                            unsigned char cmap[3][MAXCOLORMAPSIZE],
                            int interlace, int transparent));

static int
FileMatchGIF(f, fileName, formatString, widthPtr, heightPtr)
    FILE *f;                    /* The image file, open for reading. */
    char *fileName;             /* The name of the image file. */
    char *formatString;         /* User-specified format string, or NULL. */
    int *widthPtr, *heightPtr;  /* The dimensions of the image are
                                 * returned here if the file is a valid
                                 * raw GIF file. */
{
        return ReadGIFHeader(f, widthPtr, heightPtr);
}

static int
FileReadGIF(interp, f, fileName, formatString)
    Tcl_Interp *interp;         /* Interpreter to use for reporting errors. */
    FILE *f;                    /* The image file, open for reading. */
    char *fileName;             /* The name of the image file. */
    char *formatString;         /* User-specified format string, or NULL. */
{
    int logicalWidth, logicalHeight;
    int nBytes;
    Tk_PhotoImageBlock block;
    unsigned char buf[100];
    int bitPixel;
    unsigned int colorResolution;
    unsigned int background;
    unsigned int aspectRatio;
    unsigned char localColorMap[3][MAXCOLORMAPSIZE];
    unsigned char colorMap[3][MAXCOLORMAPSIZE];
    int useGlobalColormap;
    int transparent = -1;
    int delay = 0;
    Tk_Window winPtr;
    int imageLeftPos, imageTopPos, imageWidth, imageHeight;
    Tk_PhotoHandle photoHandle;

    char widthbuf[32], heightbuf[32];
    Tcl_DString resultbuf;

    char newresbuf[640];
    char *imageName;
    char *resultptr;
    int loop = -1;

    if((winPtr = Tk_MainWindow(interp)) == NULL){
        return TCL_ERROR;
    }

#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\t\tHeader check...");
#endif
    if (!ReadGIFHeader(f, &logicalWidth, &logicalHeight)) {
        Tcl_AppendResult(interp, "couldn't read GIF header from file \"",
                fileName, "\"", NULL);
        return TCL_ERROR;
    }
#ifdef TKANIM_DEBUG
    fprintf(stderr, "done ");
#endif
    if ((logicalWidth <= 0) || (logicalHeight <= 0)) {
        Tcl_AppendResult(interp, "GIF image file \"", fileName,
                "\" has dimension(s) <= 0", (char *) NULL);
        return TCL_ERROR;
    }

    if (fread(buf, 1, 3, f) != 3) {
        return TCL_OK;
    }
    bitPixel = 2<<(buf[0]&0x07);
    colorResolution = (((buf[0]&0x70)>>3)+1);
    background = buf[1];
    aspectRatio = buf[2];

    if (BitSet(buf[0], LOCALCOLORMAP)) {    /* Global Colormap */
        if (!ReadColorMap(f, bitPixel, colorMap)) {
            Tcl_AppendResult(interp, "error reading color map",
                    (char *) NULL);
            return TCL_ERROR;
        }
    }

#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\t\tReading frames ");
    prevpos = ftell(f);
#endif
    sprintf( widthbuf, "%d ", logicalWidth);
    sprintf( heightbuf, "%d ", logicalHeight);

    Tcl_DStringInit(&resultbuf);
    Tcl_DStringAppend(&resultbuf, widthbuf, -1);
    Tcl_DStringAppend(&resultbuf, " ", -1);
    Tcl_DStringAppend(&resultbuf, heightbuf, -1);
    Tcl_DStringAppend(&resultbuf, " ", -1);
    Tcl_DStringAppend(&resultbuf, "{", -1);
    
    while (1) {
        if (fread(buf, 1, 1, f) != 1) {
            /*
             * Premature end of image.  We should really notify
             * the user, but for now just show garbage.
             */
#ifdef TKANIM_DEBUG
    fprintf(stderr, "Premature end of image");
#endif

            break;
        }

        if (buf[0] == ';') {
            /*
             * GIF terminator.
             */
#ifdef TKANIM_DEBUG
    fprintf(stderr, ";");
    prevpos = ftell(f);
#endif

            break;
        }

        if (buf[0] == '!') {
            /*
             * This is a GIF extension.
             */
#ifdef TKANIM_DEBUG
    fprintf(stderr, "!");
    prevpos = ftell(f);
#endif

            if (fread(buf, 1, 1, f) != 1) {
                Tcl_AppendResult( interp,
                 "error reading extension function code in GIF image", NULL );
/*
                interp->result =
                        "error reading extension function code in GIF image";
*/
                goto error;
            }
            if (DoExtension(f, buf[0], &transparent, &delay, &loop) < 0) {
                Tcl_AppendResult( interp,
                 "error reading extension in GIF image", NULL );
/*
                interp->result = "error reading extension in GIF image";
*/              goto error;
            }
            continue;
        }

        if (buf[0] == '\0') {
            /*
             * Not a valid start character; ignore it.
             */
#ifdef TKANIM_DEBUG
            fprintf(stderr, "0", buf[0]);
            prevpos = ftell(f);
#endif
            continue;
        }

        if (buf[0] != ',') {
            /*
             * Not a valid start character; ignore it.
             */
#ifdef TKANIM_DEBUG
    fprintf(stderr, "?(%c)", buf[0]);
    prevpos = ftell(f);
#endif
            continue;
        }

        if (fread(buf, 1, 9, f) != 9) {
            Tcl_AppendResult( interp,
                "couldn't read left/top/width/height in GIF image", NULL );
/*
            interp->result = "couldn't read left/top/width/height in GIF image";
*/
            goto error;
        }

        useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

        bitPixel = 1<<((buf[8]&0x07)+1);

        imageLeftPos= LM_to_uint(buf[0], buf[1]);
        imageTopPos=  LM_to_uint(buf[2], buf[3]);
        imageWidth=   LM_to_uint(buf[4], buf[5]); 
        imageHeight=  LM_to_uint(buf[6], buf[7]);

        block.width = imageWidth;
        block.height = imageHeight;
        block.pixelSize = 3;
        block.pitch = 3 * imageWidth;
        block.offset[0] = 0;
        block.offset[1] = 1;
        block.offset[2] = 2;
        block.offset[3] = 3;
        nBytes = imageHeight * block.pitch;
        block.pixelPtr = (unsigned char *) ckalloc((unsigned) nBytes);

        sprintf(widthbuf, "%d", imageWidth);
        sprintf(heightbuf, "%d", imageHeight);

        /* save result */

        {
#if (TK_MAJOR_VERSION >= 8 && TK_MINOR_VERSION >= 1)
          Tcl_Obj *argv[7];
          int i;

          argv[0] = Tcl_NewStringObj("image", -1);
          argv[1] = Tcl_NewStringObj("create", -1);
          argv[2] = Tcl_NewStringObj("photo", -1);
          argv[3] = Tcl_NewStringObj("-width", -1);
          argv[4] = Tcl_NewStringObj(widthbuf, -1);
          argv[5] = Tcl_NewStringObj("-height", -1);
          argv[6] = Tcl_NewStringObj(heightbuf, -1);
      
          for(i=0; i<7; i++){ Tcl_IncrRefCount(argv[i]); }

          if( Tk_ImageObjCmd((ClientData) winPtr, interp, 
                        /* "image create photo -width <imageWidth> 
                           -height <imageHeight>" */
                             7, argv) == TCL_ERROR ){
            return TCL_ERROR;
          }
        
        for(i=0; i<7; i++){ Tcl_DecrRefCount(argv[i]); }

#else
        char *argv[7] = {"image", "create", "photo", "-width", NULL,
                         "-height", NULL};
        argv[4] = widthbuf;
        argv[6] = heightbuf;
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\t\timage creation (%s %s %s %s %s %s %s)", 
            argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
#endif
        if( Tk_ImageCmd((ClientData) winPtr, interp, 
                        /* "image create photo -width <imageWidth> 
                           -height <imageHeight>" */
                        7, argv) == TCL_ERROR ){
            return TCL_ERROR;
        }
#endif

#ifdef TKANIM_DEBUG
    fprintf(stderr, " done ");
#endif
        }

        imageName = interp->result;
#if (TK_MAJOR_VERSION < 8)
        photoHandle = Tk_FindPhoto(interp->result);
#else
        photoHandle = Tk_FindPhoto(interp, interp->result);
#endif
        if (!useGlobalColormap) {
            if (!ReadColorMap(f, bitPixel, localColorMap)) {
                    Tcl_AppendResult(interp, "error reading color map", 
                            (char *) NULL);
                    goto error;
            }
            if (ReadImage(interp, (char *) block.pixelPtr, f, imageWidth,
                    imageHeight, localColorMap, BitSet(buf[8], INTERLACE),
                    transparent) != TCL_OK) {
                goto error;
            }
        } else {
            if (ReadImage(interp, (char *) block.pixelPtr, f, imageWidth,
                    imageHeight, colorMap, BitSet(buf[8], INTERLACE),
                    transparent) != TCL_OK) {
                goto error;
            }
        }
        Tk_PhotoPutBlock(
#if (TK_MAJOR_VERSION == 8 && TK_MINOR_VERSION >= 5 || TK_MAJOR_VERSION > 8)
	NULL,
#endif
photoHandle, &block, 0, 0, imageWidth, imageHeight
#if (TK_MAJOR_VERSION == 8 && TK_MINOR_VERSION >= 4 || TK_MAJOR_VERSION > 8)
                   , TK_PHOTO_COMPOSITE_SET
#endif
          );
#ifdef TKANIM_DEBUG
    fprintf(stderr, " Retrieving result\n");
#endif
        /* retrieve result */
        sprintf(newresbuf, "{%s %d %d %d %d %d} ", 
                imageName, imageWidth, imageHeight, imageLeftPos, imageTopPos, 
                delay); 
#ifdef TKANIM_DEBUG
    fprintf(stderr, " newresbuf = %s\n", newresbuf);
#endif
        ckfree((char *) block.pixelPtr);
#ifdef TKANIM_DEBUG
    fprintf(stderr, " free done (now append result)");
#endif
        Tcl_DStringAppend( &resultbuf, newresbuf, -1 );
#ifdef TKANIM_DEBUG
        fprintf(stderr, "\n\t\tFrame done (%d)", ftell(f) - prevpos);
        prevpos = ftell(f);
#endif
    }
    sprintf( widthbuf, "%d", loop );
    Tcl_DStringAppend( &resultbuf, "} ", -1 );
    resultptr = Tcl_DStringAppend( &resultbuf, widthbuf, -1 );
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\nResult = %s\n", resultptr);
#endif
    Tcl_ResetResult(interp);
    Tcl_AppendResult(interp, resultptr, NULL);
    Tcl_DStringFree(&resultbuf);
    return TCL_OK;

    error:
    Tcl_DStringFree(&resultbuf);
    ckfree((char *) block.pixelPtr);
    return TCL_ERROR;

}

static int
DoExtension(fd, label, transparent, delay, loop)
FILE    *fd;
int label;
int     *transparent;
int     *delay;
int     *loop;
{
        static unsigned char buf[256];
        int count = 0;

        switch (label) {
                case 0x01:      /* Plain Text Extension */
                        break;

                case 0xff:      /* Application Extension */
                        count = GetDataBlock(fd, (unsigned char*) buf);
                        if( count < 0){
                            return 1;
                        }
                        if( !strncmp (buf, "NETSCAPE", 8) ) {
                            /* we ignore check of "2.0" */
                            count = GetDataBlock (fd, (unsigned char*) buf);
                            if( count < 0){
                                return 1;
                            }
                            if( buf[0] != 1 ){
                                fprintf(stderr, "??? %d", buf[0]);
                            }
                            *loop = LM_to_uint(buf[1], buf[2]);
                        }
                        do {
                                count = GetDataBlock(fd, (unsigned char*) buf);
                        } while (count > 0);
                        return count;
                        break;

                case 0xfe:      /* Comment Extension */
                        do {
                                count = GetDataBlock(fd, (unsigned char*) buf);
                        } while (count > 0);
                        return count;

                case 0xf9:      /* Graphic Control Extension */
                        count = GetDataBlock(fd, (unsigned char*) buf);
                        if (count < 0) {
                                return 1;
                        }
                        if ((buf[0] & 0x1) != 0) {
                                *transparent = buf[3];
                        }

                        /* Delay time */
                        *delay = LM_to_uint(buf[1],buf[2]);

                        do {
                            count = GetDataBlock(fd, (unsigned char*) buf);
                        } while (count > 0);
                        return count;
        }

        do {
            count = GetDataBlock(fd, (unsigned char*) buf);
        } while (count > 0);
        return count;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadGIFHeader --
 *
 *      This procedure reads the GIF header from the beginning of a
 *      GIF file and returns the dimensions of the image.
 *
 * Results:
 *      The return value is 1 if file "f" appears to start with
 *      a valid GIF header, 0 otherwise.  If the header is valid,
 *      then *widthPtr and *heightPtr are modified to hold the
 *      dimensions of the image.
 *
 * Side effects:
 *      The access position in f advances.
 *
 *----------------------------------------------------------------------
 */

static int
ReadGIFHeader(f, widthPtr, heightPtr)
    FILE *f;                    /* Image file to read the header from */
    int *widthPtr, *heightPtr;  /* The dimensions of the image are
                                 * returned here. */
{
    unsigned char buf[7];

    if ((fread(buf, 1, 6, f) != 6)
            || ((strncmp("GIF87a", (char *) buf, 6) != 0)
            && (strncmp("GIF89a", (char *) buf, 6) != 0))) {
        return 0;
    }

    if (fread(buf, 1, 4, f) != 4) {
        return 0;
    }

    *widthPtr = LM_to_uint(buf[0],buf[1]);
    *heightPtr = LM_to_uint(buf[2],buf[3]);
    return 1;
}

/*
 *-----------------------------------------------------------------
 * The code below is copied from the giftoppm program and modified
 * just slightly.
 *-----------------------------------------------------------------
 */

static int
ReadColorMap(fd,number,buffer)
FILE        *fd;
int     number;
unsigned char   buffer[3][MAXCOLORMAPSIZE];
{
        int     i;
        unsigned char   rgb[3];

        for (i = 0; i < number; ++i) {
                if (! ReadOK(fd, rgb, sizeof(rgb)))
                        return 0;

                buffer[CM_RED][i] = rgb[0] ;
                buffer[CM_GREEN][i] = rgb[1] ;
                buffer[CM_BLUE][i] = rgb[2] ;
        }
        return 1;
}



static int ZeroDataBlock = 0;

static int
GetDataBlock(fd, buf)
FILE        *fd;
unsigned char   *buf;
{
        unsigned char   count;

        if (! ReadOK(fd,&count,1)) {
                return -1;
        }

        ZeroDataBlock = count == 0;

        if ((count != 0) && (! ReadOK(fd, buf, count))) {
                return -1;
        }

        return count;
}


static int
ReadImage(interp, imagePtr, fd, len, height, cmap, interlace, transparent)
Tcl_Interp *interp;
char    *imagePtr;
FILE    *fd;
int len, height;
unsigned char   cmap[3][MAXCOLORMAPSIZE];
int interlace;
int transparent;
{
        unsigned char   c;
        int     v;
        int     xpos = 0, ypos = 0, pass = 0;
        char    *colStr;


        /*
         *  Initialize the Compression routines
         */
        if (! ReadOK(fd,&c,1))  {
            Tcl_AppendResult(interp, "error reading GIF image: ",
                    Tcl_PosixError(interp), (char *) NULL);
            return TCL_ERROR;
        }

        if (LWZReadByte(fd, 1, c) < 0) {
            interp->result = "format error in GIF image";
            return TCL_ERROR;
        }

        if (transparent!=-1 && 
                (colStr = Tcl_GetVar(interp, "TRANSPARENT_GIF_COLOR", 0L))) {
                XColor *colorPtr;
                colorPtr = Tk_GetColor(interp, Tk_MainWindow(interp), 
                                                          Tk_GetUid(colStr));
                if (colorPtr) {
/*
                        printf("color is %d %d %d\n", 
                                        colorPtr->red >> 8, 
                                        colorPtr->green >> 8, 
                                        colorPtr->blue >> 8);
*/
                        cmap[CM_RED][transparent] = colorPtr->red >> 8;
                        cmap[CM_GREEN][transparent] = colorPtr->green >> 8;
                        cmap[CM_BLUE][transparent] = colorPtr->blue >> 8;
                        Tk_FreeColor(colorPtr);
                }
        }

        while ((v = LWZReadByte(fd,0,c)) >= 0 ) {

                imagePtr[ (xpos*3)  +  (ypos *len*3)] = cmap[CM_RED][v];
                imagePtr[ (xpos*3)  +  (ypos *len*3) +1] = cmap[CM_GREEN][v];
                imagePtr[ (xpos*3)  +  (ypos *len*3) +2] = cmap[CM_BLUE][v];

                ++xpos;
                if (xpos == len) {
                        xpos = 0;
                        if (interlace) {
                                switch (pass) {
                                        case 0:
                                        case 1:
                                                ypos += 8; break;
                                        case 2:
                                                ypos += 4; break;
                                        case 3:
                                                ypos += 2; break;
                                }

                                if (ypos >= height) {
                                        ++pass;
                                        switch (pass) {
                                                case 1:
                                                        ypos = 4; break;
                                                case 2:
                                                        ypos = 2; break;
                                                case 3:
                                                        ypos = 1; break;
                                                default:
                                                        return TCL_OK;
                                        }
                                }
                        } else {
                                ++ypos;
                        }
                }
                if (ypos >= height)
                        break;
        }
        return TCL_OK;
}

static int
LWZReadByte(fd, flag, input_code_size)
FILE    *fd;
int flag;
int input_code_size;
{
        static int  fresh = 0;
        int     code, incode;
        static int  code_size, set_code_size;
        static int  max_code, max_code_size;
        static int  firstcode, oldcode;
        static int  clear_code, end_code;
        static int  table[2][(1<< MAX_LWZ_BITS)];
        static int  stack[(1<<(MAX_LWZ_BITS))*2], *sp;
        register int    i;


        if (flag) {

                set_code_size = input_code_size;
                code_size = set_code_size+1;
                clear_code = 1 << set_code_size ;
                end_code = clear_code + 1;
                max_code_size = 2*clear_code;
                max_code = clear_code+2;

                GetCode(fd, 0, 1);

                fresh = 1;

                for (i = 0; i < clear_code; ++i) {
                        table[0][i] = 0;
                        table[1][i] = i;
                }
                for (; i < (1<<MAX_LWZ_BITS); ++i) {
                        table[0][i] = table[1][0] = 0;
                }

                sp = stack;

                return 0;

        } else if (fresh) {

                fresh = 0;
                do {
                        firstcode = oldcode = GetCode(fd, code_size, 0);
                } while (firstcode == clear_code);
                return firstcode;
        }

        if (sp > stack)
                return *--sp;

        while ((code = GetCode(fd, code_size, 0)) >= 0) {
                if (code == clear_code) {
                        for (i = 0; i < clear_code; ++i) {
                                table[0][i] = 0;
                                table[1][i] = i;
                        }

                        for (; i < (1<<MAX_LWZ_BITS); ++i) {
                                table[0][i] = table[1][i] = 0;
                        }

                        code_size = set_code_size+1;
                        max_code_size = 2*clear_code;
                        max_code = clear_code+2;
                        sp = stack;
                        firstcode = oldcode = GetCode(fd, code_size, 0);
                        return firstcode;

        } else if (code == end_code) {
                int     count;
                unsigned char   buf[260];

                if (ZeroDataBlock)
                        return -2;

                while ((count = GetDataBlock(fd, buf)) > 0)
                        ;

                if (count != 0)
                        return -2;
        }

        incode = code;

        if (code >= max_code) {
                *sp++ = firstcode;
                code = oldcode;
        }

        while (code >= clear_code) {
                *sp++ = table[1][code];
                if (code == table[0][code]) {
                        return -2;

                        fprintf(stderr, "circular table entry BIG ERROR\n");
                        /*
                         * Used to be this instead, Steve Ball suggested
                         * the change to just return.

                        printf("circular table entry BIG ERROR\n");
                        */
                }
                code = table[0][code];
        }

        *sp++ = firstcode = table[1][code];

        if ((code = max_code) <(1<<MAX_LWZ_BITS)) {

                table[0][code] = oldcode;
                table[1][code] = firstcode;
                ++max_code;
                if ((max_code>=max_code_size) && (max_code_size < (1<<MAX_LWZ_BITS))) {
                        max_code_size *= 2;
                        ++code_size;
                }
        }

        oldcode = incode;

        if (sp > stack)
                return *--sp;
        }
        return code;
}


static int
GetCode(fd, code_size, flag)
FILE    *fd;
int code_size;
int flag;
{
        static unsigned char    buf[280];
        static int      curbit, lastbit, done, last_byte;
        int         i, j, ret;
        unsigned char       count;

        if (flag) {
                curbit = 0;
                lastbit = 0;
                done = 0;
                return 0;
        }


        if ( (curbit+code_size) >= lastbit) {
                if (done) {
                        /* ran off the end of my bits */
                        return -1;
                }
                buf[0] = buf[last_byte-2];
                buf[1] = buf[last_byte-1];

                if ((count = GetDataBlock(fd, &buf[2])) == 0)
                        done = 1;

                last_byte = 2 + count;
                curbit = (curbit - lastbit) + 16;
                lastbit = (2+count)*8 ;
        }

        ret = 0;
        for (i = curbit, j = 0; j < code_size; ++i, ++j)
                ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;


        curbit += code_size;

        return ret;
}

int Tk_AnimationCmd(clientData, interp, argc, argv)
    ClientData clientData;      /* Main window associated with interpreter. */
    Tcl_Interp *interp;         /* Current interpreter. */
    int argc;                   /* Number of arguments. */
    char **argv;                /* Argument strings. */
{
    char c;
    int length;

    if (argc < 2) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
                " option ?arg arg ...?\"", (char *) NULL);
        return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if((c == 'c') && (length >= 2) 
       && (strncmp(argv[1], "create", length) == 0)) {

        char * realFileName;
        Tcl_DString buffer;
        FILE *f;

#ifdef TKANIM_DEBUG
    fprintf(stderr, "AnimationCmd => create ");
#endif

        if ( argc != 3 ){
            Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
                             " create GifFile\"", (char *) NULL);
            return TCL_ERROR;
        }
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\tRealFileName = ");
#endif
        realFileName = Tcl_TranslateFileName(interp, argv[2],
                &buffer);
        if(realFileName == NULL) {
            Tcl_DStringFree(&buffer);
            return TCL_ERROR;
        }
#ifdef TKANIM_DEBUG
    fprintf(stderr, "%s ", realFileName);
#endif
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\tOpen ", realFileName);
#endif
        f = fopen(realFileName, "rb");
        Tcl_DStringFree(&buffer);
        if (f == NULL ){
            Tcl_AppendResult(interp, "couldn't read image file \"",
                    argv[2], "\": ", Tcl_PosixError(interp),
                    (char *) NULL);
            return TCL_ERROR;
        }
#ifdef TKANIM_DEBUG
    fprintf(stderr, "success ", realFileName);
#endif
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\tRead ", realFileName);
#endif
        if( FileReadGIF(interp, f, argv[2], "gif") != TCL_OK ){
#ifdef TKANIM_DEBUG
            fprintf(stderr, "\n\tRead failed", realFileName);
#endif
            return TCL_ERROR;
        }
        fclose(f);
#ifdef TKANIM_DEBUG
    fprintf(stderr, "\n\tRead done", realFileName);
#endif
#ifdef TKANIM_DEBUG
        fprintf(stderr, "done\n");
#endif
    }
    return TCL_OK;
}

void
TkDeleteTkAnim(clientData)
    ClientData clientData;
{
#ifdef TKANIM_DEBUG
    fprintf(stderr, "TkDeleteTkAnim\n");
#endif
}

int Tkanim_Init(interp)
    Tcl_Interp *interp;
{
#ifdef TKANIM_DEBUG
    fprintf(stderr, "Tkanim initialize...");
#endif
    Tcl_CreateCommand(interp, "animation", Tk_AnimationCmd, 
                      (ClientData) NULL,
                      (Tcl_CmdDeleteProc *) TkDeleteTkAnim);
#ifdef TKANIM_DEBUG
    fprintf(stderr, "done\n");
#endif
    return Tcl_PkgProvide(interp, "Tkanim", TKANIM_VERSION );
}
