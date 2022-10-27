/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                David Allsopp, MetaStack Solutions Ltd.                 */
/*                                                                        */
/*   Copyright 2015 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operating system - Windows specific stuff */

#ifndef CAML_WINSUPPORT_H
#define CAML_WINSUPPORT_H

#if defined(_WIN32) && defined(CAML_INTERNALS)

#include <windef.h>

/*
 * This structure is defined inconsistently. mingw64 has it in ntdef.h (which
 * doesn't look like a primary header) and technically it's part of ntifs.h in
 * the WDK. Requiring the WDK is a bit extreme, so the definition is taken from
 * ntdef.h. Both ntdef.h and ntifs.h define REPARSE_DATA_BUFFER_HEADER_SIZE
 */
#ifndef REPARSE_DATA_BUFFER_HEADER_SIZE
typedef struct _REPARSE_DATA_BUFFER
{
  ULONG  ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union
  {
    struct
    {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG  Flags;
      WCHAR  PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct
    {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR  PathBuffer[1];
    } MountPointReparseBuffer;
    struct
    {
      UCHAR  DataBuffer[1];
    } GenericReparseBuffer;
  };
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;
#endif

typedef union {
  FILETIME ft;
  ULONGLONG ul;
} CAML_ULONGLONG_FILETIME;

#endif

#endif /* CAML_WINSUPPORT_H */
