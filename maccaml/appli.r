/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */


#include "Types.r"
#include "Sound.r"

#include "ocamlconstants.h"


/* These 5 resources are meant to be overridden. */

data 'Line' (1000) { "%a\000" };  /* command line template */

data 'Line' (1001) { "" };        /* environment template */

data 'TEXT' (1000, purgeable) {  /* kAboutText1 */
  "\n"
  APPLNAME "\n"
  "\n"
  "\n"
  "\n"
  "\n"
  "\n"
};

data 'cicn' (1000) {  /* kApplicationIcon */
    $"0000 0000 8020 0000 0000 0020 0020 0000 0000 0000 0000 0048"
    $"0000 0048 0000 0000 0008 0001 0008 0000 0000 0000 0000 0000"
    $"0000 0000 0000 0004 0000 0000 0020 0020 0000 0000 0004 0000"
    $"0000 0020 0020 0000 0000 0000 0000 001F F01C 003F F83E 007F"
    $"FC7E 00FF FEFC 01FF FFF8 03FF FFF0 07FF FFE0 0FFF FFE0 1FFF"
    $"FFF0 3FFF FFF8 7FFF FFFC FFFF FFFF FFFF FFFF FFFF FFFF 7FFF"
    $"FFFF 3FFF FFFF 1FFF FFFF 0FFF FFFF 07FF FFFF 03FF FFFF 01FF"
    $"FFFE 00FF FFFF 007F FFFF 003F FFFF 001F FFFE 000F FFFC 0007"
    $"FFF8 0003 FFF0 0001 FFE0 0000 FF80 0000 7F00 0000 0000 001F"
    $"F01C 0030 3826 0048 7C5E 0084 E69C 0103 C338 0200 9E70 0400"
    $"24E0 0800 49E0 1000 9330 2001 2618 4002 4E0C 8002 9A07 8001"
    $"7C07 C002 8007 6002 E007 3007 FC07 1806 1FC7 0C00 01FF 0600"
    $"003F 0300 0007 0180 000E 00C0 001F 0060 003F 0030 007F 0018"
    $"00FE 000C 01FC 0006 03F8 0003 07F0 0001 8FE0 0000 DF80 0000"
    $"7F00 0000 0000 0000 002F 0000 FFFF FFFF FFFF 0001 FFFF FFFF"
    $"6666 0002 FFFF CCCC CCCC 0003 FFFF CCCC 9999 0004 FFFF CCCC"
    $"6666 0005 FFFF 9999 9999 0006 FFFF 0000 3333 0007 CCCC CCCC"
    $"CCCC 0008 CCCC CCCC 9999 0009 CCCC CCCC 6666 000A CCCC 9999"
    $"9999 000B CCCC 9999 6666 000C CCCC 9999 3333 000D CCCC 6666"
    $"6666 000E CCCC 6666 3333 000F 9999 9999 9999 0010 9999 9999"
    $"6666 0011 9999 9999 3333 0012 9999 6666 6666 0013 9999 6666"
    $"3333 0014 9999 3333 6666 0015 9999 3333 3333 0016 9999 0000"
    $"3333 0017 9999 0000 0000 0018 6666 6666 6666 0019 6666 6666"
    $"3333 001A 6666 3333 6666 001B 6666 3333 3333 001C 6666 3333"
    $"0000 001D 6666 0000 3333 001E 3333 3333 0000 001F 3333 0000"
    $"3333 0020 3333 0000 0000 0021 0000 0000 3333 0022 8888 0000"
    $"0000 0023 4444 0000 0000 0024 1111 0000 0000 0025 0000 1111"
    $"0000 0026 EEEE EEEE EEEE 0027 DDDD DDDD DDDD 0028 BBBB BBBB"
    $"BBBB 0029 AAAA AAAA AAAA 002A 8888 8888 8888 002B 7777 7777"
    $"7777 002C 5555 5555 5555 002D 4444 4444 4444 002E 2222 2222"
    $"2222 002F 1111 1111 1111 0000 0000 0000 0000 0000 0000 0000"
    $"0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000"
    $"0000 0000 0029 0F29 0F29 0F29 0F18 0000 0000 0000 0022 062D"
    $"0000 0000 0000 0000 0000 0000 2A2B 2600 0000 0027 2C2A 2D00"
    $"0000 0000 1606 1622 2D00 0000 0000 0000 0000 002A 2700 2B26"
    $"0000 072D 2928 2A2D 0000 002C 0717 171F 2D00 0000 0000 0000"
    $"0000 2A07 0000 262B 2607 2C29 2827 282A 2D00 2C01 0F2B 2E2D"
    $"0000 0000 0000 0000 002A 2700 0000 0026 2B2C 2928 0707 030A"
    $"0F11 0410 1B23 2C00 0000 0000 0000 0000 2A27 0000 0000 0000"
    $"2628 0702 0803 0A0A 1104 0C1B 1E2D 0000 0000 0000 0000 002A"
    $"2700 0000 0000 0000 2627 080A 0503 0F13 040B 1B1E 1A00 0000"
    $"0000 0000 0000 2A07 0000 0000 0000 0000 2708 0A03 1015 1304"
    $"0B1C 1E0A 2C00 0000 0000 0000 002A 2700 0000 0000 0000 0026"
    $"0A05 0312 1B13 040B 1C1F 0E03 0A18 0000 0000 0000 2A27 0000"
    $"0000 0000 0000 0028 0403 0E1B 1304 0C1B 1E1B 1203 030B 2C00"
    $"0000 002A 2700 0000 0000 0000 0000 2705 0312 1B13 040B 1B24"
    $"1B15 1310 0303 0A2C 0000 2A27 0000 0000 0000 0000 0000 0703"
    $"141C 1B09 0C1B 252F 1513 0D0B 0303 090A 182A 2C07 0000 0000"
    $"0000 0000 0026 1215 1B0B 0B0D 0A0B 0303 0303 0303 0303 050B"
    $"2B12 2D2C 2800 0000 0000 0000 0026 261C 1B05 0A12 120B 0B03"
    $"0303 0303 030A 0B0A 2B11 002D 2C07 0000 0000 0000 0000 0711"
    $"0B1B 151E 1B1B 1519 120B 0A0B 0B0B 1212 2A29 0000 2D2D 0700"
    $"0000 0000 0027 120B 1319 2B12 1B1D 1E1B 1B1B 1B12 2C15 1213"
    $"0F29 0000 002D 2C07 0000 0000 0008 2C2C 2A0A 2726 2729 1223"
    $"2320 1E1D 1C2D 1B1B 2A29 0000 0000 2D2D 2800 0000 272C 0A28"
    $"2727 2727 2727 0728 2912 1219 2C1B 1B1B 2B29 0000 0000 002D"
    $"2C28 0000 0721 2D2C 182A 2928 2807 0707 0707 0707 0707 0F2B"
    $"2B29 0000 0000 0000 2D2C 2800 0026 2707 2829 290F 0F0F 2928"
    $"2828 2828 2828 2A2E 2D2D 0000 0000 0000 002D 2C28 0000 0026"
    $"2727 2727 2707 2829 0F0F 0F0F 0F29 2E2D 2D00 0000 0000 0000"
    $"0000 2D2C 2800 0000 0000 0000 2727 2707 0707 0707 282F 2D2D"
    $"2D2D 0000 0000 0000 0000 002D 2C28 0000 0000 0000 0000 0000"
    $"2727 2728 2F2C 2D2D 2D2D 0000 0000 0000 0000 0000 2D2C 2800"
    $"0000 0000 0000 0000 0000 072F 2D2D 2D2D 2D2D 0000 0000 0000"
    $"0000 0000 002D 2C28 0000 0000 0000 0000 0007 2E2D 2D2D 2D2D"
    $"2D00 0000 0000 0000 0000 0000 0000 2D2C 2800 0000 0000 0000"
    $"072E 2D2D 2D2D 2D2D 0000 0000 0000 0000 0000 0000 0000 002D"
    $"2C28 0000 0000 0007 2E2D 2D2D 2D2D 2D00 0000 0000 0000 0000"
    $"0000 0000 0000 0000 2D2C 2900 0000 072E 2D2D 2D2D 2D2D 0000"
    $"0000 0000 0000 0000 0000 0000 0000 0000 002D 2C28 2607 2F2D"
    $"2D2D 2D2D 2D00 0000 0000 0000 0000 0000 0000 0000 0000 0000"
    $"0000 2D2D 282E 2D2D 2D2D 2D00 0000 0000 0000 0000 0000 0000"
    $"0000 0000 0000 0000 0000 002D 2E2D 2D2D 2D2D 0000 0000 0000"
    $"0000"
};

data 'ICON' (1000) {  /* kApplicationIcon */
    $"0000 0000 001F F01C 0030 3826 0048 7C5E 0084 E69C 0103 C338"
    $"0200 9E70 0400 24E0 0800 49E0 1000 9330 2001 2618 4002 4E0C"
    $"8002 9A07 8001 7C07 C002 8007 6002 E007 3007 FC07 1806 1FC7"
    $"0C00 01FF 0600 003F 0300 0007 0180 000E 00C0 001F 0060 003F"
    $"0030 007F 0018 00FE 000C 01FC 0006 03F8 0003 07F0 0001 8FE0"
    $"0000 DF80 0000 7F00"
};


/* The other resources should not need to be changed. */

data 'TEXT' (kAboutText2, purgeable) {
  "Includes (parts of) Objective Caml, MPW libraries,"
             "and the WASTE text engine.\n"
  "\n"
  "Objective Caml Copyright 1991-2001 INRIA, all rights reserved.\n"
  "MPW © 1983-2001 by Apple Computer, Inc., all rights reserved\n"
  "WASTE text engine © 1993-1998 Marco Piovanelli\n"
};

resource 'SIZE' (-1) {
    reserved,
    acceptSuspendResumeEvents,
    reserved,
    canBackground,
    doesActivateOnFGSwitch,
    backgroundAndForeground,
    dontGetFrontClicks,
    ignoreChildDiedEvents,
    is32BitCompatible,
    isHighLevelEventAware,
    localAndRemoteHLEvents,
    isStationeryAware,
    dontuseTextEditServices,
    reserved,
    reserved,
    reserved,
    PREFSIZE * 1024,
    MINSIZE * 1024
};

type 'Kequ' {
  wide array KequArray {
    byte any = 0   command = 1;
    byte char;
    byte item;
    fill byte;
  };
};

resource 'Kequ' (kKeysOK) {
  {
    any, charReturn, 1,
    any, charEnter, 1,
    any, 'o', 1,
    any, 'O', 1,
  }
};

resource 'Kequ' (kKeysSaveDontCancel) {
  {
    any, charReturn, 1,
    any, charEnter, 1,
    any, 'y', 1,
    any, 'Y', 1,
    any, 's', 1,
    any, 'S', 1,

    any, charEscape, 2,
    command, '.', 2,
    any, 'c', 2,
    any, 'C', 2,

    any, 'n', 3,
    any, 'N', 3,
    any, 'd', 3,
    any, 'D', 3,
  }
};

resource 'ALRT' (kAlertBug) {
    {60, 61, 260, 451}, kAlertBug,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertBug) {
    {
        {160, 310, 180, 368}, Button {enabled, "Quit"},

        {10, 70, 80, 368},
        StaticText {disabled,  /* Don't change this occurrence of Obj Caml */
          "You have discovered a bug in Objective Caml.  Please"
          " report the following information to <caml-bugs@inria.fr>."
        },

        {80, 20, 145, 368},
        StaticText {disabled, "file: ^1\nline: ^2\nexpr: ^0"},
    }
};

resource 'ALRT' (kAlertNotYet) {
    {60, 81, 160, 431}, kAlertNotYet,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertNotYet) {
    {
        {60, 270, 80, 328}, Button {enabled, "OK"},

        {10, 70, 45, 328},
        StaticText {disabled, "This feature is not yet implemented." },
    }
};

resource 'ALRT' (kAlertNeedSys7) {
    {60, 81, 200, 431}, kAlertNeedSys7,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionMainScreen
};

resource 'DITL' (kAlertNeedSys7) {
    {
        {100, 270, 120, 328},
        Button {enabled, "Quit"},

        {10, 70, 85, 328},
        StaticText {
            disabled,
            APPLNAME " cannot run on MacOS versions prior to System 7."
        },

        {10, 20, 42, 52}, Icon {disabled, kApplicationIcon},
    }
};

resource 'ALRT' (kAlertNeed32BitQD) {
    {60, 81, 200, 431}, kAlertNeed32BitQD,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionMainScreen
};

resource 'DITL' (kAlertNeed32BitQD) {
    {
        {100, 270, 120, 328},
        Button {enabled, "Quit"},

        {10, 70, 85, 328},
        StaticText {
            disabled,
            APPLNAME " needs a Macintosh with 32-bit color QuickDraw."
        },

        {10, 20, 42, 52}, Icon {disabled, kApplicationIcon},
    }
};

resource 'ALRT' (kAlertExit) {
    {60, 81, 210, 431}, kAlertExit,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertExit) {
    {
        {110, 270, 130, 328}, Button {enabled, "OK"},

        {10, 70, 95, 328},
        StaticText {
          disabled,
          "The " APPLNAME " toplevel loop has terminated^0^1.\n\n"
          "Any further input in the toplevel window will be ignored."
        },
    }
};

resource 'ALRT' (kAlertErrorMsg) {
    {60, 81, 200, 431}, kAlertErrorMsg,
    {
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertErrorMsg) {
    {
        {100, 270, 120, 328}, Button {enabled, "OK"},
        {10, 70, 85, 328}, StaticText { disabled, "^0^1^2^3" },
    }
};

resource 'ALRT' (kAlertErrorNum) {
    {60, 81, 200, 431}, kAlertErrorNum,
    {
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertErrorNum) {
    {
        {100, 270, 120, 328}, Button {enabled, "OK"},

        {10, 70, 85, 328},
        StaticText { disabled, "An error occurred.\n\nerror code = ^3" },
    }
};

resource 'ALRT' (kAlertGeneric) {
    {60, 81, 200, 431}, kAlertGeneric,
    {
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
        OK, visible, sound1,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertGeneric) {
    {
        {100, 270, 120, 328}, Button {enabled, "OK"},

        {10, 20, 85, 378},
        StaticText { disabled, "^0^1^2^3" },
    }
};

resource 'ALRT' (kAlertSaveAsk) {
    {60, 81, 200, 431}, kAlertSaveAsk,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertSaveAsk) {
    {
        {100, 270, 120, 328}, Button {enabled, "Save"},
        {100, 202, 120, 260}, Button {enabled, "Cancel"},
        {100,  22, 120, 110}, Button {enabled, "Don't Save"},
        {10, 70, 85, 328}, StaticText { disabled, "Save \"^0\" before ^1 ?" },
        {10, 20, 42, 52}, Icon {disabled, kApplicationIcon},
    }
};

resource 'DLOG' (kDialogAbout) {
    {70, 60, 285, 470},
    noGrowDocProc,
    visible,
    goAway,
    0,
    kDialogAbout,
    "About " APPLNAME,
    alertPositionMainScreen
};

resource 'DITL' (kDialogAbout) {
    {
        {10, 20, 42, 52}, Icon {disabled, kApplicationIcon},
        {10, 72, 205, 400}, UserItem { disabled },
    }
};

resource 'MBAR' (kMenuBar) {
    { kMenuApple, kMenuFile, kMenuEdit, kMenuWindows, }
};

resource 'MENU' (kMenuApple) {
    kMenuApple,
    textMenuProc,
    0x7FFFFFFD,
    enabled,
    apple,
    {
        "About " APPLNAME "…", noIcon, noKey, noMark, plain,
        "-", noIcon, noKey, noMark, plain,
    }
};

resource 'MENU' (kMenuFile) {
    kMenuFile,
    textMenuProc,
    0x7FFFFB7B,
    enabled,
    "File",
    {
        "New", noIcon, "N", noMark, plain,
        "Open…", noIcon, "O", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Close", noIcon, "W", noMark, plain,
        "Save", noIcon, "S", noMark, plain,
        "Save as…", noIcon, noKey, noMark, plain,
        "Revert to Saved", noIcon, noKey, noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Page Setup…", noIcon, nokey, noMark, plain,
        "Print…", noIcon, "P", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Quit", noIcon, "Q", noMark, plain,
    }
};

resource 'MENU' (kMenuEdit) {
    kMenuEdit,
    textMenuProc,
    0x7FFFFFBD,
    enabled,
    "Edit",
    {
        "Undo", noIcon, "Z", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Cut", noIcon, "X", noMark, plain,
        "Copy", noIcon, "C", noMark, plain,
        "Paste", noIcon, "V", noMark, plain,
        "Clear", noIcon, noKey, noMark, plain,
        "Select All", noIcon, "A", noMark, plain,
        "Show Clipboard", noIcon, noKey, noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Find…", noIcon, "F", noMark, plain,
        "Replace…", noIcon, "R", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Preferences…", noIcon, noKey, noMark, plain,
    }
};

resource 'MENU' (kMenuWindows) {
    kMenuWindows,
    textMenuProc,
    0x7FFFFFF9,
    enabled,
    "Windows",
    {
        "Toplevel", noIcon, "T", noMark, plain,
        "Graphics", noIcon, "G", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
    }
};

resource 'STR#' (kUndoStrings) {
  {
    "Cannot undo",
    "Undo", "Redo",
    "Undo Typing", "Redo Typing",
    "Undo Cut", "Redo Cut",
    "Undo Paste", "Redo Paste",
    "Undo Clear", "Redo Clear",
    "Undo Drag & Drop", "Redo Drag & Drop",
    /* Style change is not supported. */
  }
};

resource 'STR#' (kMiscStrings, purgeable) {
  {
    APPLNAME " Preferences",
    "Untitled",
    "closing",
    "quitting",
    "Unable to open \"",
    "\".  ",
    "Save file as:",
    "",
    "Unable to write to \"",
    " with error code ",
  }
};

resource 'STR#' (kErrorStrings, purgeable) {
  {
    "There is not enough memory.",
    "The disk is full.",
    "The directory is full.",
    "Too many files are already open.",
    "The file does not exist.",
    "The disk is write-protected.",
    "The file is locked.",
    "The disk is locked.",
    "The file is in use.",
    "The file is already open (by " APPLNAME " or another application).",
    "The disk was ejected.",
    "The file is locked or you do not have the permission to open it.",
    "You do not have the permission to write to this file.",
    "The folder does not exist.",
    "The connection to the file server was closed or broken.",
    "A hardware error occurred during input or output.",
  }
};

resource 'STR ' (kPrefsDescriptionStr, purgeable) {
  "This document describes user preferences for " APPLNAME ". "
  "You cannot open or print this document. To be "
  "effective, this document must be stored in the Preferences "
  "folder of the System Folder."
};

resource 'WIND' (kToplevelWinTemplate) {
  {40, 4, 342, 512},
  zoomDocProc,
  invisible,
  noGoAway,
  0,
  APPLNAME " Toplevel",
  noAutoCenter
};

resource 'WIND' (kGraphicsWinTemplate) {
  {40, 4, 342, 512},
  zoomDocProc,
  invisible,
  goAway,
  0,
  APPLNAME " Graphics",
  noAutoCenter
};

resource 'WIND' (kDocumentWinTemplate) {
  {45, 10, 342, 512},
  zoomDocProc,
  visible,
  goAway,
  0,
  "Untitled",
  staggerMainScreen
};

resource 'CNTL' (kScrollBarTemplate) {
  {0, 0, 16, 16},
  0,
  invisible,
  0, 0,
  scrollBarProc,
  0,
  ""
};

resource 'acur' (0) {
  {1000, 1001, 1002, 1003, }
};

resource 'CURS' (1000) {
    $"07C0 1F30 3F08 7F04 7F04 FF02 FF02 FFFE"
    $"81FE 81FE 41FC 41FC 21F8 19F0 07C0",
    $"07C0 1FF0 3FF8 7FFC 7FFC FFFE FFFE FFFE"
    $"FFFE FFFE 7FFC 7FFC 3FF8 1FF0 07C0",
    {7, 7}
};

resource 'CURS' (1001) {
    $"07C0 1FF0 3FF8 5FF4 4FE4 87C2 8382 8102"
    $"8382 87C2 4FE4 5FF4 3FF8 1FF0 07C0",
    $"07C0 1FF0 3FF8 7FFC 7FFC FFFE FFFE FFFE"
    $"FFFE FFFE 7FFC 7FFC 3FF8 1FF0 07C0",
    {7, 7}
};

resource 'CURS' (1002) {
    $"07C0 19F0 21F8 41FC 41FC 81FE 81FE FFFE"
    $"FF02 FF02 7F04 7F04 3F08 1F30 07C0",
    $"07C0 1FF0 3FF8 7FFC 7FFC FFFE FFFE FFFE"
    $"FFFE FFFE 7FFC 7FFC 3FF8 1FF0 07C0",
    {7, 7}
};

resource 'CURS' (1003) {
    $"07C0 1830 2008 701C 783C FC7E FEFE FFFE"
    $"FEFE FC7E 783C 701C 2008 1830 07C0",
    $"07C0 1FF0 3FF8 7FFC 7FFC FFFE FFFE FFFE"
    $"FFFE FFFE 7FFC 7FFC 3FF8 1FF0 07C0",
    {7, 7}
};

resource 'snd ' (1002){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    4,
    Rate22K,
    0, 4,
    0,
    60,
    $"FF01FF01"
  }
};

resource 'snd ' (1004){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    4,
    Rate22K,
    0, 4,
    0,
    60,
    $"FF800180"
  }
};

resource 'snd ' (1008){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    8,
    Rate22K,
    0, 8,
    0,
    60,
    $"FFDA8026012680DA"
  }
};

resource 'snd ' (1032){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    32,
    Rate22K,
    0, 32,
    0,
    60,
    $"FFFDF5EADAC7B19980674F3926160B0301030B1626394F678099B1C7DAEAF5FD"
  }
};


resource 'snd ' (1128){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    128,
    Rate22K,
    0, 128,
    0,
    60,
    $"FFFFFEFEFDFBFAF8F5F3F0EDEAE6E2DEDAD5D1CCC7C1BCB6B1ABA59F99938C86"
    $"807A746D67615B554F4A443F39342F2B26221E1A1613100D0B08060503020201"
    $"01010202030506080B0D1013161A1E22262B2F34393F444A4F555B61676D747A"
    $"80868C93999FA5ABB1B6BCC1C7CCD1D5DADEE2E6EAEDF0F3F5F8FAFBFDFEFEFF"
  }
};

resource 'snd ' (1512, "foo"){
  FormatOne{
    { sampledSynth, 0x80 },
  },
  {
    hasData, soundCmd {0x2C},
    noData, ampCmd {127},
    noData, freqDurationCmd {0x4321, 60},
    noData, quietCmd {},
  },
  {
    512,
    Rate22K,
    0, 512,
    0,
    60,
    $"FFFFFFFFFFFFFFFFFEFEFEFEFEFDFDFDFDFCFCFCFBFBFAFAFAF9F9F8F8F7F6F6"
    $"F5F5F4F3F3F2F1F1F0EFEFEEEDECEBEAEAE9E8E7E6E5E4E3E2E1E0DFDEDDDCDB"
    $"DAD9D8D6D5D4D3D2D1CFCECDCCCAC9C8C7C5C4C3C1C0BFBDBCBAB9B8B6B5B3B2"
    $"B1AFAEACABA9A8A6A5A3A2A09F9D9C9A999796949391908E8C8B898886858382"
    $"807E7D7B7A7877757472706F6D6C6A696766646361605E5D5B5A585755545251"
    $"4F4E4D4B4A484746444341403F3D3C3B39383736343332312F2E2D2C2B2A2827"
    $"262524232221201F1E1D1C1B1A1918171616151413121111100F0F0E0D0D0C0B"
    $"0B0A0A0908080707060606050504040403030303020202020201010101010101"
    $"0101010101010101020202020203030303040404050506060607070808090A0A"
    $"0B0B0C0D0D0E0F0F1011111213141516161718191A1B1C1D1E1F202122232425"
    $"2627282A2B2C2D2E2F31323334363738393B3C3D3F404143444647484A4B4D4E"
    $"4F5152545557585A5B5D5E606163646667696A6C6D6F7072747577787A7B7D7E"
    $"808283858688898B8C8E909193949697999A9C9D9FA0A2A3A5A6A8A9ABACAEAF"
    $"B1B2B3B5B6B8B9BABCBDBFC0C1C3C4C5C7C8C9CACCCDCECFD1D2D3D4D5D6D8D9"
    $"DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEAEBECEDEEEFEFF0F1F1F2F3F3F4F5"
    $"F5F6F6F7F8F8F9F9FAFAFAFBFBFCFCFCFDFDFDFDFEFEFEFEFEFFFFFFFFFFFFFF"
  }
};

resource 'FREF' (128) {
    'APPL',
    0,
    ""
};

resource 'FREF' (129) {
    'TEXT',
    1,
    ""
};

resource 'FREF' (130) {
    'sEXT',
    2,
    ""
};

resource 'BNDL' (128) {
    CREATOR,
    0,
    {   /* array TypeArray: 2 elements */
        /* [1] */
        'FREF',
        {   /* array IDArray: 3 elements */
            /* [1] */
            0, 128,
            /* [2] */
            1, 129,
            /* [3] */
            2, 130
        },
        /* [2] */
        'ICN#',
        {   /* array IDArray: 3 elements */
            /* [1] */
            0, 1000,
            /* [2] */
            1, 1001,
            /* [3] */
            2, 1002
        }
    }
};

data CREATOR (0) {
    $"00"                                                                         /* . */
};
