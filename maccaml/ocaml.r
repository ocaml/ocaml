/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "constants.h"

resource 'vers' (1) {
#define d development
#define a alpha
#define b beta
  OCAMLVNUM, MACVNUM, STAGE, DEVVNUM,
  0,
  VERSIONSTR,
  "Objective Caml version " VERSIONSTR "\n"
  "Copyright 1991-1999 INRIA"
#undef d
#undef a
#undef b
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
    4000 * 1024,
    1500 * 1024
};

data 'Line' (kCommandLineTemplate) {
  "%a\000"
};

data 'Line' (kEnvironmentTemplate) {
  "CAMLLIB=%dstdlib:\000"
  "TempFolder=%t\000"
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
        StaticText {disabled,
          "You have discovered a bug in Objective Caml.  Please"
          " report the following information to <caml-light@inria.fr>."
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
            "Objective Caml cannot run on MacOS versions prior to System 7."
        },

        {10, 20, 42, 52}, Icon {disabled, kJoeCamlIcon},
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
            "Objective Caml needs a Macintosh with 32-bit color QuickDraw."
        },

        {10, 20, 42, 52}, Icon {disabled, kJoeCamlIcon},
    }
};

resource 'ALRT' (kAlertNonZeroExit) {
    {60, 81, 200, 431}, kAlertNonZeroExit,
    {
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
        OK, visible, silent,
    },
    alertPositionParentWindowScreen
};

resource 'DITL' (kAlertNonZeroExit) {
    {
        {100, 270, 120, 328}, Button {enabled, "OK"},

        {10, 70, 85, 328},
        StaticText {
          disabled,
          "The O'Caml toplevel loop exited with error code ^0."
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
        {10, 20, 42, 52}, Icon {disabled, kJoeCamlIcon},
    }
};

resource 'DLOG' (kDialogAbout) {
    {70, 60, 260, 452},
    noGrowDocProc,
    visible,
    goAway,
    0,
    kDialogAbout,
    "About Objective Caml",
    alertPositionMainScreen
};

resource 'DITL' (kDialogAbout) {
    {
        {10, 20, 42, 52}, Icon {disabled, kJoeCamlIcon},
        {10, 72, 180, 382}, UserItem { disabled },
    }
};

data 'TEXT' (kAboutText, purgeable) {
    "Objective Caml version " VERSIONSTR "\n"
    "Copyright 1991-1998 INRIA\n"
    "\n"
    "Xavier Leroy, Jer™me Vouillon, Damien Doligez, et al.\n"
    "\n"
    "\n"
    "O'Caml's interface to MacOS is compiled with MPW"
    " and uses the WASTE text engine.\n"
    "\n"
    "WASTE text engine © 1993-1996 Marco Piovanelli\n"
    "\n"
    "MPW libraries © 1995-1998 by Apple Computer, Inc., all rights reserved"
};

resource 'MBAR' (kMenuBar) {
    {kMenuApple, kMenuFile, kMenuEdit, kMenuWindows, }
};

resource 'MENU' (kMenuApple) {
    kMenuApple,
    textMenuProc,
    0x7FFFFFFD,
    enabled,
    apple,
    {
        "About Objective CamlÉ", noIcon, noKey, noMark, plain,
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
        "OpenÉ", noIcon, "O", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Close", noIcon, "W", noMark, plain,
        "Save", noIcon, "S", noMark, plain,
        "Save asÉ", noIcon, noKey, noMark, plain,
        "Revert to Saved", noIcon, noKey, noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "Page SetupÉ", noIcon, nokey, noMark, plain,
        "PrintÉ", noIcon, "P", noMark, plain,
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
        "FindÉ", noIcon, "F", noMark, plain,
        "ReplaceÉ", noIcon, "R", noMark, plain,
        "-", noIcon, noKey, noMark, plain,
        "PreferencesÉ", noIcon, noKey, noMark, plain,
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
    "Objective Caml Preferences",
    "Untitled",
    "closing",
    "quitting",
    "Unable to open \"",
    "\".  ",
    "Save file as:",
    "",
    "Unable to write to \"",
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
    "The file is already open (by Objective Caml or another application).",
    "The disk was ejected.",
    "The file is locked or you do not have the permission to open it.",
    "You do not have the permission to write to this file.",
    "The folder does not exist.",
    "The connexion to the file server was closed or broken.",
    "A hardware error occurred during input or output.",
  }
};

resource 'STR ' (kPrefsDescriptionStr, purgeable) {
  "This document describes user preferences for Objective Caml. "
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
  "O'Caml Toplevel",
  noAutoCenter
};

resource 'WIND' (kGraphicsWinTemplate) {
  {40, 4, 342, 512},
  zoomDocProc,
  invisible,
  goAway,
  0,
  "O'Caml Graphics",
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
    noData, freqDurationCmd {/* duration */ 0x4321, 60},
    noData, quietCmd {},
  },
  {
    4,
    /* sampling rate */ Rate22K,
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


/*****************************************************************
  derez -m 60 'alcools:caml light:caml-icons.rsrc' "{rincludes}types.r" ¶
        "{rincludes}finder.r" "{rincludes}icons.r" >>ocaml.r
*/

resource 'icl4' (1000) {
    $"0000 0000 000F FFFF F000 0000 0000 0000 0000 0000 00FF FFFF"
    $"FFF0 0000 0000 0000 0000 0000 FFFF FFFF FFFF 0000 0000 0000"
    $"FFFF FF0F FFFF BBBB BBBF F000 00FF FFF0 FAAA AAFF FFBB BBBB"
    $"BBBB BFFF FFFF FFF0 FAAA AAAF FFFF FFFF FFFF FFFF FFFF FFF0"
    $"FAAA AAAA FFFD DDFF FFFF FFFF FFFF FF00 FFAA AAAA AFFF CCFF"
    $"FFFF FFFF FFFF FF00 0FFA AAAA AAFF FCFF FFFF FBBF FFFF F000"
    $"00FF FAAA AAAF FFFF FFFF BBBB FFFF 0000 0000 FFAA AAAA FFFF"
    $"FFFF BBBB BBFF 0000 0000 0FFA AAAA AFFF FFFA ABBB BBBF F000"
    $"0000 0FFA AAAA ABBB BBFF AABB BBBB FF00 0000 FFFA AAAA BBBB"
    $"BBBF FAAB BBBB BFF0 0000 FFFA AAAB BBBB BBBB FFAA BBBB BBFF"
    $"0000 FFFA AAAB BBBB FFBB BFFA ABBB BBFF 0000 FFFA AAAB BBBB"
    $"FFBB BBFF AABA BBFF 0000 FFFA AAAB BBBB FFBB BBBF FAAA AAFF"
    $"0000 FFFA AAAB BBBB FFBB BBBB FFAA AAFF 0000 FFFA AAAA BBBB"
    $"FFBB BBBB BFFA AFFF 0000 FFFF AAAA ABBB FFFB BBBB BBFF AFBF"
    $"0000 0FFF AAAA AABB FFFB BBBB BBBF FFBF 0000 00FF AAAA AAA1"
    $"81FB BBBB BBBF FBBF 0000 000F AAAA A81A AFFF BBBB BBBF FBBF"
    $"0000 000F AAA1 8AAA AFFF FBBB BBBF FBBF 0000 00FF A81A 1AAA"
    $"AAAF FFBB BBBF FBBF 0000 00FA 11AA 8AAA AAAA FFFB BBFF FBF0"
    $"0000 0FF8 A8AA AAAA AAAA AFFF BFFF FBF0 0000 0F8A A8AA AAAA"
    $"AAAA AAFF FFFF FF00 0000 FFAA AAAA AAAA AAAA AFFF FFF0 0000"
    $"0000 FAAA AAAA AAAA AAAA FF00 0000 0000 0000 FFFF FFFF FFFF"
    $"FFFF F0"
};

resource 'icl4' (1001) {
    $"0FFF FFFF FFFF FFFF FFFF 0000 0000 0000 0F00 0000 0000 0000"
    $"000F F000 0000 0000 0F00 0000 0000 0000 000F CF00 0000 0000"
    $"0F00 0000 0FFF FF00 000F 0CF0 0000 0000 0F00 FFFF FFBB BFFF"
    $"FFFF 00CF 0000 0000 0F00 FAAF FFFF FFFF FFFF 0CCC F000 0000"
    $"0F00 FAAA FFFF FFFF FFFF FFFF FF00 0000 0F00 0FFA AFFF FFBB"
    $"FF00 DDDD DF00 0000 0F00 00FF AAFF FFAB BF00 CCCC CF00 0000"
    $"0F00 00FF AAAB BFAA BBF0 0000 CF00 0000 0F00 00FF AABB BBFA"
    $"ABBF 0000 CF00 0000 0F00 00FF AABB FBBF AAAF 0000 CF00 0000"
    $"0F00 00FF AABB FBBB FFFF 0000 CF00 0000 0F00 00FF AAAB FBBB"
    $"BFBF 0000 CF00 0000 0F00 000F AA81 FBBB BFBF 0000 CF00 0000"
    $"0F00 000F 818A AFBB BFBF 0000 CF00 0000 0F00 00FF 8A8A AAFB"
    $"BFF0 0000 CF00 0000 0F00 00F8 AAAA AAFF FF00 0000 CF00 0000"
    $"0F00 00FF FFFF FFF0 0000 0000 CF00 0000 0F00 0000 0000 0000"
    $"0000 0000 CF00 0000 0F00 0000 0000 0000 0000 0000 CF00 0000"
    $"0F00 0000 0000 0000 0000 0000 CF00 0000 0F00 0000 0000 0000"
    $"0000 0000 CF00 0000 0F00 FF00 FF00 0000 0000 0000 CF00 0000"
    $"0F00 FF00 FF00 0000 0000 0000 CF00 0000 0F00 0000 0000 0000"
    $"0000 0000 CF00 0000 0F00 FF00 FF00 0000 0000 0000 CFE0 0000"
    $"0F00 FF00 FF00 0000 0000 0000 CFEE E000 0F00 0F00 0F00 0000"
    $"0000 0000 CFEE EEE0 0F00 F000 F000 0000 0000 0000 CFEE EEE0"
    $"0F00 0000 0000 0000 0000 0000 CFEE E000 0FFF FFFF FFFF FFFF"
    $"FFFF FFFF FFE0"
};

resource 'icl4' (1002) {
    $"FFFF FFFF FFFF FFFF FFFF FFFF F000 0000 F000 0000 0000 0000"
    $"0000 0000 F000 0000 F00F F00F F000 0000 0000 0000 FFF0 0000"
    $"F00F F00F F000 0000 0000 0000 FDF0 0000 F000 0000 0000 0000"
    $"0000 0000 FDF0 0000 F00F F00F F000 0000 0000 0000 FDF0 0000"
    $"F00F F00F F000 0000 0000 0000 FDF0 0000 F000 F000 F000 0000"
    $"0000 0000 FDF0 0000 F00F 000F 0000 0000 0000 0000 FDF0 0000"
    $"F000 0000 0000 0000 0000 0000 FDF0 0000 F000 0000 0000 0000"
    $"0000 0000 FDF0 0000 F000 0000 0000 0000 0000 0000 FDF0 0000"
    $"F000 0000 FFFF F000 0000 0000 FDF0 0000 F00F FFFF FBBB FFFF"
    $"FFF0 0000 FDF0 0000 F00F AAFF FFFF FFFF FFF0 0000 FDF0 0000"
    $"F00F AAAF FFFF FFFF FF00 0000 FDF0 0000 F000 FFAA FFFF FBBF"
    $"F000 0000 FDF0 0000 F000 0FFA AFFF FABB F000 0000 FDF0 0000"
    $"F000 0FFA AABB FAAB BF00 0000 FDF0 0000 F000 0FFA ABBB BFAA"
    $"BBF0 0000 FDF0 0000 F000 0FFA ABBF BBFA AAF0 0000 FDF0 0000"
    $"F000 0FFA ABBF BBBF FFF0 0000 FDF0 0000 F000 0FFA AABF BBBB"
    $"FBF0 0000 FDF0 0000 F000 00FA A81F BBBB FBFF FFFF FDF0 0000"
    $"F000 00F8 18AA FBBB FBFC CCCF DCF0 0000 F000 0FF8 A8AA AFBB"
    $"FFFC CCFD CCF0 0000 F000 0F8A AAAA AFFF F0FC CFDC CCFE 0000"
    $"F000 0FFF FFFF FF00 00FC FDCC CCFE EE00 F000 0000 0000 0000"
    $"00FF DCCC CCFE EEEE FFFF FFFF FFFF FFFF FFFD CCCC CCFE EEEE"
    $"00FD DDDD DDDD DDDD DDDC CCCC CCFE EE00 00FF FFFF FFFF FFFF"
    $"FFFF FFFF FFFE"
};

resource 'icl8' (1000) {
    $"0000 0000 0000 0000 0000 00FF FFFF FFFF FF00 0000 0000 0000"
    $"0000 0000 0000 0000 0000 0000 0000 0000 0000 FFFF FFFF FFFF"
    $"FFFF FF00 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000"
    $"FFFF FFFF FFFF FFFF FFFF FFFF 0000 0000 0000 0000 0000 0000"
    $"FFFF FFFF FFFF 00FF FFFF FFFF 0808 0808 0808 08FF FF00 0000"
    $"0000 FFFF FFFF FF00 FF33 3333 3333 FFFF FFFF 0808 0808 0808"
    $"0808 0808 08FF FFFF FFFF FFFF FFFF FF00 FF33 3333 3333 33FF"
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FF00"
    $"FF33 3333 3333 3333 FFFF FFF9 F9F9 FFFF FFFF FFFF FFFF FFFF"
    $"FFFF FFFF FFFF 0000 FFFF 3333 3333 3333 33FF FFFF F6F6 FFFF"
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF 0000 00FF FF33 3333 3333"
    $"3333 FFFF FFF6 FFFF FFFF FFFF FF08 08FF FFFF FFFF FF00 0000"
    $"0000 FFFF FF33 3333 3333 33FF FFFF FFFF FFFF FFFF 0808 0808"
    $"FFFF FFFF 0000 0000 0000 0000 FFFF 3333 3333 3333 FFFF FFFF"
    $"FFFF FFFF 0808 0808 0808 FFFF 0000 0000 0000 0000 00FF FF33"
    $"3333 3333 33FF FFFF FFFF FF33 3308 0808 0808 08FF FF00 0000"
    $"0000 0000 00FF FF33 3333 3333 3308 0808 0808 FFFF 3333 0808"
    $"0808 0808 FFFF 0000 0000 0000 FFFF FF33 3333 3333 0808 0808"
    $"0808 08FF FF33 3308 0808 0808 08FF FF00 0000 0000 FFFF FF33"
    $"3333 3308 0808 0808 0808 0808 FFFF 3333 0808 0808 0808 FFFF"
    $"0000 0000 FFFF FF33 3333 3308 0808 0808 FFFF 0808 08FF FF33"
    $"3308 0808 0808 FFFF 0000 0000 FFFF FF33 3333 3308 0808 0808"
    $"FFFF 0808 0808 FFFF 3333 0833 0808 FFFF 0000 0000 FFFF FF33"
    $"3333 3308 0808 0808 FFFF 0808 0808 08FF FF33 3333 3333 FFFF"
    $"0000 0000 FFFF FF33 3333 3308 0808 0808 FFFF 0808 0808 0808"
    $"FFFF 3333 3333 FFFF 0000 0000 FFFF FF33 3333 3333 0808 0808"
    $"FFFF 0808 0808 0808 08FF FF33 33FF FFFF 0000 0000 FFFF FFFF"
    $"3333 3333 3308 0808 FFFF FF08 0808 0808 0808 FFFF 33FF 08FF"
    $"0000 0000 00FF FFFF 3333 3333 3333 0808 FFFF FF08 0808 0808"
    $"0808 08FF FFFF 08FF 0000 0000 0000 FFFF 3333 3333 3333 3305"
    $"E305 FF08 0808 0808 0808 08FF FF08 08FF 0000 0000 0000 00FF"
    $"3333 3333 33E3 0533 33FF FFFF 0808 0808 0808 08FF FF08 08FF"
    $"0000 0000 0000 00FF 3333 3305 E333 3333 33FF FFFF FF08 0808"
    $"0808 08FF FF08 08FF 0000 0000 0000 FFFF 33E3 0533 0533 3333"
    $"3333 33FF FFFF 0808 0808 08FF FF08 08FF 0000 0000 0000 FF33"
    $"0505 3333 E333 3333 3333 3333 FFFF FF08 0808 FFFF FF08 FF00"
    $"0000 0000 00FF FFE3 33E3 3333 3333 3333 3333 3333 33FF FFFF"
    $"08FF FFFF FF08 FF00 0000 0000 00FF E333 33E3 3333 3333 3333"
    $"3333 3333 3333 FFFF FFFF FFFF FFFF 0000 0000 0000 FFFF 3333"
    $"3333 3333 3333 3333 3333 3333 33FF FFFF FFFF FF00 0000 0000"
    $"0000 0000 FF33 3333 3333 3333 3333 3333 3333 3333 FFFF 0000"
    $"0000 0000 0000 0000 0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FFFF FFFF FF"
};

resource 'icl8' (1001) {
    $"00FF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF 0000 0000"
    $"0000 0000 0000 0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5FF FF00 0000 0000 0000 0000 0000 00FF F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF F8FF 0000 0000 0000 0000 0000"
    $"00FF F5F5 F5F5 F5F5 F5FF FFFF FFFF F5F5 F5F5 F5FF 00F8 FF00"
    $"0000 0000 0000 0000 00FF F5F5 FFFF FFFF FFFF 0808 08FF FFFF"
    $"FFFF FFFF 0000 F8FF 0000 0000 0000 0000 00FF F5F5 FF33 33FF"
    $"FFFF FFFF FFFF FFFF FFFF FFFF F5F6 F6F8 FF00 0000 0000 0000"
    $"00FF F5F5 FF33 3333 FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FFFF 0000 0000 0000 00FF F5F5 F5FF FF33 33FF FFFF FFFF 0808"
    $"FFFF F5F5 F9F9 F9F9 F9FF 0000 0000 0000 00FF F5F5 F5F5 FFFF"
    $"3333 FFFF FFFF 3308 08FF F5F5 F7F7 F7F7 F7FF 0000 0000 0000"
    $"00FF F5F5 F5F5 FFFF 3333 3308 08FF 3333 0808 FFF5 F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 FFFF 3333 0808 0808 FF33"
    $"3308 08FF F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 F5F5 FFFF"
    $"3333 0808 FF08 08FF 3333 33FF F5F5 F5F5 F7FF 0000 0000 0000"
    $"00FF F5F5 F5F5 FFFF 3333 0808 FF08 0808 FFFF FFFF F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 FFFF 3333 3308 FF08 0808"
    $"08FF 08FF F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 F5F5 F5FF"
    $"3333 E305 FF08 0808 08FF 08FF F5F5 F5F5 F7FF 0000 0000 0000"
    $"00FF F5F5 F5F5 F5FF E305 E333 33FF 0808 08FF 08FF F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 FFFF E333 E333 3333 FF08"
    $"08FF FFF5 F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 F5F5 FFE3"
    $"3333 3333 3333 FFFF FFFF F5F5 F5F5 F5F5 F7FF 0000 0000 0000"
    $"00FF F5F5 F5F5 FFFF FFFF FFFF FFFF FFF5 F5F5 F5F5 F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F7FF 0000 0000 0000"
    $"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 FFFF F5F5"
    $"FFFF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F7FF 0000 0000 0000"
    $"00FF F5F5 FFFF F5F5 FFFF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F7FF 0000 0000 0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F7FF 0000 0000 0000 00FF F5F5 FFFF F5F5"
    $"FFFF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F7FF FC00 0000 0000"
    $"00FF F5F5 FFFF F5F5 FFFF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F7FF FCFC FC00 0000 00FF F5F5 F5FF F5F5 F5FF F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F7FF FCFC FCFC FC00 00FF F5F5 FFF5 F5F5"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F7FF FCFC FCFC FC00"
    $"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F7FF FCFC FC00 0000 00FF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FFFF FFFF FFFF FFFF FFFF FC"
};

resource 'icl8' (1002) {
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FF00 0000 0000 0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 FF00 0000 0000 0000 FFF5 F5FF FFF5 F5FF"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FFFF FF00 0000 0000"
    $"FFF5 F5FF FFF5 F5FF FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5FF FFF5 F5FF"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5FF FFF5 F5FF FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 FFF5 F5F5 FFF5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5FF F5F5 F5FF"
    $"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5F5 F5F5 F5F5 FFFF FFFF FFF5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5FF FFFF FFFF FF08 0808 FFFF FFFF"
    $"FFFF FFF5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5FF 3333 FFFF"
    $"FFFF FFFF FFFF FFFF FFFF FFF5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5FF 3333 33FF FFFF FFFF FFFF FFFF FFFF F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 FFFF 3333 FFFF FFFF FF08 08FF"
    $"FFF5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5F5 F5FF FF33"
    $"33FF FFFF FF33 0808 FFF5 F5F5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5F5 F5FF FF33 3333 0808 FF33 3308 08FF F5F5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 F5FF FF33 3308 0808 08FF 3333"
    $"0808 FFF5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5F5 F5FF FF33"
    $"3308 08FF 0808 FF33 3333 FFF5 F5F5 F5F5 FFF9 FF00 0000 0000"
    $"FFF5 F5F5 F5FF FF33 3308 08FF 0808 08FF FFFF FFF5 F5F5 F5F5"
    $"FFF9 FF00 0000 0000 FFF5 F5F5 F5FF FF33 3333 08FF 0808 0808"
    $"FF08 FFF5 F5F5 F5F5 FFF9 FF00 0000 0000 FFF5 F5F5 F5F5 FF33"
    $"33E3 05FF 0808 0808 FF08 FFFF FFFF FFFF FFF9 FF00 0000 0000"
    $"FFF5 F5F5 F5F5 FFE3 05E3 3333 FF08 0808 FF08 FF2B 2B2B F7FF"
    $"F9F7 FF00 0000 0000 FFF5 F5F5 F5FF FFE3 33E3 3333 33FF 0808"
    $"FFFF FF2B 2BF7 FFF9 F72B FF00 0000 0000 FFF5 F5F5 F5FF E333"
    $"3333 3333 33FF FFFF FFF5 FF2B F7FF F9F7 2BF6 FFFC 0000 0000"
    $"FFF5 F5F5 F5FF FFFF FFFF FFFF FFFF F5F5 F5F5 FFF7 FFF9 F72B"
    $"F6F6 FFFC FCFC 0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
    $"F5F5 FFFF F9F7 2BF6 F6F6 FFFC FCFC FCFC FFFF FFFF FFFF FFFF"
    $"FFFF FFFF FFFF FFFF FFFF FFF9 F72B F6F6 F6F6 FFFC FCFC FCFC"
    $"0000 FFF9 F9F9 F9F9 F9F9 F9F9 F9F9 F9F9 F9F9 F9F7 2BF6 F6F6"
    $"F6F6 FFFC FCFC 0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FFFF FFFF FFFF FFFF FFFF FFFC"
};

resource 'ICN#' (1000) {
    {   /* array: 2 elements */
        /* [1] */
        $"001F 8000 003F E000 00FF F000 FDF0 183E 83C0 07FE 81FF FFFE"
        $"80E3 FFFC C073 FFFC 603B F9F8 381F F0F0 0C0F F030 0607 E018"
        $"0600 300C 0E00 1806 0E00 0C03 0E00 C603 0E00 C303 0E00 C183"
        $"0E00 C0C3 0E00 C067 0F00 E035 0700 E01D 0301 E019 0106 7019"
        $"0118 7819 0368 1C19 02C8 0E3A 0740 077A 0640 03FC 0C00 07E0"
        $"0800 0C00 0F7F F8",
        /* [2] */
        $"001F 8000 003F E000 00FF F000 FDFF F83E FFFF FFFE FFFF FFFE"
        $"FFFF FFFC FFFF FFFC 7FFF FFF8 3FFF FFF0 0FFF FFF0 07FF FFF8"
        $"07FF FFFC 0FFF FFFE 0FFF FFFF 0FFF FFFF 0FFF FFFF 0FFF FFFF"
        $"0FFF FFFF 0FFF FFFF 0FFF FFFF 07FF FFFF 03FF FFFF 01FF FFFF"
        $"01FF FFFF 03FF FFFF 03FF FFFE 07FF FFFE 07FF FFFC 0FFF FFE0"
        $"0FFF FC00 0FFF F8"
    }
};

resource 'ICN#' (1001) {
    {   /* array: 2 elements */
        /* [1] */
        $"7FFF F000 4000 1800 4000 1400 407C 1200 4FC7 F100 49FF F080"
        $"48FF FFC0 467C C040 433C 4040 4304 2040 4302 1040 4309 1040"
        $"4308 F040 4308 5040 4138 5040 41E4 5040 43A2 6040 4203 C040"
        $"43FE 0040 4000 0040 4000 0040 4000 0040 4000 0040 4CC0 0040"
        $"4CC0 0040 4000 0040 4CC0 0060 4CC0 0078 4440 007E 4880 007E"
        $"4000 0078 7FFF FFE0",
        /* [2] */
        $"7FFF F000 7FFF F800 7FFF FC00 7FFF FE00 7FFF FF00 7FFF FF80"
        $"7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0"
        $"7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0"
        $"7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0 7FFF FFC0"
        $"7FFF FFC0 7FFF FFC0 7FFF FFE0 7FFF FFF8 7FFF FFFE 7FFF FFFE"
        $"7FFF FFF8 7FFF FFE0"
    }
};

resource 'ICN#' (1002) {
    {   /* array: 2 elements */
        /* [1] */
        $"FFFF FF80 8000 0080 9980 00E0 9980 00A0 8000 00A0 9980 00A0"
        $"9980 00A0 8880 00A0 9100 00A0 8000 00A0 8000 00A0 8000 00A0"
        $"80F8 00A0 9F8F E0A0 93FF E0A0 91FF C0A0 8CF9 80A0 8678 80A0"
        $"8608 40A0 8604 20A0 8612 20A0 8611 E0A0 8610 A0A0 8270 BFA0"
        $"83C8 A120 8744 E220 8407 A430 87FC 283C 8000 303F FFFF E03F"
        $"2000 003C 3FFF FFF0",
        /* [2] */
        $"FFFF FF80 FFFF FF80 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0"
        $"FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0"
        $"FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0"
        $"FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0 FFFF FFE0"
        $"FFFF FFE0 FFFF FFE0 FFFF FFF0 FFFF FFFC FFFF FFFF FFFF FFFF"
        $"3FFF FFFC 3FFF FFF0"
    }
};

resource 'ics#' (1000) {
    {   /* array: 2 elements */
        /* [1] */
        $"07C0 FC7F 9FFF 8FFE 67CC 33C4 3042 3021 3091 308F 3085 1385"
        $"1E45 3A26 203C 3FE0",
        /* [2] */
        $"07C0 FFFF FFFF FFFE 7FFC 3FFC 3FFE 3FFF 3FFF 3FFF 3FFF 1FFF"
        $"1FFF 3FFE 3FFC 3FE0"
    }
};

resource 'ics#' (1001) {
    {   /* array: 2 elements */
        /* [1] */
        $"FFE0 8070 8058 8078 8008 8008 8008 B608 B608 8008 B608 B608"
        $"9208 A40E 800F FFFE",
        /* [2] */
        $"FFE0 FFF0 FFF8 FFF8 FFF8 FFF8 FFF8 FFF8 FFF8 FFF8 FFF8 FFF8"
        $"FFF8 FFFE FFFF FFFE"
    }
};

resource 'ics#' (1002) {
    {   /* array: 2 elements */
        /* [1] */
        $"FFF8 800C B60C B60C 800C B60C B60C 920C A40C 800C 800C 807C"
        $"8054 8066 FFC7 7FFE",
        /* [2] */
        $"FFF8 FFFC FFFC FFFC FFFC FFFC FFFC FFFC FFFC FFFC FFFC FFFC"
        $"FFFC FFFE FFFF 7FFE"
    }
};

resource 'ics4' (1000) {
    $"0000 0FFF FF00 0000 FFFF FFBB BFFF FFFF FAAF FFFF FFFF FFFF"
    $"FAAA FFFF FFFF FFF0 0FFA AFFF FFBB FF00 00FF AAFF FFAB BF00"
    $"00FF AAAB BFAA BBF0 00FF AABB BBFA ABBF 00FF AABB FBBF AAAF"
    $"00FF AABB FBBB FFFF 00FF AAAB FBBB BFBF 000F AA81 FBBB BFBF"
    $"000F 818A AFBB BFBF 00FF 8A8A AAFB BFF0 00F8 AAAA AAFF FF00"
    $"00FF FFFF FFF0"
};

resource 'ics4' (1001) {
    $"FFFF FFFF FFF0 0000 F000 0000 0FFF 0000 F000 0000 0FCF F000"
    $"F000 0000 0FFF F000 F000 0000 00CC F000 F000 0000 000C F000"
    $"F000 0000 000C F000 F0FF 0FF0 000C F000 F0FF 0FF0 000C F000"
    $"F000 0000 000C F000 F0FF 0FF0 000C F000 F0FF 0FF0 000C F000"
    $"F00F 00F0 000C F000 F0F0 0F00 000C FEE0 F000 0000 000C FEEE"
    $"FFFF FFFF FFFF FEE0"
};

resource 'ics4' (1002) {
    $"FFFF FFFF FFFF F000 F000 0000 0000 FF00 F0FF 0FF0 0000 FF00"
    $"F0FF 0FF0 0000 FF00 F000 0000 0000 FF00 F0FF 0FF0 0000 FF00"
    $"F0FF 0FF0 0000 FF00 F00F 00F0 0000 FF00 F0F0 0F00 0000 FF00"
    $"F000 0000 0000 FF00 F000 0000 0000 FF00 F000 0000 0FFF FF00"
    $"F000 0000 0FCF DF00 F000 0000 0FFD CFE0 FFFF FFFF FFDC CFEE"
    $"0FFF FFFF FFFF FFE0"
};

resource 'ics8' (1000) {
    $"0000 0000 00FF FFFF FFFF 0000 0000 0000 FFFF FFFF FFFF 0808"
    $"08FF FFFF FFFF FFFF FF33 33FF FFFF FFFF FFFF FFFF FFFF FFFF"
    $"FF33 3333 FFFF FFFF FFFF FFFF FFFF FF00 00FF FF33 33FF FFFF"
    $"FFFF 0808 FFFF 0000 0000 FFFF 3333 FFFF FFFF 3308 08FF 0000"
    $"0000 FFFF 3333 3308 08FF 3333 0808 FF00 0000 FFFF 3333 0808"
    $"0808 FF33 3308 08FF 0000 FFFF 3333 0808 FF08 08FF 3333 33FF"
    $"0000 FFFF 3333 0808 FF08 0808 FFFF FFFF 0000 FFFF 3333 3308"
    $"FF08 0808 08FF 08FF 0000 00FF 3333 E305 FF08 0808 08FF 08FF"
    $"0000 00FF E305 E333 33FF 0808 08FF 08FF 0000 FFFF E333 E333"
    $"3333 FF08 08FF FF00 0000 FFE3 3333 3333 3333 FFFF FFFF 0000"
    $"0000 FFFF FFFF FFFF FFFF FF"
};

resource 'ics8' (1001) {
    $"FFFF FFFF FFFF FFFF FFFF FF00 0000 0000 FFF5 F5F5 F5F5 F5F5"
    $"F5FF FFFF 0000 0000 FFF5 F5F5 F5F5 F5F5 F5FF F6FF FF00 0000"
    $"FFF5 F5F5 F5F5 F5F5 F5FF FFFF FF00 0000 FFF5 F5F5 F5F5 F5F5"
    $"F5F5 F7F7 FF00 0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F7 FF00 0000"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F7 FF00 0000 FFF5 FFFF F5FF FFF5"
    $"F5F5 F5F7 FF00 0000 FFF5 FFFF F5FF FFF5 F5F5 F5F7 FF00 0000"
    $"FFF5 F5F5 F5F5 F5F5 F5F5 F5F7 FF00 0000 FFF5 FFFF F5FF FFF5"
    $"F5F5 F5F7 FF00 0000 FFF5 FFFF F5FF FFF5 F5F5 F5F7 FF00 0000"
    $"FFF5 F5FF F5F5 FFF5 F5F5 F5F7 FF00 0000 FFF5 FFF5 F5FF F5F5"
    $"F5F5 F5F7 FFFC FC00 FFF5 F5F5 F5F5 F5F5 F5F5 F5F7 FFFC FCFC"
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFC FC"
};

resource 'ics8' (1002) {
    $"FFFF FFFF FFFF FFFF FFFF FFFF FF00 0000 FFF5 F5F5 F5F5 F500"
    $"F5F5 F5F5 FFFF 0000 FFF5 FFFF F5FF FF00 F5F5 F5F5 FFFF 0000"
    $"FFF5 FFFF F5FF FF00 F5F5 F5F5 FFFF 0000 FFF5 F5F5 F5F5 F500"
    $"F5F5 F5F5 FFFF 0000 FFF5 FFFF F5FF FF00 F5F5 F5F5 FFFF 0000"
    $"FFF5 FFFF F5FF FF00 F5F5 F5F5 FFFF 0000 FFF5 F5FF F5F5 FF00"
    $"F5F5 F5F5 FFFF 0000 FFF5 FFF5 F5FF F500 F5F5 F5F5 FFFF 0000"
    $"FFF5 F5F5 F5F5 F500 F5F5 F5F5 FFFF 0000 FFF5 F5F5 F5F5 F5F5"
    $"F5F5 F5F5 FFFF 0000 FFF5 F5F5 F5F5 F5F5 F5FF FFFF FFFF 0000"
    $"FFF5 F5F5 F5F5 F5F5 F5FF F5FF F9FF 0000 FFF5 F5F5 F5F5 F5F5"
    $"F5FF FFF9 F7FF FC00 FFFF FFFF FFFF FFFF FFFF F9F7 F7FF FCFC"
    $"00FF FFFF FFFF FFFF FFFF FFFF FFFF FC"
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
    'Caml',
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

resource 'ICON' (1000) {
    $"001F 8000 003F E000 00FF F000 FDF0 183E 83C0 07FE 81FF FFFE"
    $"80E3 FFFC C073 FFFC 603B F9F8 381F F0F0 0C0F F030 0607 E018"
    $"0600 300C 0E00 1806 0E00 0C03 0E00 C603 0E00 C303 0E00 C183"
    $"0E00 C0C3 0E00 C067 0F00 E035 0700 E01D 0301 E019 0106 7019"
    $"0118 7819 0368 1C19 02C8 0E3A 0740 077A 0640 03FC 0C00 07E0"
    $"0800 0C00 0FFF F8"
};

data 'cicn' (1000) {
    $"0000 0000 8010 0000 0000 0020 0020 0000 0000 0000 0000 0048"                /* ....€...... . .........H */
    $"0000 0048 0000 0000 0004 0001 0004 0000 0000 0000 0000 0000"                /* ...H.................... */
    $"0000 0000 0000 0004 0000 0000 0020 0020 0000 0000 0004 0000"                /* ............. . ........ */
    $"0000 0020 0020 0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF FFFF"                /* ... . ....ÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"                /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"                /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"                /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"                /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
    $"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF 001F 8000 003F"                /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ..€..? */
    $"E000 00FF F000 FDF0 183E 83C0 07FE 81FF FFFE 80E3 FFFC C073"                /* à..ÿğ.ığ.>ƒÀ.şÿÿş€ãÿüÀs */
    $"FFFC 603B F9F8 381F F0F0 0C0F F030 0607 E018 0600 300C 0E00"                /* ÿü`;ùø8.ğğ..ğ0..à...0... */
    $"1806 0E00 0C03 0E00 C603 0E00 C303 0E00 C183 0E00 C0C3 0E00"                /* ........Æ...Ã...Áƒ..ÀÃ.. */
    $"C067 0F00 E035 0700 E01D 0301 E019 0106 7019 0118 7819 0368"                /* Àg..à5..à...à...p...x..h */
    $"1C19 02C8 0E3A 0740 077A 0640 03FC 0C00 07E0 0800 0C00 0FFF"                /* ...È.:.@.z.@.ü...à.....ÿ */
    $"F800 0000 0000 0000 0007 0000 FFFF FFFF FFFF 0001 FFFF FFFF"                /* ø...........ÿÿÿÿÿÿ..ÿÿÿÿ */
    $"0000 0002 CCCC 9999 6666 0003 8888 8888 8888 0004 DDDD DDDD"                /* ....ÌÌ™™ff..ˆˆˆˆˆˆ..İİİİ */
    $"DDDD 0005 FFFF CCCC 9999 0006 0000 BBBB 0000 000F 0000 0000"                /* İİ..ÿÿÌÌ™™....»»........ */
    $"0000 0000 0000 000F FFFF F000 0000 0000 0000 0000 0000 00FF"                /* ........ÿÿğ............ÿ */
    $"FFFF FFF0 0000 0000 0000 0000 0000 FFFF FFFF FFFF 0000 0000"                /* ÿÿÿğ..........ÿÿÿÿÿÿ.... */
    $"0000 FFFF FF0F FFFF 5555 555F F000 00FF FFF0 F222 22FF FF55"                /* ..ÿÿÿ.ÿÿUUU_ğ..ÿÿğò""ÿÿU */
    $"5555 5555 5FFF FFFF FFF0 F222 222F FFFF FFFF FFFF FFFF FFFF"                /* UUUU_ÿÿÿÿğò""/ÿÿÿÿÿÿÿÿÿÿ */
    $"FFF0 F222 2222 FFF3 33FF FFFF FFFF FFFF FF00 FF22 2222 2FFF"                /* ÿğò"""ÿó3ÿÿÿÿÿÿÿÿ.ÿ"""/ÿ */
    $"44FF FFFF FFFF FFFF FF00 0FF2 2222 22FF F4FF FFFF F55F FFFF"                /* Dÿÿÿÿÿÿÿÿ..ò"""ÿôÿÿÿõ_ÿÿ */
    $"F000 00FF F222 222F FFFF FFFF 5555 FFFF 0000 0000 FF22 2222"                /* ğ..ÿò""/ÿÿÿÿUUÿÿ....ÿ""" */
    $"FFFF FFFF 5555 55FF 0000 0000 0FF2 2222 2FFF FFF2 2555 555F"                /* ÿÿÿÿUUUÿ.....ò""/ÿÿò%UU_ */
    $"F000 0000 0FF2 2222 2555 55FF 2255 5555 FF00 0000 FFF2 2222"                /* ğ....ò""%UUÿ"UUUÿ...ÿò"" */
    $"5555 555F F225 5555 5FF0 0000 FFF2 2225 5555 5555 FF22 5555"                /* UUU_ò%UU_ğ..ÿò"%UUUUÿ"UU */
    $"55FF 0000 FFF2 2225 5555 FF55 5FF2 2555 55FF 0000 FFF2 2225"                /* Uÿ..ÿò"%UUÿU_ò%UUÿ..ÿò"% */
    $"5555 FF55 55FF 2252 55FF 0000 FFF2 2225 5555 FF55 555F F222"                /* UUÿUUÿ"RUÿ..ÿò"%UUÿUU_ò" */
    $"22FF 0000 FFF2 2225 5555 FF55 5555 FF22 22FF 0000 FFF2 2222"                /* "ÿ..ÿò"%UUÿUUUÿ""ÿ..ÿò"" */
    $"5555 FF55 5555 5FF2 2FFF 0000 FFFF 2222 2555 FFF5 5555 55FF"                /* UUÿUUU_ò/ÿ..ÿÿ""%UÿõUUUÿ */
    $"2F5F 0000 0FFF 2222 2255 FFF5 5555 555F FF5F 0000 00FF 2222"                /* /_...ÿ"""UÿõUUU_ÿ_...ÿ"" */
    $"2221 61F5 5555 555F F55F 0000 000F 2222 2612 2FFF 5555 555F"                /* "!aõUUU_õ_....""&./ÿUUU_ */
    $"F55F 0000 000F 2221 6222 2FFF F555 555F F55F 0000 00FF 2612"                /* õ_...."!b"/ÿõUU_õ_...ÿ&. */
    $"1222 222F FF55 555F F55F 0000 00F2 1122 6222 2222 FFF5 55FF"                /* .""/ÿUU_õ_...ò."b"""ÿõUÿ */
    $"F5F0 0000 0FF6 2622 2222 2222 2FFF 5FFF F5F0 0000 0F62 2622"                /* õğ...ö&"""""/ÿ_ÿõğ...b&" */
    $"2222 2222 22FF FFFF FF00 0000 FF22 2222 2222 2222 2FFF FFF0"                /* """""ÿÿÿÿ...ÿ"""""""/ÿÿğ */
    $"0000 0000 F222 2222 2222 2222 FF00 0000 0000 0000 FFFF FFFF"                /* ....ò"""""""ÿ.......ÿÿÿÿ */
    $"FFFF FFFF F000 0000 0000"                                                   /* ÿÿÿÿğ..... */
};

data 'Caml' (0) {
    $"00"                                                                         /* . */
};
