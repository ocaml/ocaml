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

#define kMinSystemVersion 0x700

#define kExtraStackSpace (128 * 1024)
#define kMoreMasters 6
#define kScrapThreshold (4 * 1024)
#define kMinimumMemory (32 * 1024)

#define kTitleBarSpace 20
#define kWinBorderSpace 5
#define kPowerStripSpace 20
#define kVisualDelay 8UL          /* XXX use double-click time ?? */

#define ktextwidth 32000
#define kHorizScrollDelta 32
#define kGraphScrollDelta 8
#define kScrollBarWidth 15        /* not counting one of the borders. */
#define kTextMarginV 3
#define kTextMarginH 6
#define kMinWindowWidth 64
#define kMinWindowHeight 64

#define keyPgUp 0x74
#define keyPgDn 0x79
#define keyHome 0x73
#define keyEnd 0x77
#define keyF1 0x7A
#define keyF2 0x78
#define keyF3 0x63
#define keyF4 0x76

#define charEnter 0x03
#define charBackspace 0x08
#define charReturn 0x0D
#define charEscape 0x1B
#define charArrowLeft 0x1C
#define charArrowRight 0x1D
#define charArrowUp 0x1E
#define charArrowDown 0x1F
#define charDelete 0x7F

#define kWinUnknown 0
#define kWinUninitialised 1
#define kWinAbout 2
#define kWinToplevel 3
#define kWinGraphics 4
#define kWinDocument 5
#define kWinPrefs 6
#define kWinClipboard 7

#define kCreatorCaml 'Caml'
#define kTypeText 'TEXT'

/* Resource IDs */

#define kToplevelWinTemplate 1000
#define kGraphicsWinTemplate 1001
#define kDocumentWinTemplate 1002

#define kScrollBarTemplate 1000

#define kJoeCamlIcon 1000  /* see ocaml.r(ICON/cicn) before changing */

#define kDialogAbout 1000
#define kAlertNeedSys7 1001
#define kAlertBug 1002
#define kAlertGeneric 1003
#define kAlertNonzeroExit 1004
#define kDialogPrefs 1005
#define kAlertNotYet 1006
#define kAlertSaveAsk 1007
#define kAlertErrorMsg 1008
#define kAlertErrorNum 1009
#define kAlertNeed32BitQD 1010

#define kKeysOK 1000
#define kKeysSaveDontCancel 1001

#define kPrefsDescriptionStr 1000
#define kApplicationMissing -16397

#define kUndoStrings 1000

#define kMiscStrings 1001
#define kPrefsFileNameIdx 1
#define kUntitledIdx 2
#define kClosingIdx 3
#define kQuittingIdx (kClosingIdx + 1)
#define kCannotOpenIdx 5
#define kCloseQuoteIdx 6
#define kSaveAsPromptIdx 7
#define kEmptyIdx 8
#define kCannotWriteIdx 9

#define kErrorStrings 1002
#define kMemFull 1
#define kDiskFull 2
#define kDirFull 3
#define kTooManyFiles 4
#define kFileNotFound 5
#define kWriteProtect 6
#define kFileLocked 7
#define kVolLocked 8
#define kFileBusy 9
#define kFileOpen 10
#define kVolOffLine 11
#define kPermDenied 12
#define kWritePermDenied 13
#define kDirNotFound 14
#define kDisconnected 15
#define kIOError 16

#define kAboutText 1000

#define kMenuBar 1000

#define kCommandLineTemplate 1000
#define kEnvironmentTemplate 1001


/* Sound stuff */

#define kDurationOffset 0x1E
#define kSampleRateOffset 0x34


/* Menus */

#define kMenuApple 1000
#define kMenuFile 1001
#define kMenuEdit 1002
#define kMenuWindows 1003

/***** Apple menu */
#define kItemAbout 1

/***** File menu */
#define kItemNew 1
#define kItemOpen 2
/* - */
#define kItemClose 4
#define kItemSave 5
#define kItemSaveAs 6
#define kItemRevert 7
/* - */
#define kItemPageSetup 9
#define kItemPrint 10
/* - */
#define kItemQuit 12

/***** Edit menu */
#define kItemUndo 1
/* - */
#define kItemCut 3
#define kItemCopy 4
#define kItemPaste 5
#define kItemClear 6
#define kItemSelectAll 7
#define kItemShowClipboard 8
/* - */
#define kItemFind 10
#define kItemReplace 11
/* - */
#define kItemPreferences 13

/***** Windows menu */
#define kItemToplevel 1
#define kItemGraphics 2
/* - */
#define kItemDocuments 4
