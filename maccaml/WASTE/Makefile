# Makefile for the WASTE library (version 1.3)

# $Id$

# Put this makefile into the "WASTE 1.3 Distribution" folder.
# If you type "Make all", it will build five files:
#
# WASTELib.o                 the WASTE library (classic 68k version)
# WASTELib.CFMo              the WASTE library (CFM 68k version)
# WASTELib.x                 the WASTE library (PPC version)
# "WASTE Demo (CFM)"         the demo application (CFM 68k and CFM PPC)
# "WASTE Demo (classic)"     the demo application (classic 68k and CFM PPC)

# You can easily change the WASTE options, the compiler options, and the
# libraries to suit your needs.  (see below)


#### WASTE Options.  See ":WASTE 1.3:Private Includes:WASTEIntf.h" for details.

# The defaults are shown here.
#WEDefs = -d WASTE_DEBUG=0  ¶
#         -d WASTE_OBJECTS=1  ¶
#         -d WASTE_TSM_SUPPORT=1  ¶
#         -d WASTE_DRAG_AND_DROP=1  ¶
#         -d WASTE_USE_UPPS=GENERATINGCFM  ¶
#         -d WASTE_NO_SYNCH=0  ¶
#         -d WASTE_NO_RO_CARET=1  ¶
#         -d WASTE_IC_SUPPORT=1  ¶
#         -d WASTE_OBJECTS_ARE_GLYPHS=0  ¶
#         -d WASTE_RESOLVE_FONT_DESIGNATORS=1  ¶
#         -d WASTE_WECALTEXT_DOES_REDRAW=0  ¶
#         -d WASTE_TRANSLUCENT_DRAGS=0

WEDefs = -d WASTE_DEBUG=0  ¶
         -d WASTE_OBJECTS=0  ¶
         -d WASTE_TSM_SUPPORT=0  ¶
         -d WASTE_DRAG_AND_DROP=1  ¶
         -d WASTE_USE_UPPS=GENERATINGCFM  ¶
         -d WASTE_NO_SYNCH=0  ¶
         -d WASTE_NO_RO_CARET=0  ¶
         -d WASTE_IC_SUPPORT=0  ¶
         -d WASTE_OBJECTS_ARE_GLYPHS=0  ¶
         -d WASTE_RESOLVE_FONT_DESIGNATORS=1  ¶
         -d WASTE_WECALTEXT_DOES_REDRAW=0  ¶
         -d WASTE_TRANSLUCENT_DRAGS=0


#### Compilers and their options.

# Classic 68k
C = sc
COptions = {Defs} {Incl} -model far -w 17 -proto strict -sym on
#C = mwc68k
#COptions = {Defs} {Incl} -model far

# CFM 68k
CFMC = sc
CFMCOptions = {Defs} {Incl} -model cfmflat -w 17 -proto strict -sym on
#CFMC = {c}
#CFMCOptions = {coptions}

# PPC
PPCC = mrc
PPCCOptions = {Defs} {Incl} -w 35 -sym on


#### Libraries for the demo application

Libs = "{libraries}MacRuntime.o" ¶
       "{clibraries}StdCLib.far.o" ¶
       "{libraries}Interface.o"
#Libs = "{mw68klibraries}macos.lib" ¶
#       "{mw68klibraries}ANSI (N/4i/8d) C.68K.Lib"

CFMLibs = "{CFM68kLibraries}NuMacRuntime.o" ¶
          "{sharedlibraries}StdCLib" ¶
          "{sharedlibraries}DragLib" ¶
          "{sharedlibraries}InterfaceLib"
#CFMLibs = {Libs}

PPCLibs = "{ppclibraries}PPCCRuntime.o" ¶
          "{sharedlibraries}StdCLib" ¶
          "{ppclibraries}StdCRuntime.o" ¶
          "{sharedlibraries}DragLib" ¶
          "{sharedlibraries}InterfaceLib" ¶


#### Common definitions

# Compiler options
Incl = -i ":WASTE C/C++ Headers:" {OHIncl} {WEIncl} {WDIncl} {WTIncl}
Defs = {WEDefs} -d inline=static


#### Main target

all Ä everything


#### :WASTE 1.3:

WEObj = ¶
  ':WASTE 1.3:Source:WEAccessors.c.o' ¶
  ':WASTE 1.3:Source:WEBirthDeath.c.o' ¶
  ':WASTE 1.3:Source:WEDebug.c.o' ¶
  ':WASTE 1.3:Source:WEDrawing.c.o' ¶
  ':WASTE 1.3:Source:WEFontTables.c.o' ¶
  ':WASTE 1.3:Source:WEHighLevelEditing.c.o' ¶
  ':WASTE 1.3:Source:WEICGlue.c.o' ¶
  ':WASTE 1.3:Source:WEInlineInput.c.o' ¶
  ':WASTE 1.3:Source:WELineLayout.c.o' ¶
  ':WASTE 1.3:Source:WELongCoords.c.o' ¶
  ':WASTE 1.3:Source:WELowLevelEditing.c.o' ¶
  ':WASTE 1.3:Source:WEMouse.c.o' ¶
  ':WASTE 1.3:Source:WEObjects.c.o' ¶
  ':WASTE 1.3:Source:WEScraps.c.o' ¶
  ':WASTE 1.3:Source:WESelecting.c.o' ¶
  ':WASTE 1.3:Source:WESelectors.c.o' ¶
  ':WASTE 1.3:Source:WEUserSelectors.c.o' ¶
  ':WASTE 1.3:Source:WEUtilities.c.o' ¶

WECFMObj = ¶
  ':WASTE 1.3:Source:WEAccessors.c.CFMo' ¶
  ':WASTE 1.3:Source:WEBirthDeath.c.CFMo' ¶
  ':WASTE 1.3:Source:WEDebug.c.CFMo' ¶
  ':WASTE 1.3:Source:WEDrawing.c.CFMo' ¶
  ':WASTE 1.3:Source:WEFontTables.c.CFMo' ¶
  ':WASTE 1.3:Source:WEHighLevelEditing.c.CFMo' ¶
  ':WASTE 1.3:Source:WEICGlue.c.CFMo' ¶
  ':WASTE 1.3:Source:WEInlineInput.c.CFMo' ¶
  ':WASTE 1.3:Source:WELineLayout.c.CFMo' ¶
  ':WASTE 1.3:Source:WELongCoords.c.CFMo' ¶
  ':WASTE 1.3:Source:WELowLevelEditing.c.CFMo' ¶
  ':WASTE 1.3:Source:WEMouse.c.CFMo' ¶
  ':WASTE 1.3:Source:WEObjects.c.CFMo' ¶
  ':WASTE 1.3:Source:WEScraps.c.CFMo' ¶
  ':WASTE 1.3:Source:WESelecting.c.CFMo' ¶
  ':WASTE 1.3:Source:WESelectors.c.CFMo' ¶
  ':WASTE 1.3:Source:WEUserSelectors.c.CFMo' ¶
  ':WASTE 1.3:Source:WEUtilities.c.CFMo' ¶

WEPPCObj = ¶
  ':WASTE 1.3:Source:WEAccessors.c.x' ¶
  ':WASTE 1.3:Source:WEBirthDeath.c.x' ¶
  ':WASTE 1.3:Source:WEDebug.c.x' ¶
  ':WASTE 1.3:Source:WEDrawing.c.x' ¶
  ':WASTE 1.3:Source:WEFontTables.c.x' ¶
  ':WASTE 1.3:Source:WEHighLevelEditing.c.x' ¶
  ':WASTE 1.3:Source:WEICGlue.c.x' ¶
  ':WASTE 1.3:Source:WEInlineInput.c.x' ¶
  ':WASTE 1.3:Source:WELineLayout.c.x' ¶
  ':WASTE 1.3:Source:WELongCoords.c.x' ¶
  ':WASTE 1.3:Source:WELowLevelEditing.c.x' ¶
  ':WASTE 1.3:Source:WEMouse.c.x' ¶
  ':WASTE 1.3:Source:WEObjects.c.x' ¶
  ':WASTE 1.3:Source:WEScraps.c.x' ¶
  ':WASTE 1.3:Source:WESelecting.c.x' ¶
  ':WASTE 1.3:Source:WESelectors.c.x' ¶
  ':WASTE 1.3:Source:WEUserSelectors.c.x' ¶
  ':WASTE 1.3:Source:WEUtilities.c.x' ¶

WEIncl = -i ":WASTE 1.3:Private Includes:" ¶
         -i ":WASTE 1.3:Internet Config Headers:"

WETarg = WASTELib.o WASTELib.CFMo WASTELib.x

WASTELib.o Ä {WEObj}
  lib -o WASTELib.o {WEObj}

WASTELib.CFMo Ä {WECFMObj}
  lib -o WASTELib.CFMo {WECFMObj}

WASTELib.x Ä {WEPPCObj}
  ppclink -xm library -o WASTELib.x {WEPPCObj} -sym on

clean ÄÄ
  delete -i {WEObj} {WECFMObj} {WEPPCObj} {WETarg}


#### :Extras:Sample Object Handlers:

OHObj = ":Extras:Sample Object Handlers:WEObjectHandlers.c.o"
OHCFMObj = ":Extras:Sample Object Handlers:WEObjectHandlers.c.CFMo"
OHPPCObj = ":Extras:Sample Object Handlers:WEObjectHandlers.c.x"
OHIncl = -i ":Extras:Sample Object Handlers:"
OHTarg = {OHObj} {OHCFMObj} {OHPPCObj}

clean ÄÄ
  delete -i {OHTarg}


#### :Extras:WASTE Tabs 1.3.2:

WTObj = ¶
  ":Extras:WASTE Tabs 1.3.2:WETabs.c.o" ¶
  ":Extras:WASTE Tabs 1.3.2:WETabHooks.c.o"¶

WTCFMObj = ¶
  ":Extras:WASTE Tabs 1.3.2:WETabs.c.CFMo" ¶
  ":Extras:WASTE Tabs 1.3.2:WETabHooks.c.CFMo" ¶

WTPPCObj = ¶
  ":Extras:WASTE Tabs 1.3.2:WETabs.c.x" ¶
  ":Extras:WASTE Tabs 1.3.2:WETabHooks.c.x" ¶

WTIncl = -i ":Extras:Waste Tabs 1.3.2:"
WTTarg = {WTObj} {WTCFMObj} {WTPPCObj}

clean ÄÄ
  delete -i {WTTarg}


#### :Demo:Source:

WDObj = ¶
  :Demo:Source:DialogUtils.c.o ¶
  :Demo:Source:LongControls.c.o ¶
  ':Demo:Source:SmartScroll Stuff:SmartScroll.c.o' ¶
  :Demo:Source:WEDemoAbout.c.o ¶
  :Demo:Source:WEDemoDrags.c.o ¶
  :Demo:Source:WEDemoEvents.c.o ¶
  :Demo:Source:WEDemoFiles.c.o ¶
  :Demo:Source:WEDemoInit.c.o ¶
  :Demo:Source:WEDemoIntf.c.o ¶
  :Demo:Source:WEDemoMain.c.o ¶
  :Demo:Source:WEDemoMenus.c.o ¶
  :Demo:Source:WEDemoScripting.c.o ¶
  :Demo:Source:WEDemoWindows.c.o ¶
  :Demo:Source:qd.c.o ¶

WDLibs = WASTELib.o {WTObj} {OHObj}

WDCFMObj = ¶
  :Demo:Source:DialogUtils.c.CFMo ¶
  :Demo:Source:LongControls.c.CFMo ¶
  ':Demo:Source:SmartScroll Stuff:SmartScroll.c.CFMo' ¶
  :Demo:Source:WEDemoAbout.c.CFMo ¶
  :Demo:Source:WEDemoDrags.c.CFMo ¶
  :Demo:Source:WEDemoEvents.c.CFMo ¶
  :Demo:Source:WEDemoFiles.c.CFMo ¶
  :Demo:Source:WEDemoInit.c.CFMo ¶
  :Demo:Source:WEDemoIntf.c.CFMo ¶
  :Demo:Source:WEDemoMain.c.CFMo ¶
  :Demo:Source:WEDemoMenus.c.CFMo ¶
  :Demo:Source:WEDemoScripting.c.CFMo ¶
  :Demo:Source:WEDemoWindows.c.CFMo ¶
  :Demo:Source:qd.c.CFMo ¶

WDCFMLibs = WASTELib.CFMo {WTCFMObj} {OHCFMObj}

WDPPCObj = ¶
  :Demo:Source:DialogUtils.c.x ¶
  :Demo:Source:LongControls.c.x ¶
  ':Demo:Source:SmartScroll Stuff:SmartScroll.c.x' ¶
  :Demo:Source:WEDemoAbout.c.x ¶
  :Demo:Source:WEDemoDrags.c.x ¶
  :Demo:Source:WEDemoEvents.c.x ¶
  :Demo:Source:WEDemoFiles.c.x ¶
  :Demo:Source:WEDemoInit.c.x ¶
  :Demo:Source:WEDemoIntf.c.x ¶
  :Demo:Source:WEDemoMain.c.x ¶
  :Demo:Source:WEDemoMenus.c.x ¶
  :Demo:Source:WEDemoScripting.c.x ¶
  :Demo:Source:WEDemoWindows.c.x ¶
  :Demo:Source:qd.c.x ¶

WDPPCLibs = WASTELib.x {WTPPCObj} {OHPPCObj}

WDIncl = -i ":Demo:Source:" -i ":Demo:Source:SmartScroll Stuff:"

WDTarg = "WASTE Demo (classic)" "WASTE Demo (CFM)"

:Demo:Source:qd.c Ä
  echo "#include <QuickDraw.h>¶nQDGlobals qd;¶n" > :Demo:Source:qd.c

:Demo:Source:size.r Ä
  begin
    echo '#include "Types.r"'
    echo 'resource '¶''SIZE'¶'' (-1) {'
    echo 'reserved,'
    echo 'acceptSuspendResumeEvents,'
    echo 'reserved,'
    echo 'canBackground,'
    echo 'multiFinderAware,'
    echo 'backgroundAndForeground,'
    echo 'dontGetFrontClicks,'
    echo 'ignoreChildDiedEvents,'
    echo 'is32BitCompatible,'
    echo 'isHighLevelEventAware,'
    echo 'localAndRemoteHLEvents,'
    echo 'notStationeryAware,'
    echo 'dontUseTextEditServices,'
    echo 'reserved,'
    echo 'reserved,'
    echo 'reserved,'
    echo '262144,'
    echo '196608'
    echo '};'
  end > :Demo:Source:size.r

"WASTE Demo (classic)" ÄÄ {WDObj} {WDLibs}
  ilink -c OEDE {WDObj} {WDLibs} {Libs} -o "WASTE Demo (classic)" ¶
        -model far -compact -pad 0 -state nouse

"WASTE Demo (classic)" ÄÄ {WDPPCObj} {WDPPCLibs}
  ppclink -c OEDE {WDPPCObj} {WDPPCLibs} {PPCLibs} ¶
          -fragname 'WASTE Demo PPC' -sym on
  mergefragment -z PPCLink.out "WASTE Demo (classic)"
  delete -i PPCLink.out
  rename -y PPCLink.out.xcoff "WASTE Demo (classic).xcoff"

"WASTE Demo (classic)" ÄÄ :Demo:Source:WEDemo.rsrc :Demo:Source:size.r
  begin
    echo 'include ":Demo:Source:WEDemo.rsrc";'
    echo '#include ":Demo:Source:size.r"'
  end | rez -a -c OEDE -o "WASTE Demo (classic)"
  setfile -a Bi "WASTE Demo (classic)"

"WASTE Demo (CFM)" ÄÄ {WDCFMObj} {WDCFMLibs}
  ilink -c OEDE {WDCFMObj} {WDCFMLibs} {CFMLibs} -o "WASTE Demo (CFM)" ¶
        -model cfmseg -state nouse -fragname 'WASTE Demo 68k'

"WASTE Demo (CFM)" ÄÄ {WDPPCObj} {WDPPCLibs}
  ppclink -c OEDE {WDPPCObj} {WDPPCLibs} {PPCLibs} ¶
          -fragname 'WASTE Demo PPC' -sym on
  mergefragment -z PPCLink.out "WASTE Demo (CFM)"
  delete -i PPCLink.out
  rename -y PPCLink.out.xcoff "WASTE Demo (CFM).xcoff"

"WASTE Demo (CFM)" ÄÄ :Demo:Source:WEDemo.rsrc :Demo:Source:size.r
  begin
    echo 'include ":Demo:Source:WEDemo.rsrc";'
    echo '#include ":Demo:Source:size.r";'
  end | rez -a -c OEDE -o "WASTE Demo (CFM)"
  setfile -a Bi "WASTE Demo (CFM)"

clean ÄÄ
  delete -i {WDObj} {WDCFMObj} {WDPPCObj} {WDTarg}
  delete -i :Demo:Source:size.r :Demo:Source:qd.c
  delete -i "WASTE Demo (CFM).xcoff" "WASTE Demo (classic).xcoff"


#### Main target (continued)

everything Ä {OHTarg} {WETarg} {WDTarg}


#### Default rule for CFM-68k compilation.

.c.CFMo Ä .c
  {CFMC} {depdir}{default}.c -o {targdir}{default}.c.CFMo {CFMCOptions}


#### Dependencies

':Extras:Sample Object Handlers:WEObjectHandlers.c.o' ¶
':Extras:Sample Object Handlers:WEObjectHandlers.c.CFMo' ¶
':Extras:Sample Object Handlers:WEObjectHandlers.c.x' ¶
Ä ":Extras:Sample Object Handlers:WEObjectHandlers.h" ¶
  ":WASTE C/C++ Headers:WASTE.h" ¶
  ":WASTE C/C++ Headers:LongCoords.h"

':WASTE 1.3:Source:WEAccessors.c.o' ¶
':WASTE 1.3:Source:WEAccessors.c.CFMo' ¶
':WASTE 1.3:Source:WEAccessors.c.x' ¶
':WASTE 1.3:Source:WEBirthDeath.c.o' ¶
':WASTE 1.3:Source:WEBirthDeath.c.CFMo' ¶
':WASTE 1.3:Source:WEBirthDeath.c.x' ¶
':WASTE 1.3:Source:WEDebug.c.o' ¶
':WASTE 1.3:Source:WEDebug.c.CFMo' ¶
':WASTE 1.3:Source:WEDebug.c.x' ¶
':WASTE 1.3:Source:WEDrawing.c.o' ¶
':WASTE 1.3:Source:WEDrawing.c.CFMo' ¶
':WASTE 1.3:Source:WEDrawing.c.x' ¶
':WASTE 1.3:Source:WEFontTables.c.o' ¶
':WASTE 1.3:Source:WEFontTables.c.CFMo' ¶
':WASTE 1.3:Source:WEFontTables.c.x' ¶
':WASTE 1.3:Source:WEHighLevelEditing.c.o' ¶
':WASTE 1.3:Source:WEHighLevelEditing.c.CFMo' ¶
':WASTE 1.3:Source:WEHighLevelEditing.c.x' ¶
':WASTE 1.3:Source:WEICGlue.c.o' ¶
':WASTE 1.3:Source:WEICGlue.c.CFMo' ¶
':WASTE 1.3:Source:WEICGlue.c.x' ¶
':WASTE 1.3:Source:WEInlineInput.c.o' ¶
':WASTE 1.3:Source:WEInlineInput.c.CFMo' ¶
':WASTE 1.3:Source:WEInlineInput.c.x' ¶
':WASTE 1.3:Source:WELineLayout.c.o' ¶
':WASTE 1.3:Source:WELineLayout.c.CFMo' ¶
':WASTE 1.3:Source:WELineLayout.c.x' ¶
':WASTE 1.3:Source:WELongCoords.c.o' ¶
':WASTE 1.3:Source:WELongCoords.c.CFMo' ¶
':WASTE 1.3:Source:WELongCoords.c.x' ¶
':WASTE 1.3:Source:WELowLevelEditing.c.o' ¶
':WASTE 1.3:Source:WELowLevelEditing.c.CFMo' ¶
':WASTE 1.3:Source:WELowLevelEditing.c.x' ¶
':WASTE 1.3:Source:WEMouse.c.o' ¶
':WASTE 1.3:Source:WEMouse.c.CFMo' ¶
':WASTE 1.3:Source:WEMouse.c.x' ¶
':WASTE 1.3:Source:WEObjects.c.o' ¶
':WASTE 1.3:Source:WEObjects.c.CFMo' ¶
':WASTE 1.3:Source:WEObjects.c.x' ¶
':WASTE 1.3:Source:WEScraps.c.o' ¶
':WASTE 1.3:Source:WEScraps.c.CFMo' ¶
':WASTE 1.3:Source:WEScraps.c.x' ¶
':WASTE 1.3:Source:WESelecting.c.o' ¶
':WASTE 1.3:Source:WESelecting.c.CFMo' ¶
':WASTE 1.3:Source:WESelecting.c.x' ¶
':WASTE 1.3:Source:WESelectors.c.o' ¶
':WASTE 1.3:Source:WESelectors.c.CFMo' ¶
':WASTE 1.3:Source:WESelectors.c.x' ¶
':WASTE 1.3:Source:WEUserSelectors.c.o' ¶
':WASTE 1.3:Source:WEUserSelectors.c.CFMo' ¶
':WASTE 1.3:Source:WEUserSelectors.c.x' ¶
':WASTE 1.3:Source:WEUtilities.c.o' ¶
':WASTE 1.3:Source:WEUtilities.c.CFMo' ¶
':WASTE 1.3:Source:WEUtilities.c.x' ¶
Ä ":WASTE 1.3:Private Includes:WASTEIntf.h" ¶
  ":WASTE 1.3:Private Includes:LongCoords.h"

":WASTE 1.3:Source:WEMouse.c.o" ¶
":WASTE 1.3:Source:WEMouse.c.CFMo" ¶
":WASTE 1.3:Source:WEMouse.c.x" ¶
Ä ":Waste 1.3:Internet Config Headers:ICTypes.h" ¶
  ":Waste 1.3:Internet Config Headers:ICAPI.h"

":WASTE 1.3:Source:WEICGlue.c.o" ¶
":WASTE 1.3:Source:WEICGlue.c.CFMo" ¶
":WASTE 1.3:Source:WEICGlue.c.x" ¶
Ä ":Waste 1.3:Internet Config Headers:ICComponentSelectors.h" ¶
  ":Waste 1.3:Internet Config Headers:ICAPI.h"

:Demo:Source:DialogUtils.c.o ¶
:Demo:Source:DialogUtils.c.CFMo ¶
:Demo:Source:DialogUtils.c.x ¶
:Demo:Source:LongControls.c.o ¶
:Demo:Source:LongControls.c.CFMo ¶
:Demo:Source:LongControls.c.x ¶
:Demo:Source:WEDemoAbout.c.o ¶
:Demo:Source:WEDemoAbout.c.CFMo ¶
:Demo:Source:WEDemoAbout.c.x ¶
:Demo:Source:WEDemoDrags.c.o ¶
:Demo:Source:WEDemoDrags.c.CFMo ¶
:Demo:Source:WEDemoDrags.c.x ¶
:Demo:Source:WEDemoEvents.c.o ¶
:Demo:Source:WEDemoEvents.c.CFMo ¶
:Demo:Source:WEDemoEvents.c.x ¶
:Demo:Source:WEDemoFiles.c.o ¶
:Demo:Source:WEDemoFiles.c.CFMo ¶
:Demo:Source:WEDemoFiles.c.x ¶
:Demo:Source:WEDemoInit.c.o ¶
:Demo:Source:WEDemoInit.c.CFMo ¶
:Demo:Source:WEDemoInit.c.x ¶
:Demo:Source:WEDemoIntf.c.o ¶
:Demo:Source:WEDemoIntf.c.CFMo ¶
:Demo:Source:WEDemoIntf.c.x ¶
:Demo:Source:WEDemoMain.c.o ¶
:Demo:Source:WEDemoMain.c.CFMo ¶
:Demo:Source:WEDemoMain.c.x ¶
:Demo:Source:WEDemoMenus.c.o ¶
:Demo:Source:WEDemoMenus.c.CFMo ¶
:Demo:Source:WEDemoMenus.c.x ¶
:Demo:Source:WEDemoScripting.c.o ¶
:Demo:Source:WEDemoScripting.c.CFMo ¶
:Demo:Source:WEDemoScripting.c.x ¶
:Demo:Source:WEDemoWindows.c.o ¶
:Demo:Source:WEDemoWindows.c.CFMo ¶
:Demo:Source:WEDemoWindows.c.x ¶
Ä ":Demo:Source:WEDemoIntf.h" ¶
  ":WASTE C/C++ Headers:WASTE.h"

:Demo:Source:WEDemoInit.c.o ¶
:Demo:Source:WEDemoInit.c.CFMo ¶
:Demo:Source:WEDemoInit.c.x ¶
Ä ":Demo:Source:SmartScroll Stuff:SmartScroll.h" ¶
  ":Extras:Sample Object Handlers:WEObjectHandlers.h"

:Demo:Source:WEDemoMenus.c.o ¶
:Demo:Source:WEDemoMenus.c.CFMo ¶
:Demo:Source:WEDemoMenus.c.x ¶
Ä ":Extras:WASTE Tabs 1.3.2:WETabs.h"

:Demo:Source:WEDemoWindows.c.o ¶
:Demo:Source:WEDemoWindows.c.CFMo ¶
:Demo:Source:WEDemoWindows.c.x ¶
Ä ":WASTE C/C++ Headers:LongCoords.h" ¶
  ":Demo:Source:SmartScroll Stuff:SmartScroll.h"

':Demo:Source:SmartScroll Stuff:SmartScroll.c.o' ¶
':Demo:Source:SmartScroll Stuff:SmartScroll.c.CFMo' ¶
':Demo:Source:SmartScroll Stuff:SmartScroll.c.x' ¶
Ä ':Demo:Source:SmartScroll Stuff:SmartScroll.h'
