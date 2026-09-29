!include "MUI.nsh"


Name "FlexDLL"
OutFile "flexdll_setup.exe"
InstallDir "$PROGRAMFILES\flexdll"

!insertmacro MUI_PAGE_DIRECTORY

!insertmacro MUI_PAGE_INSTFILES
!define MUI_FINISHPAGE_NOAUTOCLOSE
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

section
setOutPath $INSTDIR
file "flexdll_install_files\*"
sectionEnd
