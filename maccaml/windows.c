/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission                    */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

WindowPtr winToplevel = NULL;
WindowPtr winGraphics = NULL;
long wintopfrontier = 0;

static WELineBreakUPP charBreakUPP;

/* WE hook for breaking line at char (not word) boundaries. */
static pascal StyledLineBreakCode CharBreak
   (Ptr pText, SInt32 textLength, SInt32 textStart, SInt32 textEnd,
    Fixed *textWidth, SInt32 *textOffset, WEHandle hWE)
{
  long base = textStart;
  long len = textEnd - textStart;
  long l = 0;
  long i;
  short w;
  short text_width = HiWord (*textWidth);

  while (len > 0){
    if (pText [base] == '\n'){
      *textOffset = base + 1;
      return smBreakWord;
    }

    l = len >= 128 ? 128 : len;
    for (i = 0; i < l; i++){
      if (pText [base + i] == '\n') l = i;
    }

    w = TextWidth (pText, base, l);
    if (w > text_width){
      short locs [129];
      long i;
      MeasureText (l, pText + base, (Ptr) locs);
      for (i = 0; i < l; i++){
        if (locs [i+1] > text_width) break;
      }
      *textOffset = base + i;
      return smBreakChar;
    }

    len -= l;
    base += l;
    text_width -= w;
  }
  *textOffset = base;
  *textWidth = FixRatio (text_width, 1);
  return smBreakOverflow;
}

static void UpdateToplevelRO (void)
{
  WEReference we = WinGetWE (winToplevel);
  long selstart, selend;

  Assert (we != NULL);
  WEGetSelection (&selstart, &selend, we);
  if (selstart >= wintopfrontier){
    WEFeatureFlag (weFReadOnly, weBitClear, we);
  }else{
    WEFeatureFlag (weFReadOnly, weBitSet, we);
  }
}

OSErr InitialiseWindows (void)
{
  charBreakUPP = NewWELineBreakProc (CharBreak);
  return noErr;
}

/* The window becomes active if [activate] is true,
   inactive if false.
*/
void WinActivateDeactivate (int activate, WindowPtr w)
{
  WStatusH st = WinGetStatus (w);
  WEHandle we = WinGetWE (w);
  VHSelect axis;
  GrafPtr savePort;

  if (st == NULL) return;

  GetPort (&savePort);
  SetPort (w);

  if (we != NULL){
    if (activate) WEActivate (we);   else   WEDeactivate (we);
  }
  for (axis = V; axis <= H; axis++){
    ControlHandle bar = (*st)->scrollbars[axis];
    if (bar != NULL){
      if (activate) ShowControl (bar);   else   HideControl (bar);
      /* We sometimes get an activate without any previous deactivate.
         In this case, ShowControl will do nothing, but the control
         still needs to be redrawn.  It will be done with the normal
         update mechanism.  In the normal case, the control will be
         drawn twice, but what the hell. */
      /* ValidRect (&(*bar)->contrlRect); */
    }
  }
  /* There seems to be a bug in DrawGrowIcon that makes it draw an icon
     for non-resizable windows when processing a suspend/resume event.
  */
  if (GetWVariant (w) != noGrowDocProc) DrawGrowIcon (w);

  SetPort (savePort);
}

void WinAdvanceTopFrontier (long length)
{
  wintopfrontier += length;
  UpdateToplevelRO ();
}

OSErr WinAllocStatus (WindowPtr w)
{
  WStatusH st = NULL;
  OSErr err;

  err = AllocHandle (sizeof (struct WStatus), (Handle *) &st);
  if (err != noErr) return err;
  HLock ((Handle) st);
  (*st)->kind = kWinUninitialised;
  (*st)->datarefnum = (*st)->resrefnum = -1;
  (*st)->canwritesel = 0;
  (*st)->dirty = 0;
  (*st)->basemodcount = 0;
  (*st)->hascontents = 0;
  (*st)->scrollbars [V] = NULL;
  (*st)->scrollbars [H] = NULL;
  /* XXX initialiser les rectangles */
  (*st)->line_height = 1;
  (*st)->we = NULL;
  HUnlock ((Handle) st);
  SetWRefCon (w, (long) st);
  return noErr;
}

void WinCloseGraphics (void)
{
  Rect r;
  GrafPtr saveport;
  
  Assert (winGraphics != NULL);

  GetPort (&saveport);
  SetPort (winGraphics);
  r = winGraphics->portRect;
  LocalToGlobalRect (&r);
  prefs.graphpos = r;
  SetPort (saveport);

  DisposeWindow (winGraphics);
  winGraphics = NULL;
}

void WinCloseToplevel (void)
{
  Rect r;
  GrafPtr saveport;

  if (winToplevel != NULL){
    GetPort (&saveport);
    SetPort (winToplevel);

    r = winToplevel->portRect;
    LocalToGlobalRect (&r);
    prefs.toppos = r;
    if (prefs.asksavetop){
      XXX ();
    }
    SetPort (saveport);
  }
  DisposeWindow (winToplevel);
  winToplevel = NULL;
}

void WinDoContentClick (EventRecord *event, WindowPtr w)
{
  int k = WinGetKind (w);
  int inback = !IsWindowHilited (w);

  switch (k){

  case kWinUnknown:
  case kWinAbout:
  case kWinClipboard:
    if (inback) SelectWindow (w);
    break;

  case kWinGraphics: {
    Point hitPt = event->where;
    GrafPtr saveport;

    GetPort (&saveport);
    SetPort (w);
    GlobalToLocal (&hitPt);
    if (inback){
      SelectWindow (w);
    }else{
      Rect r;
      ScrollCalcGraph (w, &r);
      if (PtInRect (hitPt, &r)){
        GraphGotEvent (event);
      }else{
        ScrollDoClick (w, hitPt, event->modifiers);
      }
    }
    SetPort (saveport);
    break;
  }

  case kWinToplevel:
  case kWinDocument: {
    int handleit = !inback;
    GrafPtr saveport;
    Point hitPt = event->where;
    WEReference we = WinGetWE (w);

    Assert (we != NULL);
    GetPort (&saveport);
    SetPort (w);
    GlobalToLocal (&hitPt);

    if (inback && gHasDragAndDrop){
      long selStart, selEnd;
      RgnHandle selRgn;

      WEGetSelection (&selStart, &selEnd, we);
      selRgn = WEGetHiliteRgn (selStart, selEnd, we);
      handleit = PtInRgn (hitPt, selRgn) && WaitMouseMoved (event->where);
      DisposeRgn (selRgn);
    }
    if (!handleit){
      SelectWindow (w);
    }else{
      Rect r;
      ScrollCalcText (w, &r);
      InsetRect (&r, -kTextMarginH, 0);
      if (PtInRect (hitPt, &r)){
        WEClick (hitPt, event->modifiers, event->when, we);
        if (w == winToplevel) UpdateToplevelRO ();
      }else{
        ScrollDoClick (w, hitPt, event->modifiers);
      }
    }
    SetPort (saveport);
    break;
  }

  default:
    Assert (0); /* There is no other window kind. */
    break;
  }
}

OSErr WinDoClose (ClosingOption close, WindowPtr w)
{
  int k = WinGetKind (w);
  OSErr err;
  WStatusH st;
  WEHandle we;

  switch (k){

  case kWinUnknown:
  case kWinToplevel:
  default:
    Assert (0);
    return noErr;

  case kWinAbout:
    CloseAboutBox (w);
    return noErr;

  case kWinGraphics:
    HideWindow (winGraphics);
    return noErr;

  case kWinDocument:
    err = FileDoClose (w, close);
    if (err != noErr) return err;
    st = WinGetStatus (w);   Assert (st != NULL);
    we = WinGetWE (w);       Assert (we != NULL);
    LCDetach ((*st)->scrollbars[V]);
    LCDetach ((*st)->scrollbars[H]);
    WEDispose (we);
    DisposeHandle ((Handle) st);
    MenuWinRemove (w);
    DisposeWindow (w);
    return noErr;

  case kWinClipboard:
    XXX ();
    return noErr;
  }
}

void WinDoDrag (Point where, WindowPtr w)
{
  Rect limits;

  limits = (*GetGrayRgn ())->rgnBBox;
  InsetRect (&limits, 4, 4);
  DragWindow (w, where, &limits);
  if (w == winGraphics) GraphNewSizePos ();
}

/* Invalidate the bottom and right margins. */
static void WinInvalMargins (WindowPtr w)
{
  Rect r;

  r = w->portRect;
  r.right -= kScrollBarWidth;
  r.left = r.right - kTextMarginH;
  r.bottom -= kScrollBarWidth;
  InvalRect (&r);
  r = w->portRect;
  r.bottom -= kScrollBarWidth;
  r.top = r.bottom - kTextMarginV;
  r.right -= kScrollBarWidth;
  InvalRect (&r);
}

static void WinGraphNewSize (WindowPtr w)
{
  Rect r;
  WStatusH st = WinGetStatus (w);
  
  Assert (st != NULL);
  ScrollCalcGraph (w, &r);
  WERectToLongRect (&r, &(*st)->viewrect);
}

static void WinWENewSize (WindowPtr w, WEReference we)
{
  Rect r;
  LongRect lr;

  ScrollCalcText (w, &r);
  WERectToLongRect (&r, &lr);
  WESetViewRect (&lr, we);
  WEGetDestRect (&lr, we);
  if (lr.right - lr.left != r.right - r.left){
    lr.right = lr.left + r.right - r.left;
    WESetDestRect (&lr, we);
    WECalText (we);
    InvalRect (&r);
  }
}

static void WinResize (WindowPtr w, short x, short y)
{
  GrafPtr saveport;
  WEReference we = WinGetWE (w);
  Rect r;

  GetPort (&saveport);
  SetPort (w);

  /* Invalidate the old grow icon and the text margin. */
  r = w->portRect;
  r.left = r.right - kScrollBarWidth;
  r.top = r.bottom - kScrollBarWidth;
  InvalRect (&r);
  if (we != NULL) WinInvalMargins (w);

  SizeWindow (w, x, y, true);

  /* Redraw the controls and invalidate whatever is needed. */
  if (we != NULL){
    WinWENewSize (w, we);
    WinInvalMargins (w);
  }
  if (w == winGraphics) WinGraphNewSize (w);
  ScrollNewSize (w);
  SetPort (saveport);
}

void WinDoGrow (Point where, WindowPtr w)
{
  Rect r;
  long newsize;
  short x, y;
  WStatusH st;

  switch (WinGetKind (w)){

  case kWinUnknown:
  case kWinAbout:
  case kWinPrefs:
    Assert (0);
    break;

  case kWinToplevel:
  case kWinDocument:
  case kWinClipboard:
    SetRect (&r, kMinWindowWidth, kMinWindowHeight, SHRT_MAX, SHRT_MAX);
    break;

  case kWinGraphics:
    st = WinGetStatus (w);
    Assert (st != NULL);
    x = (*st)->destrect.right - (*st)->destrect.left + kScrollBarWidth + 1;
    y = (*st)->destrect.bottom - (*st)->destrect.top + kScrollBarWidth + 1;
    SetRect (&r, kMinWindowWidth, kMinWindowHeight, x, y);
    break;
  }
  newsize = GrowWindow (w, where, &r);
  if (newsize != 0) WinResize (w, LoWord (newsize), HiWord (newsize));
}

void WinDoIdle (WindowPtr w)
{
  WEHandle we = WinGetWE (w);

  if (we != NULL) WEIdle (&evtSleep, we);   else   evtSleep = LONG_MAX;
}

void WinDoKey (WindowPtr w, short chr, EventRecord *e)
{
  WEReference we;
  long selstart, selend;

  switch (WinGetKind (w)){

  case kWinToplevel:
    we = WinGetWE (w);   Assert (we != NULL);
    WEGetSelection (&selstart, &selend, we);
    if (chr == charBackspace){
      if (selstart < wintopfrontier || selend == wintopfrontier) break;
    }
    if (chr == charEnter){
      long sel = WEGetTextLength (we);
      WESetSelection (sel, sel, we);
      chr = charReturn;
    }
    if (selstart == selend){
      WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
                  &prefs.unread, we);
    }
    /*XXX intercepter option-up/down, command-up/down, option-command-up/down */
    WEKey (chr, e->modifiers, we);
    UpdateToplevelRO ();
    break;

  case kWinDocument:
    we = WinGetWE (w);   Assert (we != NULL);
    if (chr == charEnter){
      XXX ();   /* XXX envoyer la phrase courante au toplevel */
    }
    /*XXX intercepter option-up/down, command-up/down, option-command-up/down
      -> myWEKey pour partager avec le toplevel */
    WEKey (chr, e->modifiers, we);
    break;

  case kWinGraphics:
    GraphGotEvent (e);
    break;

  case kWinAbout:
    CloseAboutBox (w);
    break;

  case kWinPrefs:
    XXX ();
    break;

  case kWinClipboard:
    break;

  default:
    Assert (0);
    break;
  }
}

void WinDoZoom (WindowPtr w, short partCode)
{
  XXX ();
}

/* Return a pointer to the window's descriptor record,
   NULL if there is none or w is NULL.
*/
WStatusH WinGetStatus (WindowPtr w)
{
  WStatusH st;
  short wk;

  if (w == NULL) return NULL;
  wk = GetWindowKind (w);
  if (wk != kApplicationWindowKind && wk != kDialogWindowKind) return NULL;
  st = (WStatusH) GetWRefCon (w);
  Assert (st != NULL);
  return st;
}

WEHandle WinGetWE (WindowPtr w)
{
  WStatusH st = WinGetStatus (w);

  if (st == NULL) return NULL;
  return (*st)->we;
}

int WinGetKind (WindowPtr w)
{
  WStatusH st = WinGetStatus (w);

  if (st == NULL) return kWinUnknown;
  return (*st)->kind;
}

/* Initialize all the data structures associated with a text
   window: WE record and scroll bars.
*/
static OSErr WinTextInit (WindowPtr w, TextStyle *style)
{
  OSErr err;
  WEReference we = NULL;
  WStatusH st = NULL;
  Rect viewrect;
  LongRect lviewrect, ldestrect;
  WERunInfo runinfo;
  int i;
  ControlHandle bar;

  err = WinAllocStatus (w);
  if (err != noErr) goto failed;

  st = WinGetStatus (w);  Assert (st != NULL);
  HLock ((Handle) st);

  ScrollCalcText (w, &viewrect);
  WERectToLongRect (&viewrect, &lviewrect);
  ldestrect = lviewrect;
  ldestrect.right = ldestrect.left + ktextwidth;
  err = WENew (&ldestrect, &lviewrect,
               weDoAutoScroll + weDoOutlineHilite + weDoUndo
               + weDoDragAndDrop + weDoUseTempMem + weDoDrawOffscreen
               + weDoMonoStyled,
               &we);
  if (err != noErr) goto failed;
  WESetAlignment (weFlushLeft, we);
  WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
              style, we);
  err = WESetInfo (weRefCon, &w, we);  Assert (err == noErr);
  err = WESetInfo (weScrollProc, &scrollFollowUPP, we);  Assert (err == noErr);
  err = WESetInfo (weLineBreakHook, &charBreakUPP, we);  Assert (err == noErr);
  /* XXX ajouter un hiliteDropAreaHook pour les marges asymetriques. */
  (*st)->we = we;

  WEGetRunInfo (0, &runinfo, we);
  (*st)->line_height = runinfo.runHeight;

  (*st)->scrollbars [H] = (*st)->scrollbars [V] = NULL;
  for (i = V; i <= H; i++){
    bar = GetNewControl (kScrollBarTemplate, w);
    if (bar == NULL){ err = memFullErr; goto failed; }
    err = LCAttach (bar);
    if (err != noErr) goto failed;
    (*st)->scrollbars [i] = bar;
  }

  HUnlock ((Handle) st);

  WinWENewSize (w, we);
  ScrollNewSize (w);

  return noErr;

  failed:
  if (we != NULL) WEDispose (we);
  if (st != NULL){
    if ((*st)->scrollbars [V] != NULL) LCDetach ((*st)->scrollbars[V]);
    if ((*st)->scrollbars [H] != NULL) LCDetach ((*st)->scrollbars[H]);
    DisposeHandle ((Handle) st);
  }
  return err;
}

/* Open a new empty document window.
   In case of failure, display an alert and return NULL.
*/
WindowPtr WinOpenDocument (StringPtr name)
{
  WStatusH st = NULL;
  WindowPtr w = NULL;
  OSErr err;

  w = GetNewCWindow (kDocumentWinTemplate, NULL, (WindowPtr) -1L);
  if (w == NULL){ err = memFullErr; goto failed; }

  SetWTitle (w, name);
  ShowWindow (w);
  SetPort (w);

  err = WinTextInit (w, &prefs.text);
  if (err != noErr) goto failed;

  st = WinGetStatus (w);  Assert (st != NULL);
  (*st)->kind = kWinDocument;
  (*st)->hascontents = 1;
  (*st)->canwritesel = 1;

  err = MenuWinAdd (w);
  if (err != noErr) goto failed;

  return w;

  failed:
    if (w != NULL) DisposeWindow (w);  /* Also deallocates the scroll bars. */
    ErrorAlertGeneric (err);
    return NULL;
}

OSErr WinOpenGraphics (long width, long height)
{
  WindowPtr w = NULL;
  WStatusH st = NULL;
  OSErr err;
  Rect r;
  int i;
  ControlHandle bar;
  long ww, hh;

  w = GetNewCWindow (kGraphicsWinTemplate, NULL, (WindowPtr) -1L);
  if (w == NULL){ err = memFullErr; goto failed; }

  /*XXX Calculer si la fenetre est hors de l'ecran -> stdstate */
  MoveWindow (w, prefs.graphpos.left, prefs.graphpos.top, false);
  ww = prefs.graphpos.right - prefs.graphpos.left;
  hh = prefs.graphpos.bottom - prefs.graphpos.top;
  if (ww > width + kScrollBarWidth) ww = width + kScrollBarWidth;
  if (hh > height + kScrollBarWidth) hh = height + kScrollBarWidth;
  SizeWindow (w, ww, hh, false);
  ShowWindow (w);
  SetPort (w);

  err = WinAllocStatus (w);
  if (err != noErr) goto failed;

  st = WinGetStatus (w);   Assert (st != NULL);
  HLock ((Handle) st);

  ScrollCalcGraph (w, &r);
  WERectToLongRect (&r, &(*st)->viewrect);
  r.right = r.left + width;
  r.bottom = r.top + height;
  WERectToLongRect (&r, &(*st)->destrect);
  st = WinGetStatus (w);  Assert (st != NULL);
  (*st)->kind = kWinGraphics;
  (*st)->hascontents = 1;

  (*st)->scrollbars [H] = (*st)->scrollbars [V] = NULL;
  for (i = V; i <= H; i++){
    bar = GetNewControl (kScrollBarTemplate, w);
    if (bar == NULL){ err = memFullErr; goto failed; }
    err = LCAttach (bar);
    if (err != noErr) goto failed;
    (*st)->scrollbars [i] = bar;
  }

  HUnlock ((Handle) st);

  ScrollNewSize (w);
  winGraphics = w;
  return noErr;

  failed:
    if (st != NULL){
      if ((*st)->scrollbars [V] != NULL) LCDetach ((*st)->scrollbars[V]);
      if ((*st)->scrollbars [H] != NULL) LCDetach ((*st)->scrollbars[H]);
      DisposeHandle ((Handle) st);
    }
    winGraphics = NULL;
    if (w != NULL) DisposeWindow (w);   /* Also deallocates the scroll bars. */
    return err;
}

OSErr WinOpenToplevel (void)
{
  WindowPtr w = NULL;
  WStatusH st = NULL;
  WEHandle we = NULL;
  OSErr err;

  /* Open the toplevel behind all other windows. */
  w = GetNewCWindow (kToplevelWinTemplate, NULL, NULL);
  if (w == NULL){ err = memFullErr; goto failed; }

  /*XXX Calculer si la fenetre est hors de l'ecran -> stdstate */
  MoveWindow (w, prefs.toppos.left, prefs.toppos.top, false);
  SizeWindow (w, prefs.toppos.right - prefs.toppos.left,
              prefs.toppos.bottom - prefs.toppos.top, false);
  ShowWindow (w);
  SetPort (w);

  err = WinTextInit (w, &prefs.unread);
  if (err != noErr) goto failed;

  st = WinGetStatus (w);  Assert (st != NULL);
  (*st)->kind = kWinToplevel;
  (*st)->hascontents = 1;

  we = WinGetWE (w);  Assert (we != NULL);
  WEFeatureFlag (weFUndo, weBitClear, we);
  WEFeatureFlag (weFMonoStyled, weBitClear, we);

  winToplevel = w;
  return noErr;

  failed:
    winToplevel = NULL;
    if (w != NULL) DisposeWindow (w);   /* Also deallocates the scroll bars. */
    ErrorAlertGeneric (err);
    return err;
}

void WinClipboardStdState (Rect *r)
{
  *r = (*GetGrayRgn ())->rgnBBox;
  r->bottom -= kWinBorderSpace;
  r->top = r->bottom - kMinWindowHeight;
  r->left += kWinBorderSpace;
  r->right -= 100;
}

void WinToplevelStdState (Rect *r)
{
  *r = (*GetGrayRgn ())->rgnBBox;
  r->top += kTitleBarSpace;
  r->bottom -= kPowerStripSpace;
  r->left += kWinBorderSpace;
  if (r->right > r->left + 506) r->right = r->left + 506;
}

void WinUpdate (WindowPtr w)
{
  int k = WinGetKind (w);
  WEHandle we = WinGetWE (w);
  GrafPtr saveport;
  RgnHandle updateRgn;

  Assert (k != kWinUnknown);

  GetPort (&saveport);
  SetPort (w);
  BeginUpdate (w);
  updateRgn = w->visRgn;
  if (!EmptyRgn (updateRgn)){
    EraseRgn (updateRgn);
    UpdateControls (w, updateRgn);
    DrawGrowIcon (w);
    if (k == kWinGraphics) GraphUpdate ();
    if (we != NULL) WEUpdate (updateRgn, we);
  }
  EndUpdate (w);
  SetPort (saveport);
}

void WinUpdateStatus (WindowPtr w)
{
  long selstart, selend, len;
  WStatusH st = WinGetStatus (w);
  WEHandle we = WinGetWE (w);

  if (st == NULL) return;
  switch ((*st)->kind){
  case kWinUnknown:
  case kWinUninitialised:
  case kWinAbout:
  case kWinPrefs:
  case kWinClipboard:
  case kWinGraphics:
    break;
  case kWinToplevel:
    Assert (we != NULL);
    WEGetSelection (&selstart, &selend, we);
    len = WEGetTextLength (we);
    (*st)->canwritesel = (selstart >= wintopfrontier);
    break;
  case kWinDocument:
    Assert (we != NULL);
    (*st)->dirty = ((*st)->basemodcount != WEGetModCount (we));
    break;
  default: Assert (0);
  }
}
