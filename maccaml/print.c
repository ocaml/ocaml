/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

static short (*get_npages) (THPrint printrec);
static short (*draw_page) (THPrint printrec, TPPrPort port, int pagenum);

static THPrint curjobprintrec = NULL;

/*
  dojobdialog = 0 -> no job dialog (use default settings)
  dojobdialog = 1 -> use job dialog
  dojobdialog = 2 -> no job dialog (use previous dialog settings)
*/
static short print_loop (int dojobdialog, THPrint docprintrec)
{
  short ncopies, fstpage, lstpage, npages;
  OSErr err;
  GrafPtr saveport;
  TPPrPort printerport;
  TPrStatus prstatus;
  int copy, page, pgrun;

  GetPort (&saveport);

  PrOpen ();
  err = PrError (); if (err != noErr) goto failed_PrOpen;

  PrValidate (docprintrec);
  err = PrError (); if (err != noErr) goto failed_PrValidate;
  
  npages = (*get_npages) (docprintrec);
  switch (dojobdialog){
  case 0:
    if (curjobprintrec != NULL) DisposeHandle ((Handle) curjobprintrec);
    curjobprintrec = (THPrint) NewHandle (sizeof (TPrint));
    if (curjobprintrec == NULL) goto failed_alloc_curjobprintrec;
    PrintDefault (curjobprintrec);
    PrJobMerge (curjobprintrec, docprintrec);
    break;
  case 1:
    err = PrJobDialog (docprintrec);
    if (err) goto failed_PrJobDialog;
    if (curjobprintrec != NULL) DisposeHandle ((Handle) curjobprintrec);
    curjobprintrec = docprintrec;
    HandToHand ((Handle *) &curjobprintrec);
    if (curjobprintrec == NULL) goto failed_alloc_curjobprintrec;
    break;
  case 2:
    PrJobMerge (curjobprintrec, docprintrec);
    break;
  }
  ncopies = (*docprintrec)->prJob.iCopies;
  fstpage = (*docprintrec)->prJob.iFstPage;
  lstpage = (*docprintrec)->prJob.iLstPage;
  if (lstpage > npages) lstpage = npages;
  
  /* XXX Should display a status dialog box and use a IdleProc function */

  for (copy = 0; copy < ncopies; copy++){
    printerport = PrOpenDoc (docprintrec, NULL, NULL);
    err = PrError (); if (err != noErr) goto failed_PrOpenDoc;
    pgrun = 0;
    for (page = fstpage; page <= lstpage; page++){
      if (pgrun >= iPFMaxPgs){
        PrCloseDoc (printerport);
        err = PrError (); if (err != noErr) goto failed_PrCloseDoc;
        if ((*docprintrec)->prJob.bJDocLoop == bSpoolLoop){
          PrPicFile (docprintrec, NULL, NULL, NULL, &prstatus);
        }
        printerport = PrOpenDoc (docprintrec, NULL, NULL);
        err = PrError (); if (err != noErr) goto failed_PrOpenDoc;
        pgrun = 0;
      }
      PrOpenPage (printerport, NULL);
      err = PrError (); if (err != noErr) goto failed_PrOpenPage;
      err = (*draw_page) (docprintrec, printerport, page);
      if (err != noErr) goto failed_draw_page;
      PrClosePage (printerport);
      ++ pgrun;
    }
    PrCloseDoc (printerport);
    err = PrError (); if (err != noErr) goto failed_PrCloseDoc;
    if ((*docprintrec)->prJob.bJDocLoop == bSpoolLoop){
      PrPicFile (docprintrec, NULL, NULL, NULL, &prstatus);
    }
  }
  PrClose ();
  /*XXX close status dialog box here */
  SetPort (saveport);
  return noErr;

  failed_draw_page:
    PrClosePage (printerport);
    /* fall through */
  failed_PrOpenPage:
    PrCloseDoc (printerport);
    /* fall through */
  failed_PrOpenDoc:
  failed_PrCloseDoc:
  failed_alloc_curjobprintrec:
  failed_PrJobDialog:
  failed_PrValidate:
    PrClose ();
    /* fall through */
  failed_PrOpen:
    return err;
}

void FilePageSetup (void)
{
  XXX ();
}

void FilePrint (void)
{
  XXX ();
}
