/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Developed by Jacob Navia, based on code by J-M Geffroy and X Leroy */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <math.h>
#include "mlvalues.h"
#include "alloc.h"
#include "libgraph.h"
#include "custom.h"
#include "memory.h"
#define UD(y)  (grwindow.height - 1 - y)
HDC gcMetaFile;
int grdisplay_mode;
int grremember_mode;
GR_WINDOW grwindow;

static void GetCurrentPosition(HDC hDC,POINT *pt)
{
	MoveToEx(hDC,0,0,pt);
	MoveToEx(hDC,pt->x,pt->y,0);
}

static value gr_draw_or_fill_arc(value *argv,int argc,BOOL fill);

CAMLprim value gr_plot(value vx, value vy)
{
	int x = Int_val(vx);
	int y = Int_val(vy);
	gr_check_open();
	//	gr_moveto(vx,vy);
	//	gr_lineto(Val_int(Int_val(vx)+1),vy);
	//	return Val_unit;
	if(grremember_mode)
		SetPixel(grwindow.gcBitmap, x, Wcvt(y),grwindow.CurrentColor);
	if(grdisplay_mode) {
		SetPixel(grwindow.gc, x, Wcvt(y),grwindow.CurrentColor);
	}
	//	gr_moveto(vx+1,vy);
	return Val_unit;
}

CAMLprim value gr_moveto(value vx, value vy)
{
	grwindow.grx = Int_val(vx);
	grwindow.gry = Int_val(vy);
	if(grremember_mode)
		MoveToEx(grwindow.gcBitmap,grwindow.grx,Wcvt(grwindow.gry),0);
	if (grdisplay_mode)
		MoveToEx(grwindow.gc,grwindow.grx,Wcvt(grwindow.gry),0);
	return Val_unit;
}

CAMLprim value gr_current_x(void)
{
	return Val_int(grwindow.grx);
}

CAMLprim value gr_current_y(void)
{
	return Val_int(grwindow.gry);
}

CAMLprim value gr_lineto(value vx, value vy)
{
	int x = Int_val(vx);
	int y = Int_val(vy);
	gr_check_open();
	SelectObject(grwindow.gc,grwindow.CurrentPen);
	SelectObject(grwindow.gcBitmap,grwindow.CurrentPen);
	if (grremember_mode)
		LineTo(grwindow.gcBitmap,x,Wcvt(y));
	if (grdisplay_mode)
		LineTo(grwindow.gc, x, Wcvt(y));
	grwindow.grx = x;
	grwindow.gry = y;
	return Val_unit;
}

CAMLprim value gr_draw_rect(value vx, value vy, value vw, value vh)
{
#if 0
	int x = Int_val(vx);
	int y = Int_val(vy);
	int w = Int_val(vw);
	int h = Int_val(vh);

	gr_check_open();
	if(grdisplay_mode) {
		Rectangle(grwindow.gc,x, Wcvt(y) , x+w, Wcvt(y+h));
	}
	if(grremember_mode) {
		Rectangle(grwindow.gcBitmap,x, Wcvt(y), x+w, Wcvt(h+y));
	}
	return Val_unit;
#else
	// (x,y)=top bottom
	int     x, y, w, h;
	POINT pt[5];
	x=Int_val(vx);
	y=Int_val(vy);
	w=Int_val(vw);
	h=Int_val(vh);

	//			pt[0].x = x;   pt[0].y = UD(y+h-1);
	pt[0].x = x;
	pt[0].y = Wcvt(y-1);
	pt[1].x = x+w;
	pt[1].y = pt[0].y;
	//			pt[2].x = pt[1].x;  pt[2].y = UD(y-1);
	pt[2].x = pt[1].x;
	pt[2].y = Wcvt(y+h-1);
	pt[3].x = pt[0].x;
	pt[3].y = pt[2].y;
	pt[4].x = pt[0].x;
	pt[4].y = pt[0].y;
	if (grremember_mode) {
		Polyline(grwindow.gcBitmap,pt, 5);
	}
	if (grdisplay_mode) {
		Polyline(grwindow.gc,pt, 5);
	}
	return Val_unit;
#endif
}

CAMLprim value gr_draw_text(value text,value x)
{
	POINT pt;
	int oldmode = SetBkMode(grwindow.gc,TRANSPARENT);
	SetBkMode(grwindow.gcBitmap,TRANSPARENT);
	SetTextAlign(grwindow.gcBitmap, TA_UPDATECP|TA_BOTTOM);
	SetTextAlign(grwindow.gc, TA_UPDATECP|TA_BOTTOM);
	if (grremember_mode) {
		TextOut(grwindow.gcBitmap,0,0,(char *)text,x);
	}
	if(grdisplay_mode) {
		TextOut(grwindow.gc,0,0,(char *)text,x);
	}
	GetCurrentPosition(grwindow.gc,&pt);
	grwindow.grx = pt.x;
	grwindow.gry = grwindow.height - pt.y;
	SetBkMode(grwindow.gc,oldmode);
	SetBkMode(grwindow.gcBitmap,oldmode);
	return Val_unit;
}

CAMLprim value gr_fill_rect(value vx, value vy, value vw, value vh)
{
	int x = Int_val(vx);
	int y = Int_val(vy);
	int w = Int_val(vw);
	int h = Int_val(vh);
	RECT rc;

	gr_check_open();
	rc.left = x;
	rc.top = Wcvt(y);
	rc.right = x+w;
	rc.bottom = Wcvt(y)-h;
	if (grdisplay_mode)
		FillRect(grwindow.gc,&rc,grwindow.CurrentBrush);
	if (grremember_mode)
		FillRect(grwindow.gcBitmap,&rc,grwindow.CurrentBrush);
	return Val_unit;
}

CAMLprim value gr_sound(value freq, value vdur)
{
	Beep(freq,vdur);
	return Val_unit;
}

CAMLprim value gr_point_color(value vx, value vy)
{
	int x = Int_val(vx);
	int y = Int_val(vy);
	COLORREF rgb;
	unsigned long b,g,r;

	gr_check_open();
	rgb = GetPixel(grwindow.gcBitmap,x,Wcvt(y));
	b = (unsigned long)((rgb & 0xFF0000) >> 16);
	g = (unsigned long)((rgb & 0x00FF00) >> 8);
	r = (unsigned long)(rgb & 0x0000FF);
	return Val_long((r<<16) + (g<<8) + b);
}

CAMLprim value gr_circle(value x,value y,value radius)
{
	int left,top,right,bottom;

	gr_check_open();
	left = x - radius/2;
	top = Wcvt(y) - radius/2;
	right = left+radius;
	bottom = top+radius;
	Ellipse(grwindow.gcBitmap,left,top,right,bottom);
	return Val_unit;
}

CAMLprim value gr_set_window_title(value text)
{
	SetWindowText(grwindow.hwnd,(char *)text);
	return Val_unit;
}

CAMLprim value gr_draw_arc(value *argv ,int argc)
{
	return gr_draw_or_fill_arc(argv,argc,FALSE);
}

CAMLprim value gr_set_line_width(value vwidth)
{
	int width = Int_val(vwidth);
	HPEN oldPen,newPen;

	gr_check_open();
	oldPen = grwindow.CurrentPen;
	newPen = CreatePen(PS_SOLID,width,grwindow.CurrentColor);
	SelectObject(grwindow.gcBitmap,newPen);
	SelectObject(grwindow.gc,newPen);
	DeleteObject(oldPen);
	grwindow.CurrentPen = newPen;
	return Val_unit;
}

CAMLprim value gr_set_color(value vcolor)
{
	HBRUSH oldBrush, newBrush;
	LOGBRUSH lb;
	LOGPEN pen;
	HPEN newPen;
	int color = Long_val(vcolor);

	int  r = (color & 0xFF0000) >> 16,
	g = (color & 0x00FF00) >> 8 ,
	b =  color & 0x0000FF;
	COLORREF c = RGB(r,g,b);
	memset(&lb,0,sizeof(lb));
	memset(&pen,0,sizeof(LOGPEN));
	gr_check_open();
	GetObject(grwindow.CurrentPen,sizeof(LOGPEN),&pen);
	pen.lopnColor = c;
	newPen = CreatePenIndirect(&pen);
	SelectObject(grwindow.gcBitmap,newPen);
	SelectObject(grwindow.gc,newPen);
	DeleteObject(grwindow.CurrentPen);
	grwindow.CurrentPen = newPen;
	SetTextColor(grwindow.gc,c);
	SetTextColor(grwindow.gcBitmap,c);
	//	printf("gr_set_color, vcolor = %x, lbColor = %x\n", Int_val(vcolor), lb.lbColor);
	oldBrush = grwindow.CurrentBrush;
	lb.lbStyle = BS_SOLID;
	lb.lbColor = c;
	newBrush = CreateBrushIndirect(&lb);
	SelectObject(grwindow.gc,newBrush);
	SelectObject(grwindow.gcBitmap,newBrush);
	DeleteObject(oldBrush);
	grwindow.CurrentBrush = newBrush;
	grwindow.CurrentColor = c;
	return Val_unit;
}


static value gr_draw_or_fill_arc(value *argv,int argc,BOOL fill)
{
	int x, y, r_x, r_y, start, end;
	int     x1, y1, x2, y2, x3, y3, x4, y4;
	double cvt = 3.141592653/180.0;
	//		HPEN newPen = CreatePen(PS_SOLID,1,grwindow.CurrentColor);
	HPEN oldPen;

	r_x = Int_val(argv[2]);
	r_y = Int_val(argv[3]);
	if ((r_x < 0) || (r_y < 0))
		invalid_argument("draw_arc: radius must be positive");
	x     = Int_val(argv[0]);
	y     = Int_val(argv[1]);
	start = Int_val(argv[4]);
	end   = Int_val(argv[5]);

	// Upper-left corner of bounding rect.
	x1=     x - r_x;
	y1=     y + r_y;
	// Lower-right corner of bounding rect.
	x2=     x + r_x;
	y2=     y - r_y;
	// Starting point
	x3=x + (int)(100.0*cos(cvt*start));
	y3=y + (int)(100.0*sin(cvt*start));
	// Ending point
	x4=x + (int)(100.0*cos(cvt*end));
	y4=y + (int)(100.0*sin(cvt*end));

	if (grremember_mode) {
		oldPen = SelectObject(grwindow.gcBitmap,grwindow.CurrentPen);
		SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
		if( fill )
			Pie(grwindow.gcBitmap,x1, UD(y1), x2, UD(y2),
				x3, UD(y3), x4, UD(y4));
		else
			Arc(grwindow.gcBitmap,x1, UD(y1), x2, UD(y2),
				x3, UD(y3), x4, UD(y4));
		//			SelectObject(grwindow.gcBitmap,oldPen);
	}
	if( grdisplay_mode ) {
		oldPen = SelectObject(grwindow.gc,grwindow.CurrentPen);
		SelectObject(grwindow.gc,grwindow.CurrentBrush);
		if (fill)
			Pie(grwindow.gc,x1, UD(y1), x2, UD(y2),
				x3, UD(y3), x4, UD(y4));
		else
			Arc(grwindow.gc,x1, UD(y1), x2, UD(y2),
				x3, UD(y3), x4, UD(y4));
		//			SelectObject(grwindow.gc,oldPen);
	}
	//		DeleteObject(newPen);
	return Val_unit;
}

CAMLprim value gr_show_bitmap(value filename,int x,int y)
{
	AfficheBitmap(filename,grwindow.gcBitmap,x,Wcvt(y));
	AfficheBitmap(filename,grwindow.gc,x,Wcvt(y));
	return Val_unit;
}



CAMLprim value gr_get_mousex(void)
{
	POINT pt;
	GetCursorPos(&pt);
	MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
	return pt.x;
}

CAMLprim value gr_get_mousey(void)
{
	POINT pt;
	GetCursorPos(&pt);
	MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
	return grwindow.height - pt.y - 1;
}


static void gr_font(char *fontname)
{
	HFONT hf = CreationFont(fontname);

	if (hf && hf != INVALID_HANDLE_VALUE) {
		HFONT oldFont = SelectObject(grwindow.gc,hf);
		SelectObject(grwindow.gcBitmap,hf);
		DeleteObject(grwindow.CurrentFont);
		grwindow.CurrentFont = hf;
	}
}

CAMLprim value gr_set_font(value fontname)
{
	gr_check_open();
	gr_font(String_val(fontname));
	return Val_unit;
}

CAMLprim value gr_set_text_size (value sz)
{
	return Val_unit;
}

CAMLprim value gr_draw_char(value chr)
{
	char str[1];
	gr_check_open();
	str[0] = Int_val(chr);
	gr_draw_text((value)str, 1);
	return Val_unit;
}

CAMLprim value gr_draw_string(value str)
{
	gr_check_open();
	gr_draw_text(str, string_length(str));
	return Val_unit;
}

CAMLprim value gr_text_size(value str)
{
	SIZE extent;
	value res;

	mlsize_t len = string_length(str);
	if (len > 32767) len = 32767;

	GetTextExtentPoint(grwindow.gc,String_val(str), len,&extent);

	res = alloc_tuple(2);
	Field(res, 0) = Val_long(extent.cx);
	Field(res, 1) = Val_long(extent.cy);

	return res;
}

#if 0
static unsigned char gr_queue[SIZE_QUEUE];
static int gr_head = 0;       /* position of next read */
static int gr_tail = 0;       /* position of next write */

#define QueueIsEmpty (gr_head == gr_tail)
#define QueueIsFull  (gr_head == gr_tail + 1)

void gr_enqueue_char(unsigned char c)
{
	if (QueueIsFull) return;
	gr_queue[gr_tail] = c;
	gr_tail++;
	if (gr_tail >= SIZE_QUEUE) gr_tail = 0;
}
#endif

#define Button_down		1
#define Button_up		2
#define Key_pressed		4
#define Mouse_motion	8
#define Poll			16
int InspectMessages;
MSG msg;

CAMLprim value gr_wait_event(value eventlist)
{
	value res;
	int mask;
	BOOL poll;
	int mouse_x, mouse_y, button, key;
	int root_x, root_y, win_x, win_y;
	int r,i,stop;
	unsigned int modifiers;
	POINT pt;
	unsigned char keystate[256];

	gr_check_open();
	mask = 0;
	poll = FALSE;
	while (eventlist != Val_int(0)) {
		switch (Int_val(Field(eventlist,0))) {
		case 0:                     /* Button_down */
			mask |= Button_down;
			break;
		case 1:                     /* Button_up */
			mask |= Button_up;
			break;
		case 2:                     /* Key_pressed */
			mask |= Key_pressed;
			break;
		case 3:                     /* Mouse_motion */
			mask |= Mouse_motion;
			break;
		case 4:                     /* Poll */
			poll = TRUE;
			break;
		}
		eventlist = Field(eventlist,1);
	}
	mouse_x = -1;
	mouse_y = -1;
	button = 0;
	key = -1;

	if (poll) {
		// Poll uses peek message
		r = 0;
		if (mask & Button_down) {
			r |= PeekMessage(&msg,grwindow.hwnd,WM_LBUTTONDOWN,WM_LBUTTONDOWN,PM_REMOVE);
			if (r)
				button = 1;
			else {
				r |= PeekMessage(&msg,grwindow.hwnd,WM_MBUTTONDOWN,WM_LBUTTONDOWN,PM_REMOVE);
				if (r)
					button = 2;
				else {
					r |= PeekMessage(&msg,grwindow.hwnd,WM_RBUTTONDOWN,WM_LBUTTONDOWN,PM_REMOVE);
				}
				if (r)
					button = 3;
			}
		}
		if (mask & Button_up) {
			r |= PeekMessage(&msg,grwindow.hwnd,WM_LBUTTONUP,WM_LBUTTONUP,PM_REMOVE);
			if (r)
				button = 1;
			else {
				r |= PeekMessage(&msg,grwindow.hwnd,WM_MBUTTONUP,WM_LBUTTONUP,PM_REMOVE);
				if (r)
					button = 2;
				else {
					r |= PeekMessage(&msg,grwindow.hwnd,WM_RBUTTONUP,WM_LBUTTONUP,PM_REMOVE);
				}
				if (r)
					button = 3;
			}
		}
		if (mask & Mouse_motion) {
			r |= PeekMessage(&msg,grwindow.hwnd,WM_MOUSEMOVE,WM_MOUSEMOVE,PM_REMOVE);
		}
		if (r) {
			pt = msg.pt;
			MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
			mouse_x = pt.x;
			mouse_y = grwindow.height-pt.y;
		}
		if (mask & Key_pressed) {
			r = PeekMessage(&msg,grwindow.hwnd,WM_KEYFIRST,WM_KEYLAST,PM_REMOVE);
			if (r) {
				GetKeyboardState(keystate);
				for (i=0; i<256;i++) {
					if (keystate[i]&(~1)) {
						key = i;
						break;
					}
				}
			}
		}
	}
	else { // Not polled. Block for a message
		InspectMessages = 1;
		while (1) {
			WaitForSingleObject(EventHandle,INFINITE);
			stop = 0;
			if (msg.message == WM_LBUTTONDOWN && (mask&Button_down)) {
				stop = 1;
				button = 1;
			}
			if (msg.message == WM_MBUTTONDOWN && (mask&Button_down)) {
				stop = 1;
				button = 2;
			}
			if (msg.message == WM_RBUTTONDOWN && (mask&Button_down)) {
				stop = 1;
				button = 3;
			}
			if (mask&Button_up) {
				if (msg.message == WM_LBUTTONUP) {
					stop = 1;
					button = 1;
				}
				if (msg.message == WM_MBUTTONUP) {
					stop = 1;
					button = 2;
				}
				if (msg.message == WM_RBUTTONUP) {
					stop = 1;
					button = 3;
				}
			}
			if (mask&Mouse_motion) {
				if (msg.message == WM_MOUSEMOVE) {
					stop = 1;
				}
			}
			if (mask&Key_pressed) {
				if (msg.message >= WM_KEYFIRST && msg.message <= WM_KEYLAST) {
					stop = 1;
					GetKeyboardState(keystate);
					for (i=0; i<256;i++) {
						if (keystate[i]&(~1)) {
							key = i;
							break;
						}
					}
				}
			}
			if (stop) {
				pt = msg.pt;
				MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
				mouse_x = pt.x;
				mouse_y = grwindow.height- 1 - pt.y;
				break;
			}
			if (msg.message == WM_CLOSE)
				break;
		}
		InspectMessages = 0;
	}
	res = alloc_small(5, 0);
	Field(res, 0) = Val_int(mouse_x);
	Field(res, 1) = Val_int(mouse_y);
	Field(res, 2) = Val_bool(button);
	Field(res, 3) = Val_bool(key != -1);
	Field(res, 4) = Val_int(key & 0xFF);
	return res;
}

CAMLprim value gr_fill_poly(value vect)
{
	int n_points, i;
	POINT   *p,*poly;
	n_points = Wosize_val(vect);
	if (n_points < 3)
		gr_fail("fill_poly: not enough points",0);

	poly = (POINT *)malloc(n_points*sizeof(POINT));

	p = poly;
	for( i = 0; i < n_points; i++ ){
		p->x = Int_val(Field(Field(vect,i),0));
		p->y = UD(Int_val(Field(Field(vect,i),1)));
		p++;
	}
	if (grremember_mode) {
		SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
		Polygon(grwindow.gcBitmap,poly,n_points);
	}
	if (grdisplay_mode) {
		SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
		Polygon(grwindow.gc,poly,n_points);
	}
	free(poly);

	return Val_unit;
}

CAMLprim value gr_fill_arc(value *argv,int argc)
{
	return gr_draw_or_fill_arc(argv,argc,TRUE);
}

// Image primitives
struct image {
	int w;
	int h;
	HBITMAP data;
	HBITMAP mask;
};

#define Width(i) (((struct image *)Data_custom_val(i))->w)
#define Height(i) (((struct image *)Data_custom_val(i))->h)
#define Data(i) (((struct image *)Data_custom_val(i))->data)
#define Mask(i) (((struct image *)Data_custom_val(i))->mask)
//(1280x1024)
#define Max_image_mem 53000000 

static void finalize_image (value i)
{
	free (Data(i));
	if (Mask(i) != NULL) free(Mask(i));
}

static struct custom_operations image_ops = {
	"_image",
	finalize_image,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default
};

CAMLprim value gr_create_image(value w,value h)
{
	HBITMAP cbm;
	value res;

	if (Int_val (w) < 0 || Int_val (h) < 0)
		gr_fail("create_image: width and height must be positive",0);

	cbm = CreateCompatibleBitmap(grwindow.gcBitmap, Int_val(w), Int_val(h));
	res = alloc_custom(&image_ops, sizeof(struct image),
		w * h, Max_image_mem);
	if (res) {
		Width (res) = Int_val(w);
		Height (res) = Int_val(h);
		Data (res) = cbm;
		Mask (res) = NULL;
	}
	return res;
}

CAMLprim value gr_blit_image (value i, value x, value y)
{
	HBITMAP oldBmp = SelectObject(grwindow.tempDC,Data(i));
	int xsrc = Int_val(x);
	int ysrc = UD(Int_val(y) + Height(i) - 1);
	BitBlt(grwindow.tempDC,0, 0, Width(i), Height(i),
		grwindow.gcBitmap, xsrc, ysrc, SRCCOPY);
	SelectObject(grwindow.tempDC,oldBmp);
	return Val_unit;
}


CAMLprim value gr_draw_image(value i, value x, value y)
{
	HBITMAP oldBmp;

	int xdst = Int_val(x);
//	int ydst = UD(Int_val(y) + Height(i) - 1);
	int ydst = Wcvt(Int_val(y)+Height(i)-1);
	if (Mask(i) == NULL) {
		if (grremember_mode) {
			oldBmp = SelectObject(grwindow.tempDC,Data(i));
			BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCCOPY);
			SelectObject(grwindow.tempDC,oldBmp);
		}
		if (grdisplay_mode) {
			oldBmp = SelectObject(grwindow.tempDC,Data(i));
			BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCCOPY);
			SelectObject(grwindow.tempDC,oldBmp);
		}
	}
	else {
		if (grremember_mode) {
			oldBmp = SelectObject(grwindow.tempDC,Mask(i));
			BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCAND);
			SelectObject(grwindow.tempDC,Data(i));
			BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCPAINT);
			SelectObject(grwindow.tempDC,oldBmp);
		}
		if (grdisplay_mode) {
			oldBmp = SelectObject(grwindow.tempDC,Mask(i));
			BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCAND);
			SelectObject(grwindow.tempDC,Data(i));
			BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
				grwindow.tempDC, 0, 0, SRCPAINT);
			SelectObject(grwindow.tempDC,oldBmp);
		}
	}

	return Val_unit;
}

CAMLprim value gr_make_image(value matrix)
{
	int width, height,has_transp,i,j;
	value img;
	HBITMAP oldBmp;
	height = Wosize_val(matrix);
	if (height == 0) {
		width = 0;
	}
	else {
		width = Wosize_val(Field(matrix, 0));
		for (i = 1; i < height; i++) {
			if (width != (int) Wosize_val(Field(matrix, i)))
				gr_fail("make_image: non-rectangular matrix",0);
		}
	}
	Begin_roots1(matrix)
		img = gr_create_image(Val_int(width), Val_int(height));
	End_roots();
	has_transp = 0;
	oldBmp = SelectObject(grwindow.tempDC,Data(img));
	for (i = 0; i < height; i++) {
		for (j = 0; j < width; j++) {
			int col = Long_val (Field (Field (matrix, i), j));
			if (col == -1){
				has_transp = 1;
				SetPixel(grwindow.tempDC,j, i, 0);
			}
			else {
				int red = (col >> 16) & 0xFF;
				int green = (col >> 8) & 0xFF;
				int blue = col & 0xFF;
				SetPixel(grwindow.tempDC,j, i, RGB(red, green, blue));
			}
		}
	}
	SelectObject(grwindow.tempDC,oldBmp);
	if (has_transp) {
		HBITMAP  cbm;
		cbm = CreateCompatibleBitmap(grwindow.gc, width, height);
		Mask(img) = cbm;
		oldBmp = SelectObject(grwindow.tempDC,Mask(img));
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				int col = Long_val (Field (Field (matrix, i), j));
				SetPixel(grwindow.tempDC,j, i, col == -1 ? 0xFFFFFF : 0);
			}
		}
		SelectObject(grwindow.tempDC,oldBmp);
	}
	return img;
}

static value alloc_int_vect(mlsize_t size)
{
	value res;
	mlsize_t i;

	if (size == 0) return Atom(0);
	if (size <= Max_young_wosize) {
		res = alloc(size, 0);
	}
	else {
		res = alloc_shr(size, 0);
	}
	for (i = 0; i < size; i++) {
		Field(res, i) = Val_long(0);
	}
	return res;
}

CAMLprim value gr_dump_image (value img)
{
	int height = Height(img);
	int width = Width(img);
	value matrix = Val_unit;
	int i, j;
	HBITMAP oldBmp;

	Begin_roots2(img, matrix)
		matrix = alloc_int_vect (height);
	for (i = 0; i < height; i++) {
		modify (&Field (matrix, i), alloc_int_vect (width));
	}
	End_roots();

	oldBmp = SelectObject(grwindow.tempDC,Data(img));
	for (i = 0; i < height; i++) {
		for (j = 0; j < width; j++) {
			int col = GetPixel(grwindow.tempDC,j, i);
			int blue = (col >> 16) & 0xFF;
			int green = (col >> 8) & 0xFF;
			int red = col & 0xFF;
			Field(Field(matrix, i), j) = Val_long((red << 16) +
					(green << 8) + blue);
		}
	}
	SelectObject(grwindow.tempDC,oldBmp);
	if (Mask(img) != NULL) {
		oldBmp = SelectObject(grwindow.tempDC,Mask(img));
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				if (GetPixel(grwindow.tempDC,j, i) != 0)
					Field(Field(matrix, i), j) =
						Val_long(-1);
			}
		}
		SelectObject(grwindow.tempDC,oldBmp);
	}
	return matrix;
}
