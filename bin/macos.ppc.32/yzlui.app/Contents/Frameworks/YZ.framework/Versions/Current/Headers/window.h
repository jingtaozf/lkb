#ifndef	YZ_WINDOW_H
#define	YZ_WINDOW_H

#include	<yz/draw.h>

typedef	struct	window_t
{
	unsigned long	wseq;			// window ID number
	unsigned long	res1;			// unused
	char			name[64];		// the window's name
	unsigned short	width, height;	// the window's height and width
	
	pen_t	*pen;			// the current pen
	font_t	*font;			// the current font
	
	/* implementation specific data may be stored here */
	/* possibly including an event queue */
	void			*impl_data;

	int				origin_x, origin_y;
}	window_t;

enum
{
	YZ_IDLE	=	0,	// nothing happened
	YZ_KEY_DOWN	=	1,	// a key was pressed; “key" is filled out.
	YZ_KEY_UP	=	3,	// a key was released; "key" is filled out.
	// mouse events - each of these fills out “button” and “mouse”, too:
	YZ_MOUSE_DOWN =	6,	// the mouse was pushed; “x,” and “y” are filled out.
	YZ_MOUSE_DRAG =	7,	// drag while clicked; “x,” “x2,” “y”, “y2” `````
	YZ_MOUSE_UP	=	8,	// mouse click ended; “x,” and “y” are filled out.
	// focus events
	YZ_WINDOW_ACTIVATE = 20,	// this window has kbd focus (i.e. was raised to top)
	YZ_WINDOW_DEACTIVATE = 21,	// this window lost kbd focus (i.e. was sent from top)
	YZ_WINDOW_RESIZE = 22,		// this window was resized (x and y are filled out).
	YZ_WINDOW_CLOSE = 23		// user clicked close-box, so close the window
};

enum
{
	YZ_KEYCODE_LEFT_ARROW	=	255 + 1,
	YZ_KEYCODE_RIGHT_ARROW	=	255 + 2,
	YZ_KEYCODE_DOWN_ARROW	=	255 + 3,
	YZ_KEYCODE_UP_ARROW		=	255 + 4,
	YZ_KEYCODE_CONTROL		=	255 + 5,
	YZ_KEYCODE_SHIFT		=	255 + 6,
	YZ_KEYCODE_ALT			=	255 + 7,	//these two are the same...
	YZ_KEYCODE_OPTION		=	255 + 7,
	
	YZ_KEYCODE_UNKNOWN = 65536
};

enum
{
	YZ_BUFFER_FRONT	=	1,
	YZ_BUFFER_BACK	=	2,
	YZ_BUFFER_BOTH	=	3
};

typedef	struct	event_t
{
	int		type;
	int		button, x, y;	// which button the click was, and where it was
	int		mouse, x2, y2;	// which mouse it was, and where the drag ended
	int		key;			// the ascii code of the key pressed, or a speccial key code
	// the structure will expand as more types of events are added:
	void	*ui_element;
	int		unused[56];
}	event_t;

/* Creating Windows */
/* Note: creating a window does NOT select it. */
window_t	*yzNewWindow(char	*name, unsigned short	width, unsigned short	height);
window_t	*yzNewWindowAtFlags(char	*name, int	x, int	y, unsigned short	width, unsigned short	height, int	flags);

/* creates a window that fills the entire screen */
window_t	*yzFullScreen();

/* Window Information Management */

window_t	*yzGetSelectedWindow();
int			yzGetWindowCorner(int	*x, int	*y);
void		*yzGetWindowData(char	*key);
int			yzPutWindowData(char	*key, void	*data);

/* Deleting Windows */
/* Note: if you delete the currently selected window, the root window is automatically selected.
 * (you cannot delete the root window) */
int			yzDeleteWindow(window_t	*window);

/* Selecting Windows */
window_t	*yzSelectWindow(window_t	*window);	//returns previously selected window

/* Buffering */
int			yzBufferMode(int	mode);
int			yzUpdateBuffer(int	which);

/* Querying the Pointer */
int			yzQueryPointer(int	*x, int	*y);
		//yzQueryPointer returns -1 on no focus, 0 on no-move, 1 on moved, 2 on mousedown, 3 on mouseup

/* Query a Key */
int			yzQueryKey(int	code);
		//yzQueryKey returns 0 if the key is up, 1 if it is depressed, and -1 if the selected window is not focused

/* Event handling */
int			yzAreEventsWaiting();
event_t		yzGetEvent(int	waitfor);
event_t		yzGetUnfilteredEvent(int	waitfor);
event_t		yzGetGlobalEvent(int	waitfor, window_t	**w);

int		yzAddEventFilter(int	(*filter)(event_t*));
int		yzRemoveEventFilter(int	(*filter)(event_t*));

#endif
