#ifndef	YZ_DRAW_H
#define	YZ_DRAW_H

typedef	struct	color_t
{
	unsigned short	r, g, b, a;
}	color_t;

typedef	struct	pen_t
{
	unsigned short	x, y;
	unsigned short	size;
	color_t			color;
}	pen_t;

typedef	struct	font_t
{
	char	name[128];		// the name-string of the font. either "system" or "fixed".
	int		size;			// height of a character. unused.
	int		attr;			// bold, italics, etc. unused.
	void	*impl_data;		// each implementation will store fonts in its own way
}	font_t;

int	yzLine2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzRect2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzOutlineRect2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzCircle3i(int	x,  int	y,  int	r);
int	yzFillCircle3i(int	x, int	y, int	r);
int	yzOval4i(int	x, int	y, int	rx, int	ry);
int	yzFillOval4i(int	x, int	y, int	rx, int	ry);
int	yzOvalInRect2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzFillOvalInRect2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzClear();
int	yzPixel2i1c(int	x, int	y, color_t	c);
int	yzPixel2i3i(int	x, int	y, int	red, int	green, int	blue);
int	yzPolygon(int	count, ...);

int	yzPenColor1c(color_t	c);
int yzPenColor3i(int	r, int	g, int	b);
int yzPenColor4i(int	r, int	g, int	b, int	a);
int yzPenSize1i(int		s);

int	yzSelectFont1s(char	*font_name);
int	yzSelectFont1f(struct font_t	*font);
font_t	*yzGetFont(char	*font_name);
void	yzFreeFont(font_t	*font);
int	yzText2i1s(int	x, int	y, char	*ptr);
int	yzText2i1p1i(int	x, int	y, char	*ptr, int	len);

int	yzLetterSize(int	letter);
int	yzStringSize(char	*str, int	len);

int	yzPushClipRect2i2i(int	x1, int	y1, int	x2, int	y2);
int	yzPopClipRect();
int	yzSetClipRect2i2i(int	x1, int	y1, int	x2, int	y2);

/* shortcuts (yz 0.2.0) */
#define	yzLine			yzLine2i2i
#define	yzRect			yzRect2i2i
#define	yzOutlineRect	yzOutlineRect2i2i
#define	yzPixel			yzPixel2i1c
#define	yzPenColor		yzPenColor3i
#define	yzPenSize		yzPenSize1i
#define	yzCircle		yzCircle3i
#define	yzFillCircle	yzFillCircle3i
#define	yzOval			yzOval4i
#define	yzFillOval		yzFillOval4i
#define	yzOvalInRect	yzOvalInRect2i2i
#define	yzFillOvalInRect yzFillOvalInRect2i2i
#define	yzPoly			yzPolygon
#define	yzSelectFont	yzSelectFont1s
#define	yzUseFont		yzSelectFont1f
#define	yzText			yzText2i1s
#define	yzPushClipRect	yzPushClipRect2i2i
#define	yzSetClipRect	yzSetClipRect2i2i
#define	yzPushOrigin	yzPushOrigin2i

#endif
