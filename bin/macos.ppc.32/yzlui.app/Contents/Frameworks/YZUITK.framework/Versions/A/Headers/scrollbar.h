struct scrollarea_t;
typedef struct scrollarea_t scrollarea_t;

scrollarea_t	*yzAddScrollArea(int	x, int	y, int	dx, int	dy,
					void	(*update)(void	*data, int	x1, int	y1, int	x2, int	y2),
					void	(*event)(void	*data, event_t	ev),
					void	*data);
void	yzRemoveScrollArea(scrollarea_t	*a);
void	yzResetScrollArea(scrollarea_t	*a);
void	yzDrawScrollArea(scrollarea_t	*a);
void	yzUpdateScrollArea(scrollarea_t	*a, int	x1, int	y1, int	x2, int	y2);
void	yzResizeScrollArea(scrollarea_t	*a, int	x, int	y, int	dx, int	dy);

struct scrollbar_t;
typedef struct scrollbar_t scrollbar_t;

scrollbar_t	*yzAddScrollBar(int	x, int	y, int	dx, int	dy, int	id);
void		yzRemoveScrollBar(scrollbar_t	*sb);
void		yzDrawScrollBar(scrollbar_t	*sb);
void		yzSetScrollBarMax(scrollbar_t	*sb, float	max);
void		yzSetScrollBarMin(scrollbar_t	*sb, float	min);
float		yzGetScrollBarPosition(scrollbar_t	*sb);
int			yzSetScrollBarPosition(scrollbar_t	*sb, float	now);	// nonzero if scrolling occured
void		yzSetScrollBarPageSize(scrollbar_t	*sb, float	ps);
void		yzSetScrollBarArea(scrollbar_t	*sb, scrollarea_t	*a);
void		yzResizeScrollBar(scrollbar_t	*sb, int	x, int	y, int	dx, int	dy);
