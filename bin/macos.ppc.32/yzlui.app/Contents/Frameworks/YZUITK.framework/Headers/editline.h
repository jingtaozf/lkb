typedef enum
{
	YZ_EDITLINE_TAB	=	1,
	YZ_EDITLINE_RETURN =2,
	YZ_EDITLINE_ENTER = 4,
	YZ_EDITLINE_ESCAPE =8
}	editline_keyskips;

struct editline_t;
typedef struct editline_t editline_t;

editline_t	*yzAddEditline(char	*txt, int	x, int	y, int	dx, int	dy,
								int	id, editline_keyskips	skips);
editline_t	*yzAddEditlineWithHandlers(char	*txt, int	x, int	y, int	dx, int	dy,
										int	id, editline_keyskips	skips,
					void	(*skip_handler)(struct editline_t	*e, int	key));
void		yzRemoveEditline(editline_t	*e);
void		yzSelectEditline(editline_t	*e, int	begin, int	end);	// end==-1 == to end
void		yzEditlineHandleKey(editline_t	*e, int	key);	// to manually report a key

editline_t	*yzGetSelectedEditline();
char		*yzGetEditlineText(editline_t	*e);	// returns null-terminated
void		yzSetEditlineText(editline_t	*e, char	*txt);
void		yzSetEditlineMode(editline_t	*e, int	mode);	// 0 = normal, 1 = password field
