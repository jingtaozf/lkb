#include	"uitk.h"

typedef struct	menu_item_t
{
	char	*name;
	int		id;
}	menu_item_t;

typedef struct	menu_data_t
{
	char		*name;
	int			nitems;
	menu_item_t	*items;
}	menu_data_t;

typedef struct	popup_menu_t
{
	int			type;	// for UITK
	int			id, own_menu;
	menu_data_t		*menu;	// the contents
	int			x, y, dx, dy;
	int			lastSel;	// the last item selected
	int			lastSelID;	// the last item id selected
}	popup_menu_t;

menu_data_t		*yzNewMenuData(char	*name);
void		yzDisposeMenuData(menu_data_t	*menu);
int	yzAddMenuDataItem(menu_data_t	*menu, char	*item, int	id);

popup_menu_t	*yzAddPopupMenuFromMenuData(menu_data_t	*m, int	x, int	y,
									int	dx, int	dy, int	id);
popup_menu_t	*yzAddPopupMenu(char	*name, int	x, int	y,
									int	dx, int	dy, int	id, ...);
char			*yzPopupGetItemName(popup_menu_t	*pm, int	i);
void			yzRemovePopupMenu(popup_menu_t	*pm);
int				yzPopupUIType(void);
int				yzShowMenu(menu_data_t	*m, int	*selid, int	mx, int	my);
