#define	YZ_BUTTON_HIT	1000

struct button_t;
typedef struct button_t button_t;

button_t	*yzAddButton(char	*name, int	x, int	y, int	dx, int	dy, int	id);
button_t	*yzAddButtonWithHandler(char	*name, int	x, int	y, int	dx, int	dy,
								int	id, void	(*click_handler)(int));
void		yzRemoveButton(button_t	*b);
void		yzSetButtonFont(button_t	*b, char	*font);
