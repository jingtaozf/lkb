#ifndef	YZ_UITK_H
#define	YZ_UITK_H

#define	YZ_UI_EVENT		2001
#define	YZ_UI_FOCUS		"yz/ui/focus"

typedef struct	generic_ui_element_t
{
	int	type;
}	generic_ui_element_t;

#define	UIE_TYPE(elem)	(((struct generic_ui_element_t*)elem)->type)

// returns 1 if the given element is focusable, 0 otherwise
int	yzUIElementFocusable(generic_ui_element_t	*elem);

// returns 1 if the given element type is focusable, 0 otherwise
int	yzUITypeFocusable(int		type);

// returns a pointer to the currently focused item, or NULL if none exists
generic_ui_element_t	*yzUIGetFocus();

//returns 0 on success, negative on failure
int	yzUIFocusItem(generic_ui_element_t	*elem);

//returns 0 on success, negative on failure
int	yzUIDefocusItem(generic_ui_element_t	*elem);

#ifdef	YZ_UITK_IMPLEMENTOR
typedef struct ui_element_type_info
{
	char			*name;		// unique human-readable name
	int				type;		// allocated by yzUIRegisterTypeInfo
	int				focusable;	// can receive the focus?
	int		(*filter)(event_t*);	// can be 0
}	ui_element_type_info;

// returns pointer to type info on success, NULL on failure
ui_element_type_info	*yzUILookupType(char	*name);
ui_element_type_info	*yzUILookupTypeByNumber(int	tn);

// returns positive type id on success, 0 on failure
int						yzUIRegisterTypeInfo(ui_element_type_info	*ti);

typedef struct ui_element_list
{
	void					*e;
	struct ui_element_list	*next, *prev;
}	ui_element_list;

// register an example of a type into the selected window's list of it
int					yzUIAddToList(void	*ex);
// remove an exampel of a type from the selected window's list of it
int					yzUIRemoveFromList(void	*ex);

// retrieve the list of examples of a given type in the selected window
ui_element_list		*yzUIGetList(char	*name);
// set the list of examples of a given type in the selected window
void				yzUISetList(char	*name, ui_element_list	*lst);

#endif	// implementor

#endif	// YZ_UITK_H
