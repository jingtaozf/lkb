function mrsVariableSelect(name)
{

  classSetColor('mrsVariable' + name, 'red');

} // mrsVariableSelect()

function mrsVariableUnselect(name)
{

  classSetColor('mrsVariable' + name, '#1a04a5');

} // mrsVariableUnselect()


function classSetColor(name, color) {

  // window.status = 'setting ' + name + ' to ' + color;
  var all = document.all ? document.all : document.getElementsByTagName('*');
  var elements = new Array();
  for (var i = 0; i < all.length; i++)
    if (all[i].className == name) all[i].style.color = color;

} // classSetColor()

