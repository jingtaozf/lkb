function mrsVariableSelect(name, text)
{

  classSetColor('mrsVariable' + name, 'red');
  writetxt(text);

} // mrsVariableSelect()

function mrsVariableUnselect(name)
{

  classSetColor('mrsVariable' + name, '#1a04a5');
  writetxt(0);

} // mrsVariableUnselect()


function classSetColor(name, color) {

  //
  // _fix_me_
  // rewrite using a global array of relevant elements, indexed by embedding
  // MRS: make highlighting far more efficient and prevent highlighting across
  // structures.                                               (5-aug-03; oe)
  //
  //return false;
  var all = document.all ? document.all : document.getElementsByTagName('*');
  var elements = new Array();
  for (var i = 0; i < all.length; i++)
    //elements[all[i].className] = all[i];
    if (all[i].className == name) all[i].style.color = color;

} // classSetColor()


function postStatus(string) {

  window.status = string;
  
} // postStatus()