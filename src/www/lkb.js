var __console__ = null;

var __pageInitializedP__= false;

function debug(string) {

  if(__console__ == null || __console__.closed) {
    __console__ = window.open("JavaScript Message",
                              "console",
                              "width=600,height=300,resizable");
    __console__.document.open("text/plain");
  }// if
  var now = new Date();
  var hours = now.getHours();
  if(hours < 10) hours =  "0" + hours;
  var minutes = now.getMinutes();
  if(minutes < 10) minutes =  "0" + minutes;
  var seconds = now.getSeconds();
  if(seconds < 10) seconds =  "0" + seconds;

  __console__.focus();
  __console__.document.write("{" + window.name + "}");
  __console__.document.write("[" + hours + ":" + minutes 
                             + ":" + seconds + "] ");
  __console__.document.write(string + "\n");

} // debug()


function postStatus(string) {

  window.status = string;
  
} // postStatus()


function messenger() {

  atInitialize();
  window.focus();
  enableElement('analyze');
  enableElement('translate');
  // defaultStatus += " " + window.title;
  if(false) {
    var foo = document.getElementById("main");
    if (foo && foo.target) defaultStatus += " " + foo.target;
  } // if

  __pageInitializedP__ = true;

} // messenger()


function submitter() {

  if(__pageInitializedP_) return true;
  __pageInitializedP__ = false;
  return false;

} // submitter()

function setTarget(context, string, flag) {

  var foo = document.getElementById(context);
  if(foo && foo.target) foo.target = string;

  if(flag != undefined && flag) {
    disableElement('analyze');
    disableElement('translate');
  } // if

} // formTarget()


function enableElement(id) {
  var foo = document.getElementById(id);
  if(foo && foo.disabled) foo.disabled = false;
} // enableElement()


function getElementbyClass(name){
  var i = 0;
  var matches = new Array();

  var all = (document.all ? document.all : document.getElementsByTagName("*"));
  for(i = 0; i < all.length; i++) {
    if(all[i].className == name) matches[i++] = all[i];
  } // for
} // getElementbyClass()


//
// active MRS display: variable highlighting and property pop-ups
//

var mrsVariables = new Object();
mrsVariables.size = 0;
mrsVariables.secondary = null;

var mrsHCONSsForward = new Object();
var mrsHCONSsBackward = new Object();


function mrsVariableSelect(name, text) {

  writetxt(text);
  classSetColor('mrsVariable' + name, 'red');

  if(mrsHCONSsForward[name]) {
    mrsVariables.secondary = mrsHCONSsForward[name];
    classSetColor('mrsVariable' + mrsHCONSsForward[name], 'green');
  } else if(mrsHCONSsBackward[name]) {
    mrsVariables.secondary = mrsHCONSsBackward[name];
    classSetColor('mrsVariable' + mrsHCONSsBackward[name], 'green');
  } // if

} // mrsVariableSelect()


function mrsVariableUnselect(name) {

  writetxt(0);
  classSetColor('mrsVariable' + name, '#1a04a5');
  if (mrsVariables.secondary) {
    classSetColor('mrsVariable' + mrsVariables.secondary, '#1a04a5');
    mrsVariables.secondary = null;
  } // if

} // mrsVariableUnselect()


function classSetColor(name, color) {

  if(mrsVariables[name] != null) {
    for (var i = 0; i < mrsVariables[name].length; ++i) 
      if(color == 'swap') {
        var foo = mrsVariables[name][i].style.color;
        mrsVariables[name][i].style.color 
          = mrsVariables[name][i].style.background;
        mrsVariables[name][i].style.background = foo;
      } // if
      else {
        mrsVariables[name][i].style.color = color;
      } // else
  } // if
  else {
    var all = 
      (document.all ? document.all : document.getElementsByTagName('*'));
    for (var i = 0; i < all.length; ++i) {
      var index = all[i].className;
      if(mrsVariables[index] == null) {
        mrsVariables[index] = new Array();
        ++mrsVariables.size;
      } //if 
      mrsVariables[index].push(all[i]);
      if(all[i].className == name) {
        if(color == 'swap') {
          var foo = all[i].style.color;
          all[i].style.color = all[i].style.background;
          all[i].style.background = foo;
        } // if
        else {
          all[i].style.color = color;
        } // esle
      } // if
    } // for
  } // else

} // classSetColor()
