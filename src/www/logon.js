var __console__ = null;

var pending = null;

var __pageInitializedP__ = false;

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
  ComparisonUpdate('smt');
  ComparisonUpdate('oa');
  ComparisonUpdate('visl');
  ComparisonUpdate('it');
  // defaultStatus += " " + window.title;
  if(false) {
    var foo = document.getElementById("main");
    if (foo && foo.target) defaultStatus += " " + foo.target;
  } // if

  __pageInitializedP__ = true;

} // messenger()


function submitter() {

  if(__pageInitializedP_) {
    disableElement('analyze');
    disableElement('translate');
    return true;
  } // if
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


function disableElement(id) {

  var foo = document.getElementById(id);
  if(foo && foo.disabled) foo.disabled = true;

} // disableElement()


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


function clearElement(form, element) {

  document.forms[form].elements[element].value = "";

} // clearElement()


var posters = Array();

posters['smt'] 
= "A baseline SMT system was trained (by Erik Velldal at the University of \
   Oslo) through the application of off-the-shelf SMT \
   technology&mdash;GIZA++, the SRI LM toolkit, and Pharaoh&mdash;to the \
   LOGON development corpus, extended with some 12,000 sentences from the \
   ENPC Norwegian&ndash;English parallel corpus.";
posters['oa']
= "The <i>Oversettelsesassistent</i> (&lsquo;Translation Assistant&rsquo;) \
   prototype, built by Torbj&oslash;n Nordg&aring;rd and Ola Huseth at NTNU \
   Trondheim, combines statistical analysis and generation techniques with a \
   bi-lingual lexicon, viz. <i>Stor Norsh&ndash;Engelsk Ordbok</i> by the \
   Norwegian publisher <i>Kunnskapsforlaget</i>.";
posters['visl']
= "The VISL machine translation prototype is a wrapper to the \
   Danish&ndash;English version of the system, essentially \
   using Danish as an &lsquo;interlingua&rsquo; for translation from closely \
   related Scandinavian languages.  The system is under development by \
   Eckhard Bick and colleagues at the University of Southern Denmark.  \
   The basic VISL architecture is similar to LOGON, though using a pipeline \
   of Constraint Grammar tagging and dependency parsing for analysis, and \
   tranfer at the dependency level.";
posters['it']
= "The commercial InterTran MT system is a product offered by \
   TranslationExperts Inc.  \
   The software is available for a combination of 28 source and target \
   languages, and the product appears hardly fine-tuned for \
   Norwegian&ndash;English.  No information is available on the inner working \
   of InterTran.";


function post(id) {

  if(posters[id] != null) 
    writetxt("<div class=\"poster\">" + posters[id] + "</div>");
  
} // 


function unpost() {

  writetxt(0);

} // unpost()


function HttpClient() {

  var client = null;
  try {
    client = new XMLHttpRequest();
  } // try
  catch (e) {
    try {
      client = new ActiveXObject('MSXML2.XMLHTTP');
    } // try
    catch (e) {
      try {
        client = new ActiveXObject('Microsoft.XMLHTTP');
      } // try
      catch (e) {}
    } // catch
  } // catch
  return client;

} // HttpClient()


function ComparisonUpdate(id) {

  if(pending != null && pending[id] != null) {
    var client = HttpClient();
    client.onreadystatechange = function() {
      if(client.readyState == 4) {
        if(client.status == 200 && client.responseText != "") {
          var div = document.getElementById(id);
          div.lastChild.innerHTML = client.responseText;
          pending[id] = null;
        } // if
        else {
          window.setTimeout("ComparisonUpdate('" + id + "')", 5000);
        } // else
      } // if
    } // function()
    client.open('POST', '/fetch', true);
    client.send('id=' + pending[id]);
  } // if

} // ComparisonUpdate()


var samples =
[ { id: 210, item: "Bjørne- og ulvestammen var tallrik og angrepslysten.", readings: 20},
  { id: 870, item: "Det samme gjelder for øvrig flere falkearter.", readings: 78},
  { id: 910, item: "Om vinteren er særlig ravnen tallrik.", readings: 10},
  { id: 10020, item: "Besseggen og mange andre perler", readings: 12},
  { id: 10210, item: "Bilvei og bussforbindelse til hytta.", readings: 5},
  { id: 10250, item: "Gjendesheim ble åpnet i 1878.", readings: 13},
  { id: 10270, item: "Bestyrere: Bjørg Aaseng og Olav Gaute Vole.", readings: 4},
  { id: 10290, item: "Tlf: 61 23 89 10.", readings: 2},
  { id: 10540, item: "Bilvei og bussforbindelse forbi hytta.", readings: 5},
  { id: 10550, item: "Merkede fotturruter til Gjendesheim, Memurubu og Glitterheim.", readings: 16},
  { id: 10580, item: "Bessheim har tatt imot turister siden midten av 1800-tallet.", readings: 9},
  { id: 10920, item: "Glitterheim ble innviet i 1901.", readings: 19},
  { id: 10940, item: "Bestyrere: Solveig og Ole Vole.", readings: 4},
  { id: 10960, item: "Tlf: 61 23 18 33.", readings: 2},
  { id: 11210, item: "Helt nede fra Bøverdalen kom seterfolket.", readings: 8},
  { id: 11250, item: "Enkelte karakteriserte setra som «en stenrøis».", readings: 30},
  { id: 11280, item: "Det var DNTs tredje hytteprosjekt.", readings: 24},
  { id: 11340, item: "Naturen rundt hytta er både voldsom og vennlig.", readings: 28},
  { id: 11430, item: "Gjendebu ble innviet i 1871.", readings: 15},
  { id: 11510, item: "Trivelig for hester og folk", readings: 12},
  { id: 11620, item: "Den dag i dag er det Sande-familien som driver Sikkilsdalssetra.", readings: 36},
  { id: 11780, item: "Sikkilsdalsseter har tatt imot turister siden 1868.", readings: 6},
  { id: 11890, item: "Bilvei forbi hytta og bussforbindelse.", readings: 10},
  { id: 11920, item: "Besstrond Sæter har tatt imot turister siden 1976.", readings: 6},
  { id: 11990, item: "Hytta er fyldig presentert i hefte 4", readings: 18},
  { id: 12020, item: "Bilvei til hytta og bussforbindelse.", readings: 10},
  { id: 12030, item: "Merkede fotturruter til Glitterheim, Juvasshytta, Leirvassbu og Gjendebu.", readings: 16},
  { id: 12060, item: "Spiterstulen har tatt imot turister siden 1830-tallet.", readings: 5},
  { id: 12120, item: "Hytta er fyldig presentert i hefte 4", readings: 18},
  { id: 12140, item: "Bilvei til hytta og bussforbindelse.", readings: 10},
  { id: 12150, item: "Merkede fotturruter til Spiterstulen, Raubergstulen/Røisheim og Elveseter.", readings: 24},
  { id: 12180, item: "Juvasshytta har tatt imot turister siden 1884.", readings: 6},
  { id: 12280, item: "Bilvei forbi hytta og bussforbindelse.", readings: 10},
  { id: 12310, item: "Hindsæter fjellstue har tatt imot turister siden 1860-årene.", readings: 10},
  { id: 12400, item: "Bilvei forbi stedet og bussforbindelse.", readings: 10},
  { id: 12410, item: "Merkede fotturruter til Gjendesheim og Sikkilsdalsseter.", readings: 16},
  { id: 12440, item: "Maurvangen Camping har tatt imot turister siden 1976.", readings: 9},
  { id: 12530, item: "Har du sett den Gjendineggen noen gang?", readings: 2},
  { id: 12580, item: "Turen regnes som en relativt lett dagsmarsj.", readings: 60},
  { id: 12600, item: "Den er noe luftig, men ufarlig.", readings: 66},
  { id: 12720, item: "Der er det lagt ut bro.", readings: 11},
  { id: 12730, item: "Rundskuet fra Veslefjell er formidabelt.", readings: 5},
  { id: 12780, item: "Høydeforskjellen ned til Bessvatn er på nesten 400 meter.", readings: 26},
  { id: 12880, item: "Også denne turen er en klassiker.", readings: 2},
  { id: 12920, item: "Underveis passerer den først Sjugurtindtjørna og så Grunnevatnet.", readings: 15},
  { id: 13150, item: "Helt siden 1906 har det vært motorbåtruter på Gjende.", readings: 10},
  { id: 13300, item: "Bussruter til Gjendesheim og Bessheim.", readings: 6},
  { id: 13320, item: "Den stopper både på Gjendebu, Memurubu og Gjendesheim.", readings: 6},
  { id: 13460, item: "Han var god venn med den legendariske reinjegeren Jo Gjende.", readings: 6},
  { id: 13680, item: "Jeg foreslår allikevel at du starter på Gjendesheim.", readings: 12},
  { id: 13720, item: "Naturgrunnlaget var i hvert fall i orden.", readings: 17},
  { id: 13860, item: "Tjørnholåa krysses på bro i et gjel.", readings: 30},
  { id: 14030, item: "For svært mange dreier det seg da om Glittertind.", readings: 24},
  { id: 14290, item: "Neste etappe går helt til Gjendebu.", readings: 18},
  { id: 14380, item: "Glem allikevel ikke å løfte blikket av og til.", readings: 40},
  { id: 14840, item: "Bussruter til Gjendesheim, Bessheim og Spiterstulen.", readings: 6},
  { id: 14860, item: "Den stopper både på Gjendebu, Memurubu og Gjendesheim.", readings: 6},
  { id: 15000, item: "Det er bro over Blåtjørnåa.", readings: 23},
  { id: 20300, item: "Ny hytte ved gammel innfallsport", readings: 5},
  { id: 20460, item: "Også den toppen krever brevandring.", readings: 5},
  { id: 20550, item: "Fondsbu turisthytte ble åpnet i 1993.", readings: 26},
  { id: 20970, item: "Like ved Mørstadstølen ligger Haugseter.", readings: 10},
  { id: 21030, item: "Mange turister ble også fraktet inn i robåt.", readings: 11},
  { id: 21090, item: "Det burde glede både fotturister og veifarende langs Jotunheimveien.", readings: 48},
  { id: 21120, item: "Jotunheimveien mellom Bygdin og Skåbu passerer hytta.", readings: 1},
  { id: 21160, item: "Haugseter fjellstue har tatt imot turister siden 1860-tallet.", readings: 10},
  { id: 21220, item: "Hytta som flyttet på seg", readings: 6},
  { id: 21630, item: "Men med nye tider kom nye behov.", readings: 4},
  { id: 22160, item: "Merkede fotturruter til Skogadalsbøen, Vettismorki og Slettningsbu.", readings: 16},
  { id: 22190, item: "Tyinholmen høyfjellstuer har tatt imot turister siden 1893.", readings: 28},
  { id: 22260, item: "Riksvei 55 over Valdresflya passerer hotellet.", readings: 28},
  { id: 22370, item: "Bilvei forbi stedet og bussforbindelse.", readings: 10},
  { id: 22380, item: "Merkede fotturruter til Gjendesheim og Sikkilsdalsseter.", readings: 16},
  { id: 22410, item: "Maurvangen Camping har tatt imot turister siden 1976.", readings: 8},
  { id: 22470, item: "Riksvei 51 over Valdresflya passerer hytta.", readings: 28},
  { id: 22510, item: "Valdresflya vandrerhjem ble innviet i 1952.", readings: 26},
  { id: 22570, item: "Variert tur sør for Bygdin", readings: 4},
  { id: 23100, item: "Populær rundtur mellom store topper", readings: 5},
  { id: 23150, item: "Den første halve kilometeren går ruta på grusvei.", readings: 8},
  { id: 23250, item: "Ruta til Gjendebu er ganske lett og fin.", readings: 60},
  { id: 23310, item: "Særlig på forsommeren er fargeprakten stor.", readings: 5},
  { id: 23360, item: "Rett før hytta krysses Storåa på solid bro.", readings: 10},
  { id: 23590, item: "Bestigningen av Sjogholstind er også mulig for normalt fjellvante folk.", readings: 30},
  { id: 23610, item: "Skardalstind er også en nydelig topp.", readings: 20},
  { id: 23810, item: "En favoritt hos mange er turen på Gjendestunga.", readings: 16},
  { id: 23850, item: "Følg ruta mot Torfinnsbu opp i Svartdalen.", readings: 28},
  { id: 24020, item: "Fondsbu, Eidsbugarden, Olavsbu og Gjendebu.", readings: 1},
  { id: 24060, item: "Til Gjendebu er det rutebåt på Gjende.", readings: 12},
  { id: 24280, item: "2. Torfinnsbu til Valdresflya VH eller Bygdin Fjellhotell", readings: 36},
  { id: 24350, item: "Litt før Dyrnesodden deler stien seg.", readings: 1},
  { id: 24430, item: "Den er dermed Jotunheimens største innsjø.", readings: 76},
  { id: 24500, item: "Tyin fikk først motorbåt i 1906.", readings: 34},
  { id: 24680, item: "Det er båtrute på Bygdin.", readings: 16},
  { id: 24780, item: "Her er det imidlertid dumt å forhaste seg.", readings: 20},
  { id: 30070, item: "Det var imidlertid lettere sagt enn gjort.", readings: 42},
  { id: 30240, item: "Skogadalsbøen ble innviet i 1888.", readings: 19},
  { id: 30360, item: "Det er dumt å vandre for raskt gjennom slik natur.", readings: 21},
  { id: 30430, item: "Gården er på nærmere 50 000 mål.", readings: 5},
  { id: 30720, item: "Det ble forlatt i 1950-årene.", readings: 9},
  { id: 30860, item: "Avdalen gård har vært turisthytte siden 1991.", readings: 7},
  { id: 30900, item: "Gravdalen ble innviet i 1975.", readings: 6},
  { id: 30910, item: "Årdal Turlag driver den ubetjente hytta.", readings: 2},
  { id: 31110, item: "Allerede den første vinteren blåste hytta ned.", readings: 6},
  { id: 31210, item: "Stølsmaradalen har merkede ruter til Avdalen og Vetti.", readings: 20},
  { id: 31290, item: "Stølsmaradalen ble innviet i 1975.", readings: 19},
  { id: 31300, item: "DNT leier to sel og driver dem som ubetjent hytte.", readings: 40},
  { id: 31410, item: "Det ble raskt en populær turisthytte.", readings: 36},
  { id: 31450, item: "Den nye Leirvassbu sto klar i 1906.", readings: 14},
  { id: 31530, item: "Det er bilvei til hytta.", readings: 16},
  { id: 31560, item: "Leirvassbu ble bygget som steinbu i 1875 av DNT.", readings: 22},
  { id: 31570, item: "Første turisthytte kom i 1906.", readings: 23},
  { id: 31680, item: "En taus advarsel om storm og uvær.", readings: 42},
  { id: 31730, item: "Oversikten over Smørstabbreen i øst er nesten total.", readings: 40},
  { id: 31870, item: "Fannaråkhytta har tatt imot turister siden 1926.", readings: 6},
  { id: 32000, item: "Turtagrø ble bygget i 1888.", readings: 13},
  { id: 32010, item: "Hotellet eies og drives av Ole Berge Drægni.", readings: 20},
  { id: 32360, item: "Krossbu turiststasjon startet opp i 1902.", readings: 12},
  { id: 32420, item: "Se omtale i hefte 2", readings: 16},
  { id: 32620, item: "Sognefjellhytta ble åpnet i 1947.", readings: 13},
  { id: 32720, item: "Merkede fotturruter til Skogadalsbøen, Vettismorki og Slettningsbu.", readings: 16},
  { id: 32750, item: "Tyinholmen høyfjellstuer har tatt imot turister siden 1893.", readings: 4},
  { id: 32850, item: "Gjendebu ble innviet i 1871.", readings: 19},
  { id: 32970, item: "Fondsbu turisthytte ble åpnet i 1993.", readings: 26},
  { id: 33200, item: "Fannaråkhytta ligger helt på toppen.", readings: 25},
  { id: 33380, item: "3. Skogadalsbøen til Ingjerdbu, Vetti eller Hjelle", readings: 6},
  { id: 33410, item: "Underveis deler ruta seg to ganger.", readings: 6},
  { id: 33620, item: "Om sommeren er det kafé i våningshuset.", readings: 24},
  { id: 33770, item: "Tømmeret ble sendt utfor Vettisfossen på ettervinteren.", readings: 6},
  { id: 33810, item: "Fluorgass fra Årdal har imidlertid skadet skogen sterkt.", readings: 17},
  { id: 33870, item: "Med litt forsiktighet kan du komme deg helt innunder fossen.", readings: 5},
  { id: 34050, item: "Bussruter både til Øvre Årdal og til Turtagrø og Sognefjellet.", readings: 64},
  { id: 34060, item: "Bilvei til Hjelle fra Øvre Årdal.", readings: 8},
  { id: 34390, item: "Krossbu, Sognefjellhytta, Fannaråkhytta og Skogadalsbøen.", readings: 1},
  { id: 34420, item: "Bilvei med bussruter over Sognefjellet.", readings: 10},
  { id: 34530, item: "Bekken fra Nedre Høgvagltjønn steingås, eller vades.", readings: 32},
  { id: 34570, item: "Oppe på Raudalsbandet åpner utsynet seg igjen.", readings: 16},
  { id: 34660, item: "Først i litt uret lende fram til skaret nord for Sjogholstind.", readings: 15},
  { id: 34680, item: "Langs østbredden av dette, over nok en rygg og deretter utfor mot Fondsbu.", readings: 9},
  { id: 34850, item: "5. Krossbu/Sognefjellhytta til Leirvassbu over Smørstabbreen", readings: 9},
  { id: 35000, item: "Mjølkedalstind er et opplagt valg.", readings: 6},
  { id: 35110, item: "Det viktigste turmålet gir seg selv.", readings: 18},
  { id: 35170, item: "For brevandrere er også utvalget stort.", readings: 19},
  { id: 35210, item: "Tidligere var denne ruta kvistet.", readings: 67},
  { id: 35480, item: "Leirvassbu, Olavsbu, Fondsbu, Skogadalsbøen, Krossbu og Sognefjellhytta.", readings: 1},
  { id: 40150, item: "I 1956 overtok døtrene Ragnhild og Tora roret.", readings: 32},
  { id: 40240, item: "Bilvei til hytta og bussforbindelse.", readings: 10},
  { id: 40250, item: "Merkede fotturruter til Spiterstulen, Raubergstulen/Røisheim og Elveseter.", readings: 24},
  { id: 40280, item: "Juvasshytta har tatt imot turister siden 1884.", readings: 6},
  { id: 40370, item: "Det har da også skjedd med Spiterstulen.", readings: 21},
  { id: 40440, item: "Heldigvis har denne veksten skjedd med vett.", readings: 20},
  { id: 40460, item: "På Spiterstulen har de som har bygget, maktet å bevare seterstilen.", readings: 30},
  { id: 40490, item: "Beliggenheten er unik på andre måter også.", readings: 26},
  { id: 40600, item: "Bilvei til hytta og bussforbindelse.", readings: 10},
  { id: 40610, item: "Merkede fotturruter til Glitterheim, Juvasshytta, Leirvassbu og Gjendebu.", readings: 16},
  { id: 40640, item: "Spiterstulen har tatt imot turister siden 1830-tallet.", readings: 5},
  { id: 40880, item: "Både Smørstabbtinder og Fannaråken er naturlige turmål herfra.", readings: 10},
  { id: 40960, item: "Sognefjellhytta ble åpnet i 1947.", readings: 13},
  { id: 41130, item: "I 1914 sto derfor et anneks klart.", readings: 36},
  { id: 41310, item: "Krossbu turiststasjon startet opp i 1902.", readings: 8},
  { id: 41450, item: "Så sauen flyttet ut og turistene flyttet inn, bokstavelig talt.", readings: 24},
  { id: 41660, item: "Norges lengste fjord heter Sognefjorden.", readings: 4},
  { id: 41730, item: "For pionerene var kryssingen av Fortundalselva like nedenfor hytta lenge en halsløs gjerning.", readings: 34},
  { id: 41820, item: "Turmulighetene rundt Nørdstedalseter er også mangfoldige.", readings: 4},
  { id: 41890, item: "Nørdstedalseter ble bygget i 1889.", readings: 13},
  { id: 41990, item: "Medalsbu øverst i Middalen ble aldri noen suksess for DNT.", readings: 30},
  { id: 42020, item: "Tanken bak hytta var imidlertid god.", readings: 15},
  { id: 42220, item: "Trulsbu ble bygget i 1988 og drives som selvbetjeningshytte.", readings: 20},
  { id: 42330, item: "Hytta ble åpnet i 1946.", readings: 13},
  { id: 42560, item: "Det er bilvei til hytta.", readings: 16},
  { id: 42590, item: "Leirvassbu ble bygget som steinbu i 1875 av DNT.", readings: 22},
  { id: 42600, item: "Første turisthytte kom i 1906.", readings: 23},
  { id: 42690, item: "Sognefjellsveien passerer hytta, og der finnes det bussruter.", readings: 6},
  { id: 42730, item: "Hytta ble åpnet i 1864.", readings: 13},
  { id: 42950, item: "Fannaråkhytta har tatt imot turister siden 1926.", readings: 6},
  { id: 43070, item: "Stedet har tatt imot gjester fra ca. 1880.", readings: 1},
  { id: 43210, item: "Turtagrø ble bygget i 1888.", readings: 13},
  { id: 43220, item: "Hotellet eies og drives av Ole Berge Drægni.", readings: 20},
  { id: 43540, item: "Til topps på Galdhøpiggen fra Spiterstulen", readings: 26},
  { id: 43660, item: "Returen er så ned den vanlige ruta til Spiterstulen.", readings: 36},
  { id: 43840, item: "Vinteren 1970 tok vinden Volehytta.", readings: 6},
  { id: 43960, item: "Det er bilvei og bussrute til Spiterstulen.", readings: 15},
  { id: 43980, item: "Til topps på Galdhøpiggen fra Juvasshytta", readings: 26},
  { id: 44160, item: "På toppen er det en liten kafeteria.", readings: 25},
  { id: 44340, item: "Bilvei og bussrute til Juvasshytta.", readings: 5},
  { id: 44500, item: "Du må holde til høyre.", readings: 12},
  { id: 44550, item: "2. Leirvassbu til Krossbu eller Sognefjellhytta", readings: 6},
  { id: 44670, item: "Nedgangen på vestsiden er vanligvis på nordsiden av Bøverbreen.", readings: 16},
  { id: 44710, item: "3. Sognefjellhytta eller Krossbu til Fannaråkhytta", readings: 3},
  { id: 44900, item: "Dessverre brant Turtagrø hotell i januar 2001.", readings: 14},
  { id: 44920, item: "Sjekk forholdene på forhånd med DNT eller direkte med hotellet.", readings: 9},
  { id: 45100, item: "Spiterstulen, Leirvassbu, Krossbu, Sognefjellhytta, Fannaråkhytta og Turtagrø.", readings: 1},
  { id: 45110, item: "Det siste er noe usikkert pr. mars 2001.", readings: 6},
  { id: 45180, item: "Området nordvest for Jotunheimen bærer navnet Breheimen.", readings: 9},
  { id: 45510, item: "Du følger Dummdalen til Svarttjørna.", readings: 2},
  { id: 45600, item: "Den første ligger like ved Sognefjellsveien.", readings: 20},
  { id: 45620, item: "Det arrangeres guidede turer til grottene.", readings: 28},
  { id: 45710, item: "Krossbu, Sognefjellhytta, Nørdstedalseter og Bøvertun.", readings: 1},
  { id: 45740, item: "Bilvei og bussruter langs Sognefjellsveien.", readings: 5},
  { id: 45810, item: "Turen over den 1800 meter høye breen er storslagen.", readings: 30},
  { id: 45930, item: "Elva i dalbunnen kan vanligvis også steingås.", readings: 18},
  { id: 45980, item: "1. Alternativ: Fortundalen til Stølsdalen", readings: 8},
  { id: 46050, item: "Dette er høydepunktet på turen, bokstavelig talt.", readings: 35},
  { id: 50210, item: "Godt fottøy er halve turen.", readings: 8},
  { id: 50380, item: "Utstyrsliste for sommerturer med overnatting i hytter", readings: 4},
  { id: 50670, item: "Kartmappe, gjerne med blyant og papir", readings: 6},
  { id: 50910, item: "Ta med barna til fjells", readings: 16},
  { id: 51110, item: "Hele Norge dekkes av slike kart.", readings: 5},
  { id: 51190, item: "Når uværet kommer, er det for sent.", readings: 9},
  { id: 51250, item: "Å ta ut en kurs", readings: 14},
  { id: 51480, item: "En bevisstløs person skal legges i stabilt sideleie.", readings: 40},
  { id: 51560, item: "Legg ham eventuelt med beina høyt.", readings: 22},
  { id: 51590, item: "Dekk åpne sår, helst med sterile kompresser.", readings: 5} ];


function showSample(form, element) {

  var index = Math.round(Math.random() * (samples.length - 1));
  document.forms[form].elements[element].value = samples[index].item;

} // showSample()
