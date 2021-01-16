// script.js
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}


Shiny.addCustomMessageHandler('cookie-set', function(msg){
  
  tmp = msg.name + "=" + msg.value
  tmp += "; max-age=" + (30*24*60*60) + "; domain=.harvard.edu;";
  document.cookie = tmp
  
  
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-remove', function(msg){
  Cookies.remove(msg.name);
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-get', function(msg){  
  getCookies();
})



$(document).on('shiny:connected', function(ev){
  Shiny.setInputValue('init', 1);
  getCookies();
  
})
