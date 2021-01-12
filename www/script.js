// script.js
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}


Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value);
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
