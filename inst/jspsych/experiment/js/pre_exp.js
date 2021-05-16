var pre_exp = [];

// PIS
var pis_sona = {
  type: "instructions",
  pages: [
    '<p>Welcome to the study.</p><p>Make sure to scroll down to the bottom of each page to see the navigation buttons.</p>',
    '<img src="img/pis1_sona.png"></img>',
    '<img src="img/pis2_sona.png"></img>',
    '<img src="img/pis3_sona.png"></img>'
  ],
  show_clickable_nav: true
};
pre_exp.push(pis_sona);

var pis_online = {
  type: "instructions",
  pages: [
    '<p>Welcome to the study.</p><p>Make sure to scroll down to the bottom of each page to see the navigation buttons.</p>',
    '<img src="img/pis_online1.png" class="pis"></img>',
    '<img src="img/pis_online2.png" class="pis"></img>',
    '<img src="img/pis_online3.png" class="pis"></img>'
  ],
  show_clickable_nav: true
};
//pre_exp.push(pis_online);

var check_consent = function(elem) {
  if (document.getElementById('consent_checkbox').checked) {
    return true;
  }
  else {
    alert("If you wish to participate, you must check the box next to the statement 'I agree to participate in this study.'");
    return false;
  }
  return false;
};

// declare the block.
var consent = {
  type:'external-html',
  url: "js/consent.html",
  cont_btn: "start",
  check_fn: check_consent
};
pre_exp.push(consent);

var contact = {
  type: 'survey-html-form',
  html: obj.contact
};
pre_exp.push(contact);

// change from obj.recruitment.sona to obj.recruitment.prolific as needed
var id = {
  type: 'survey-html-form',
  html: obj.recruitment.sona
};
pre_exp.push(id);

var demographics = {
  type: "survey-html-form",
  html: obj.demographics
};
pre_exp.push(demographics);