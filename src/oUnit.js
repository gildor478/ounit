/***********************************************************************
 * The OUnit library                                                   * 
 *                                                                     * 
 * Copyright (C) 2013 Sylvain Le Gall.                                 * 
 *                                                                     * 
 * See LICENSE for details.                                            * 
 ***********************************************************************/

var successHidden = true;

function displaySuccess(display) {
  var div = document.getElementsByClassName('ounit-success');
  for (var i = 0; i < div.length; i++) {
    div[i].style.display = display;
  };
};

function toggleSuccess() {
  if (successHidden) {
    displaySuccess('block');
  } else {
    displaySuccess('none');
  };
  successHidden = ! successHidden;
  var button = document.getElementById('toggleVisibiltySuccess');
  if (successHidden) {
    button.textContent = 'Show success';
  } else {
    button.textContent = 'Hide success';
  };
};

function resetTestCurrent() {
  var div = document.getElementById('ounit-current');
  if (div) {
    div.removeAttribute('id');
  };
};

function setTestCurrent(div) {
  resetTestCurrent();
  div.id = "ounit-current";
  div.scrollIntoView(true);
};

function nextTest() {
  var div = document.getElementsByClassName('ounit-test');
  var found = false;
  var foundCurrent = false;
  var idx = 0;
  if (div) {
    for (; !found && idx < div.length; idx++) {
      if (foundCurrent && div[idx].style.display != 'none') {
        found = true;
      };
      if (div[idx].id == "ounit-current") {
        foundCurrent = true;
      };
    };
    if (!foundCurrent && div.length > 0) {
      setTestCurrent(div[0]);
    } else if (found) {
      setTestCurrent(div[idx - 1]);
    } else {
      resetTestCurrent();
    };
  };
};

function gotoTop() {
  window.scrollTo(0,0);
  resetTestCurrent();
};
