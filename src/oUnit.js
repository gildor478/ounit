/**************************************************************************/
/* The OUnit library                                                      */
/*                                                                        */
/* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           */
/* Copyright (C) 2010 OCamlCore SARL                                      */
/* Copyright (C) 2013 Sylvain Le Gall                                     */
/*                                                                        */
/* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  */
/* and Sylvain Le Gall.                                                   */
/*                                                                        */
/* Permission is hereby granted, free of charge, to any person obtaining  */
/* a copy of this document and the OUnit software ("the Software"), to    */
/* deal in the Software without restriction, including without limitation */
/* the rights to use, copy, modify, merge, publish, distribute,           */
/* sublicense, and/or sell copies of the Software, and to permit persons  */
/* to whom the Software is furnished to do so, subject to the following   */
/* conditions:                                                            */
/*                                                                        */
/* The above copyright notice and this permission notice shall be         */
/* included in all copies or substantial portions of the Software.        */
/*                                                                        */
/* The Software is provided ``as is'', without warranty of any kind,      */
/* express or implied, including but not limited to the warranties of     */
/* merchantability, fitness for a particular purpose and noninfringement. */
/* In no event shall Maas-Maarten Zeeman be liable for any claim, damages */
/* or other liability, whether in an action of contract, tort or          */
/* otherwise, arising from, out of or in connection with the Software or  */
/* the use or other dealings in the software.                             */
/*                                                                        */
/* See LICENSE.txt for details.                                           */
/**************************************************************************/

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
