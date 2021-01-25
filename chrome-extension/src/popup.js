/*global chrome*/

import { Constants } from './constants.js';

document.addEventListener('DOMContentLoaded', () => {
  const checkbox = document.querySelector('input#enableIzuna');

  getSavedBackgroundColor((savedChecked) => {
    if(savedChecked) {
      checkbox.checked = savedChecked;
    }
  });

  checkbox.addEventListener('change', () => {
    var keyValue = {};
    keyValue[Constants.ENABLE_IZUNA_KEY] = checkbox.checked;
    chrome.storage.sync.set(keyValue);
  });
});

function getSavedBackgroundColor(callback) {
  chrome.storage.sync.get(Constants.ENABLE_IZUNA_KEY, (checked) => {
    callback(chrome.runtime.lastError ? null : checked[Constants.ENABLE_IZUNA_KEY]);
  });
}
