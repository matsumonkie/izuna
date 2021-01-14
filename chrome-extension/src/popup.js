document.addEventListener('DOMContentLoaded', () => {
  const checkbox = document.querySelector('input#enableIzuna');

  getSavedBackgroundColor((savedChecked) => {
    if(savedChecked) {
      checkbox.checked = savedChecked;
    }
  });

  checkbox.addEventListener('change', () => {
    chrome.storage.sync.set({ enableIzuna: checkbox.checked });
  });
});

function getSavedBackgroundColor(callback) {
  chrome.storage.sync.get('enableIzuna', (checked) => {
    callback(chrome.runtime.lastError ? null : checked['enableIzuna']);
  });
}
