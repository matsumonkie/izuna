chrome.runtime.onInstalled.addListener(function() {
  console.log('onInstalled....');

  chrome.storage.sync.set({color: '#3aa757'}, function() {
    console.log('The color is green.');
  });

  chrome.declarativeContent.onPageChanged.removeRules(undefined, function() {
    chrome.declarativeContent.onPageChanged.addRules([{
      conditions: [ new chrome.declarativeContent.PageStateMatcher({ pageUrl: {hostEquals: 'github.com'} })
                  ],
      actions: [ new chrome.declarativeContent.ShowPageAction() ]
    }]);
  });
});

chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  fetch('http://localhost:3000/api/modulesAst/home/iori/work/Mimizuku/example/.hie')
    .then(r => r.json())
    .then(data => {
      sendResponse(data);
    });

  return true;
});
