chrome.runtime.onInstalled.addListener(function() {
  console.log('onInstalled....');

  chrome.declarativeContent.onPageChanged.removeRules(undefined, function() {
    chrome.declarativeContent.onPageChanged.addRules([{
      conditions: [ new chrome.declarativeContent.PageStateMatcher({ pageUrl: {hostEquals: 'github.com'} })
                  ],
      actions: [ new chrome.declarativeContent.ShowPageAction() ]
    }]);
  });
});

chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  chrome.tabs.query({ currentWindow: true, active: true }, tabs => {
    const [user, repo, ...tail] = tabs[0].url.slice("https://github.com/".length).split("/")
    const packageName = repo;
    const commitId = '542ee18b941e8da8ed2d59c5a7f43e6562df0838';
    const izunaServerUrl = 'https://izuna-server.patchgirl.io/api/projectInfo/' + user + '/' + repo + '/' + packageName + '/' + commitId;
    console.log(izunaServerUrl);
    fetch(izunaServerUrl, { mode: 'cors', credentials: 'omit' })
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          console.log(`Could not fetch commit info for ${user}/${repo}/${packageName}/${commitId} - response failed with: ${response}`);
          return {};
        }
      })
      .then(data => { sendResponse(data); });

    return true;
  });

  return true;
});
