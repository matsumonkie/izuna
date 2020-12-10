chrome.runtime.onInstalled.addListener(function() {
  console.log('onInstalled....');

  chrome.declarativeContent.onPageChanged.removeRules(undefined, function() {
    chrome.declarativeContent.onPageChanged.addRules([{
      conditions: [ new chrome.declarativeContent.PageStateMatcher({ pageUrl: {hostEquals: 'github.com'} }) ],
      actions: [ new chrome.declarativeContent.ShowPageAction() ]
    }]);
  });
});

function fetchIzunaInfo(tabs,sendResponse) {
  const [user, repo, pull, pullRequest, ...tail] = tabs[0].url.slice("https://github.com/".length).split("/");

  const projectInfo = {
    user: user,
    repo: repo,
    pullRequest: pullRequest
  };
  Object.entries(projectInfo).forEach(([key, value]) => {
    if(! value) { throw `Could not retrieve project info for key: ${key}` }
  });

  // If a user has a repo with multiple package, I don't have any ways to know which package they want to load
  // for now, let's assume they only have one package with the same name as the repo
  const packageName = repo;

  const githubApiUrlForPullRequests = 'https://api.github.com/repos/' + user + '/' + repo + '/pulls/' + pullRequest + '/commits';
  fetch(githubApiUrlForPullRequests)
    .then(response => {
      if(response.ok) {
        return response.json()
      } else {
        throw `Could not fetch latest commitId for this PR! Response status: ${response.status} for url: ${response.url}`;
      }
    })
    .then(data => data[data.length - 1].sha)
    .then(commitId => {
      const izunaServerUrl = 'https://izuna-server.patchgirl.io/api/projectInfo/' + user + '/' + repo + '/' + packageName + '/' + commitId;
      fetch(izunaServerUrl, { mode: 'cors', credentials: 'omit' })
        .then(response => {
          if (response.ok) {
            return response.json();
          } else {
            throw `Could not fetch izuna project info! Response status: ${response.status} for url: ${response.url}`;
          }
        })
        .then(data => {
          sendResponse(data);
        });
    });

  return true;
}

chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  chrome.tabs.query({ currentWindow: true, active: true }, tabs => fetchIzunaInfo(tabs, sendResponse));
  return true;
});
