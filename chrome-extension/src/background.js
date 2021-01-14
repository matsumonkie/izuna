var cache = {};
main();

chrome.runtime.onInstalled.addListener(() => {
  chrome.storage.sync.set({ enableIzuna: true });
});

function main() {
  chrome.tabs.onUpdated.addListener((tabId, changeInfo, tab) => {
    // enable extension icon when we are on github only
    chrome.declarativeContent.onPageChanged.removeRules(undefined, function() {
      chrome.declarativeContent.onPageChanged.addRules([{
        conditions: [
          new chrome.declarativeContent.PageStateMatcher({
            pageUrl: { hostEquals: 'github.com' },
          })
        ],
        actions: [new chrome.declarativeContent.ShowPageAction()]
      }]);
    });

    chrome.storage.sync.get('enableIzuna', function(result) {
      if(result.enableIzuna) {
        const pathname = githubPullRequestUrlPathName(tabId, changeInfo, tab);
        if(pathname) {
          const pullRequestInfo = getPullRequestInfo(pathname);

          fetchPullRequestCommitsDetails(pullRequestInfo).then(pullRequestDetails => {
            chrome.tabs.sendMessage(tab.id, { cmd: 'whichFiles' }, (files) => {
              getFilesInfo(pullRequestInfo, pullRequestDetails, files).then(payload => {
                console.log("foo", payload);
                chrome.tabs.sendMessage(tab.id, payload, (response) => {});
              });
            });
          });
        }
      }
    });
  })
}

function getFilesInfo(pullRequestInfo, pullRequestDetails, files) {
  const oldFilesInfo = fetchFilesInfo(pullRequestInfo, pullRequestDetails.targetOid, files);
  const newFilesInfo = fetchFilesInfo(pullRequestInfo, pullRequestDetails.commitOids[0], files);
  return Promise.all([oldFilesInfo, newFilesInfo]).then(filesInfo => {
    return {
      oldPackageInfo: filesInfo[0],
      newPackageInfo: filesInfo[1]
    };
  });
}


/* if the current tab is loaded and is a github pull request page, return the url pathname
 *  eg:
 *   input: https://github.com/matsumonkie/izuna-example/pull/7/files
 *   output: /matsumonkie/izuna-example/pull/7/files
 */
function githubPullRequestUrlPathName(tabId, changeInfo, tab) {
  if( changeInfo &&
      changeInfo.status &&
      changeInfo.status === 'complete' &&
      tab.active &&
      tab.status === 'complete') {
    const url = new URL(tab.url);
    if(url.hostname === 'github.com') {
      const pathAction = url.pathname.split('/')[3];
      const prTab = url.pathname.split('/')[5];
      if(pathAction === 'pull' && prTab === 'files') {
        return url.pathname;
      }
    }
  }

  return false;
}

/*
 * when browsing a pull request, we can fetch information related to the PR from the url pathname.
 * eg:
 *   input: /matsumonkie/izuna-example/pull/7/files
 *   output: { owner: matsumonkie, repo: izuna-example, pullRequest: 7, packageName: izuna-example }
 */
function getPullRequestInfo(pathname) {
  const [empty, user, repo, pull, pullRequest, ...tail] = pathname.split("/");

  // If a user has a repo with multiple package, I don't have any ways to know which package they want to load
  // for now, let's assume they only have one package with the same name as the repo
  const pullRequestInfo = {
    user: user,
    repo: repo,
    packageName: repo,
    pullRequest: pullRequest
  };
  Object.entries(pullRequestInfo).forEach(([key, value]) => {
    if(! value) { throw `Could not retrieve pull request info for key: ${key}` }
  });

  return pullRequestInfo;
}

/*
 * from a given PR, find the target commit oid (i.e: the commit we are diffing from)
 * and the latest commit oid (i.e: the latest commit we pushed for this PR)
 */
function fetchPullRequestCommitsDetails(pullRequestInfo) {
  const pullRequestInfoUrl = izunaServerUrl + '/api/pullRequestInfo/' + pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.pullRequest;
  return fetch(pullRequestInfoUrl)
    .then(response => {
      if(response.ok) {
        return response.json()
      } else {
        throw `Could not fetch pull request information for this PR! Response status: ${response.status} for url: ${response.url}`;
      }
    })
}

async function fetchFilesInfo(pullRequestInfo, commitId, files) {
  const url = izunaServerUrl + '/api/projectInfo/' + pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.packageName + '/' + commitId;
//  const izunaServerUrl = 'http://localhost:3001/api/projectInfo/' + pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.packageName + '/' + commitId;

  return fetch( url,
                { method: 'POST',
                  credentials: 'omit',
                  headers: { 'Content-Type': 'application/json' },
                  body: JSON.stringify(files)
                }
              )
    .then(response => {
      if (response.ok) {
        return response.json();
      } else if (response.status === 404) {
        return {};
      } else {
        throw `Could not fetch izuna project info! Response status: ${response.status} for url: ${response.url}`;
      }
    });
}

const izunaServerUrl = 'https://izuna.app';
