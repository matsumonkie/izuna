main();

var cache = {};

function main() {
  chrome.tabs.onUpdated.addListener((tabId, changeInfo, tab) => {
    const pathname = githubPullRequestUrlPathName(tabId, changeInfo, tab);
    if(pathname) {
      const pullRequestInfo = getPullRequestInfo(pathname);

      fetchPullRequestCommitsDetails(pullRequestInfo).then(pullRequestDetails => {
        getPackageInfo(pullRequestInfo, pullRequestDetails).then(packageInfo => {
          storeInCache(pullRequestInfo, pullRequestDetails, packageInfo);
          const payload = {
            oldPackageInfo: packageInfo[0],
            newPackageInfo: packageInfo[1]
          }

          chrome.tabs.sendMessage(tab.id, payload, (response) => {});
        });
      });
    }
  })
}

function storeInCache(pullRequestInfo, pullRequestDetails, packageInfo) {
  const cached = getInCache(pullRequestInfo, pullRequestDetails);
  if(! cached) {
    const now = Math.floor(Date.now() / 1000); // timestamp in seconds
    cache[ prCacheKey(pullRequestInfo, pullRequestDetails) ] = {
      timestamp: now,
      packageInfo: packageInfo
    };
  }
}

function getInCache(pullRequestInfo, pullRequestDetails) {
  const now = Math.floor(Date.now() / 1000);
  const cached = cache[ prCacheKey(pullRequestInfo, pullRequestDetails) ];
  if(cached) {
    // more than 10s ago
    const delayExhausted = (now - cached.timestamp) > 10;
    console.log((now - cached.timestamp));
    if(delayExhausted) {
      cache[ prCacheKey(pullRequestInfo, pullRequestDetails) ] = null;
      return null;
    } else {
      return cached.packageInfo;
    }
  } else {
    return null;
  }
}

// we use a cache to make sure we don't flood izuna server
function getPackageInfo(pullRequestInfo, pullRequestDetails) {
  const cached = getInCache(pullRequestInfo, pullRequestDetails);
  if (cached) {
    return Promise.resolve(cached);
  } else {
    const oldPackageInfo = fetchPackageInfo(pullRequestInfo, pullRequestDetails.targetOid);
    const newPackageInfo = fetchPackageInfo(pullRequestInfo, pullRequestDetails.commitOids[0]);
    return Promise.all([oldPackageInfo, newPackageInfo]);
  }
}


function prCacheKey(pullRequestInfo, pullRequestDetails) {
  return pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.packageName + '/' + pullRequestInfo.pullRequest + '/' + pullRequestDetails.targetOid + '/' + pullRequestDetails.commitOids[0];
}


// return the url pathname if the current tab is loaded and is a github pull request page
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
        return url.pathname
      }
    }
  }

  return false;
}

/*
 * when browsing a pull request, we can fetch information related to the PR from the url pathname.
 eg: /matsumonkie/izuna-example/pull/7/files
 gives us: the owner, the repo and the PR number
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
  const pullRequestInfoUrl = 'https://izuna-server.patchgirl.io/api/pullRequestInfo/' + pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.pullRequest;
  return fetch(pullRequestInfoUrl)
    .then(response => {
      if(response.ok) {
        return response.json()
      } else {
        throw `Could not fetch pull request information for this PR! Response status: ${response.status} for url: ${response.url}`;
      }
    })
}

async function fetchPackageInfo(pullRequestInfo, commitId) {
  const izunaServerUrl = 'https://izuna-server.patchgirl.io/api/projectInfo/' + pullRequestInfo.user + '/' + pullRequestInfo.repo + '/' + pullRequestInfo.packageName + '/' + commitId;

  return fetch(izunaServerUrl, { mode: 'cors', credentials: 'omit' })
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
