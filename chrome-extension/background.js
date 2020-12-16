main();

function main() {
  chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
    chrome.tabs.query({ currentWindow: true, active: true }, tabs => {
      const pullRequestInfo = getPullRequestInfo(tabs);
      fetchPullRequestCommitsDetails(pullRequestInfo).then(pullRequestDetails => {
        const oldPackageInfo = fetchPackageInfo(pullRequestInfo, pullRequestDetails.targetOid);
        const newPackageInfo = fetchPackageInfo(pullRequestInfo, pullRequestDetails.commitOids[0]);

        Promise.all([oldPackageInfo, newPackageInfo])
          .then(packageInfo => sendResponse(
            {
              oldPackageInfo: packageInfo[0],
              newPackageInfo: packageInfo[1]
            }
          ));
      });
    });
    return true;
  });
}

/*
 * when browsing a pull request, we can fetch information related to the PR from the url.
 eg: https://github.com/matsumonkie/izuna-example/pull/7/files
 gives us: the owner, the repo and the PR number
*/
function getPullRequestInfo(tabs) {
  const [user, repo, pull, pullRequest, ...tail] = tabs[0].url.slice("https://github.com/".length).split("/");

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
