/*global chrome*/

import { IzunaServerService } from './izunaServerService.js';
import { Constants } from './constants.js';
import { Cache } from './cache.js';

chrome.runtime.onInstalled.addListener(() => {
  var keyValue = {};
  keyValue[Constants.ENABLE_IZUNA_KEY] = true;
  chrome.storage.sync.set(keyValue);
});

chrome.tabs.onUpdated.addListener(debounce((tabId, changeInfo, tab) => {
  chrome.declarativeContent.onPageChanged.removeRules(undefined, () => {
    chrome.declarativeContent.onPageChanged.addRules([{
      conditions: [
        new chrome.declarativeContent.PageStateMatcher({
          pageUrl: { hostEquals: 'github.com' }, // enable extension icon when we are on github.com only
        })
      ],
      actions: [
        new chrome.declarativeContent.ShowPageAction()
      ]
    }]);
  });

  checkTabUpdate(tabId, changeInfo, tab);
}, 1000));

function debounce(callback, delay) {
  var timer;
  return function() {
    var args = arguments;
    var context = this;
    clearTimeout(timer);
    timer = setTimeout(() => { callback.apply(context, args);  }, delay);
  };
}

function checkTabUpdate(tabId, changeInfo, tab) {
  if(isTabLoaded(changeInfo, tab)) {
    const pullRequestInfo = getGithubPullRequestInfo(tab);
    if(pullRequestInfo) {
      main(Cache, tabId, pullRequestInfo);
    }
  }
}

function isTabLoaded(changeInfo, tab) {
  const url = new URL(tab.url);
  return (
    changeInfo &&
      changeInfo.status &&
      changeInfo.status === 'complete' &&
      tab.active &&
      tab.status === 'complete' &&
      url.hostname === 'github.com'
  );
}

function main(cache, tabId, pullRequestInfo) {
  chrome.storage.sync.get(Constants.ENABLE_IZUNA_KEY, (result) => {
    if(result[Constants.ENABLE_IZUNA_KEY]) {
      const izunaServerService = new IzunaServerService(Constants.IZUNA_HOST_URL, pullRequestInfo);

      izunaServerService.fetchPullRequestCommitsDetails(pullRequestInfo).then(pullRequestDetails => {
        chrome.tabs.sendMessage(tabId, { cmd: Constants.PULL_REQUEST_DETAILS_FETCHED }, (files) => {
          izunaServerService.fetchFilesInfo(pullRequestDetails, files).then(payload => {
            chrome.tabs.sendMessage(tabId, { cmd: Constants.FILES_INFO_FETCHED, payload: payload }, () => {});
          });
        });
      });
    }
  });
}

/* return the pr info
 *  eg:
 *   input: https://github.com/matsumonkie/izuna-example/pull/7/files
 *   output: { owner: matsumonkie,
 *             repo: izuna-example,
 *             pullRequest: 7
 *           }
 */
function getGithubPullRequestInfo(tab) {
  const url = new URL(tab.url);
  const pathAction = url.pathname.split('/')[3];
  const prTab = url.pathname.split('/')[5];
  if(pathAction === 'pull' && prTab === 'files') {
    const params = url.pathname.split('/');
    const pullRequestInfo = {
      user: params[1],
      repo: params[2],
      pr: params[4]
    };
    Object.entries(pullRequestInfo).forEach(([key, value]) => {
      if(! value) { throw `Could not retrieve pull request info for key: ${key}`; }
    });

    return pullRequestInfo;
  }

  return false;
}
