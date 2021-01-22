/*global chrome*/

import { Splitter } from './splitter.js';
import { Popper } from './popper.js';
import { FilesInfo } from './filesInfo.js';
import { PullRequestPageService } from './pullRequestPageService.js';
import { Constants } from './constants.js';
import { NumBlob } from './numBlob.js';

chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  const pullRequestPage = new PullRequestPageService();
  if(msg.cmd === Constants.CMD_WHICH_FILES) {
    sendResponse(pullRequestPage.getFilesWithExtension('.hs'));
  } else if(msg.cmd === Constants.CMD_IZUNA_INFO) {
    main(msg.payload, pullRequestPage);
    sendResponse({});
  } else {
    console.error(`izuna: Unknown command of from background received in contentScript: ${msg}`);
  }
});

function main (payload, pullRequestPage) {
  console.debug(payload);
  const diffDoms = pullRequestPage.getDiffsForFileWithExtension('.hs');
  const splitter = new Splitter.SplitterClass(document, Node);
  const filesInfo = new FilesInfo(payload);
  const popper = new Popper(filesInfo);
  const numBlob = new NumBlob.NumBlobClass();
  diffDoms.forEach(diffDom => {
    const isHidden = pullRequestPage.diffHidden(diffDom);
    if(isHidden) {
      watchForLoadDiff(pullRequestPage, splitter, numBlob, popper, payload, diffDom, isHidden);
    } else {
      handleDiff(pullRequestPage, splitter, numBlob, popper, payload, diffDom);
    }
  });
  document.body.appendChild(popper.tooltip);
}

function handleDiff(pullRequestPage, splitter, numBlob, popper, payload, diffDom) {
  const splitMode = pullRequestPage.isSplitMode(diffDom);
  const filePath = pullRequestPage.getFilePath(diffDom);
  const moduleInfo = {
    oldModuleInfo: payload.oldPackageInfo[filePath],
    newModuleInfo: payload.newPackageInfo[filePath]
  };
  generateIzuna(pullRequestPage, splitter, numBlob, popper, diffDom, splitMode, filePath, moduleInfo);
  watchDiffForCodeExpansion(pullRequestPage, splitter, numBlob, popper, diffDom, splitMode, filePath, moduleInfo);
}

/*
 * Sometimes Github doesn't show a diff file. There is a `Load diff` button instead.
 */
function watchForLoadDiff (pullRequestPage, splitter, numBlob, popper, payload, diffDom, isHiddenDom) {
  const isHiddenContainer = document.querySelector('div.js-diff-load-container');
  const config = { attributes: false, childList: true, subtree: false };

  const callback = function(mutationsList) {
    mutationsList.forEach (mutation => {
      const removedNodes = Array.from(mutation.removedNodes);
      console.log(mutation, removedNodes);
      const deletedDirectly = removedNodes.indexOf(isHiddenDom) > -1;
      const deletedByAParent = removedNodes.some(parent => parent.contains(isHiddenDom));
      if(deletedDirectly || deletedByAParent)
        handleDiff(pullRequestPage, splitter, numBlob, popper, payload, diffDom);
    });
  };

  const observer = new MutationObserver(callback);
  observer.observe(isHiddenContainer, config);
}

function generateIzuna(pullRequestPage, splitter, numBlob, popper, diffDom, splitMode, filePath, moduleInfo) {
  if(moduleInfo.oldModuleInfo || moduleInfo.newPackageInfo) {
    var diffRowsDom = pullRequestPage.getRows(filePath, diffDom);
    diffRowsDom = pullRequestPage.normalizeDiff(diffRowsDom, splitMode);
    Array.from(diffRowsDom).forEach (diffRowDom => {
      const oldModuleInfo = moduleInfo.oldModuleInfo;
      const newModuleInfo = moduleInfo.newModuleInfo;

      if(splitMode) { // split mode, we need to handle 2 rows, one for the old diff and one for the new diff
        const line = numBlob.getNumBlobForSplitMode(diffRowDom);
        const codeRows = pullRequestPage.getCodeRowsForSplitMode(diffRowDom);

        handleCodeRow(splitter, filePath, codeRows.leftLineRow.parentNode, codeRows.leftLineRow.codeNode, Constants.LEFT_LOCATION, line.leftLine.lineState, oldModuleInfo, line.leftLine.lineNumber);
        handleCodeRow(splitter, filePath, codeRows.rightLineRow.parentNode, codeRows.rightLineRow.codeNode, Constants.RIGHT_LOCATION, line.rightLine.lineState, newModuleInfo, line.rightLine.lineNumber);
      } else { // unified mode, we only need to handle 1 row
        const line = numBlob.getNumBlobForUnifiedMode(diffRowDom);
        const codeRow = pullRequestPage.getCodeRowForUnifiedMode(diffRowDom);
        if(codeRow) {
          var whichModuleInfo;
          if(line.lineState === NumBlob.ADDED) {
            whichModuleInfo = newModuleInfo;
          } else {
            whichModuleInfo = oldModuleInfo;
          }
          handleCodeRow(splitter, filePath, codeRow.parentNode, codeRow.codeNode, Constants.CENTER_LOCATION, line.lineState, whichModuleInfo, line.lineNumber);
        }
      }
    });
    popper.mkNotificationEvents(diffDom);
  }
}

function handleCodeRow(splitter, filePath, parentNode, codeNode, location, lineState, whichModuleInfo, lineNumber) {
  if(codeNode && lineNumber) {
    const newCodeNode = splitter.split(new DocumentFragment(), codeNode, filePath, location, lineState, lineNumber);
    parentNode.replaceChild(newCodeNode, codeNode);
  }
}

/*
 * on github, only the modified part of a file gets shown. If the user wants to see more, he can click on the expand button.
 * We watch the whole diff file and anytime child nodes are being added, we re-run izuna on the whole file
 */
function watchDiffForCodeExpansion (pullRequestPage, splitter, numBlob, popper, diffDom, splitMode, filePath, moduleInfo) {
  const codeDiffDom = diffDom.querySelector('table.diff-table tbody');
  const config = { attributes: false, childList: true, subtree: false };

  const callback = function(mutationsList) {
    mutationsList.forEach (mutation => {
      if (mutation.type === 'childList') {
        generateIzuna(pullRequestPage, splitter, numBlob, popper, diffDom, splitMode, filePath, moduleInfo);
      }
    });
  };

  const observer = new MutationObserver(callback);
  observer.observe(codeDiffDom, config);
}
