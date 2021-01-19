import { Splitter } from './splitter.js';
import { Popper } from './popper.js';
import { FilesInfo } from './filesInfo.js';
import { PullRequestPageService } from './pullRequestPageService.js';
import { Constants } from './constants.js';
import { LineState } from './lineState.js';

chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  const pullRequestPage = new PullRequestPageService(document);
  if(msg.cmd === Constants.CMD_WHICH_FILES) {
    sendResponse(pullRequestPage.getFilesWithExtension(".hs"));
  } else if(msg.cmd === Constants.CMD_IZUNA_INFO) {
    main(msg.payload, pullRequestPage);
    sendResponse({});
  } else {
    console.error(`izuna: Unknown command of from background received in contentScript: ${msg}`);
  }
});

function main (payload, pullRequestPage) {
  console.debug(payload)
  try {
    const diffDoms = pullRequestPage.getDiffsForFileWithExtension(".hs");
    const splitter = new Splitter(document, Node);
    diffDoms.forEach(diffDom => {
      const splitMode = pullRequestPage.isSplitMode(diffDom);
      const filePath = pullRequestPage.getFilePath(diffDom);
      const moduleInfo = {
        oldModuleInfo: payload.oldPackageInfo[filePath],
        newModuleInfo: payload.newPackageInfo[filePath]
      };
      generateIzuna(pullRequestPage, splitter, diffDom, splitMode, filePath, moduleInfo);
      watchDiffForCodeExpansion(pullRequestPage, splitter, diffDom, splitMode, filePath, moduleInfo);
    });
  } catch (error) {
    const message = `izuna: Fatal error occurred, izuna will stop working on this page
Try reloading your page
If the error keep appearing, please report it to https://github.com/matsumonkie/izuna/issues
Error is: ` + error;
    console.error(message);
    return;
  }

  const filesInfo = new FilesInfo(payload);
  const popper = new Popper(filesInfo);
  document.body.appendChild(popper.tooltip);
  popper.mkNotificationEvents()
}

function generateIzuna(pullRequestPage, splitter, diffDom, splitMode, filePath, moduleInfo) {
  if(moduleInfo.oldModuleInfo || moduleInfo.newPackageInfo) {
    const diffRowsDom = pullRequestPage.getRows(filePath, diffDom);
    Array.from(diffRowsDom).forEach (diffRowDom => {
      const line = pullRequestPage.getLineNumberState(diffRowDom, splitMode);
      const codeRow = pullRequestPage.getCodeRow(diffRowDom, splitMode);
      const oldModuleInfo = moduleInfo.oldModuleInfo;
      const newModuleInfo = moduleInfo.newModuleInfo;

      if(splitMode) { // split mode, we need to handle 2 rows, one for the old diff and one for the new diff
        //generateElmAppForRow(filePath, codeNode.oldCodeNode, line.old.lineState, oldModuleInfo, line.old.lineNumber);
        //generateElmAppForRow(filePath, codeNode.newCodeNode, line.new.lineState, newModuleInfo, line.new.lineNumber);
      } else { // unified mode, we only need to handle 1 row
        var whichModuleInfo;
        if(line.lineState === LineState.ADDED) {
          whichModuleInfo = newModuleInfo;
        } else {
          whichModuleInfo = oldModuleInfo;
        }
        handleCodeRow(splitter, filePath, codeRow.parentNode, codeRow.codeNode, line.lineState, whichModuleInfo, line.lineNumber);
      }

    });
  }
}

function handleCodeRow(splitter, filePath, parentNode, codeNode, lineState, whichModuleInfo, lineNumber) {
  const newCodeNode = splitter.split(new DocumentFragment(), codeNode, filePath, lineState, lineNumber);
  parentNode.replaceChild(newCodeNode, codeNode);
}

/*
 * on github, only the modified part of a file gets shown. If the user wants to see more, he can click on the expand button.
 * We watch the whole diff file and anytime child nodes are being added, we re-run izuna on the whole file
 */
function watchDiffForCodeExpansion (pullRequestPage, splitter, diffDom, splitMode, filePath, moduleInfo) {
  const codeDiffDom = diffDom.querySelector('table.diff-table tbody');
  const config = { attributes: false, childList: true, subtree: false };

  const callback = function(mutationsList, observer) {
    mutationsList.forEach (mutation => {
      if (mutation.type === 'childList') {
        generateIzuna(pullRequestPage, splitter, diffDom, splitMode, filePath, moduleInfo);
      }
    });
  };

  const observer = new MutationObserver(callback);
  observer.observe(codeDiffDom, config);
}
