import { createPopper } from '@popperjs/core';
import { Splitter } from './splitter.js';
import { PullRequestPageService } from './pullRequestPageService.js';

chrome.runtime.onMessage.addListener((payload, sender, sendResponse) => {
  const pullRequestPage = new PullRequestPageService(document);
  if(payload.cmd === 'whichFiles') {
    sendResponse(pullRequestPage.getFilesWithExtension(".hs"));
  } else {
    console.debug(payload)
    main(payload, pullRequestPage);
    sendResponse({});
  }
});

function main (payload, pullRequestPage) {
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

  const tooltip = mkPopper();
  document.body.appendChild(tooltip);
  mkNotificationEvents(tooltip);
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
        if(line.lineState === "ADDED") {
          whichModuleInfo = newModuleInfo;
        } else {
          whichModuleInfo = oldModuleInfo;
        }
        handleCodeRow(splitter, filePath, codeRow.codeNode, line.lineState, whichModuleInfo, line.lineNumber);
      }

    });
  }
}

function handleCodeRow(splitter, filePath, codeNode, lineState, whichModuleInfo, lineNumber) {
  const newCodeNode = splitter.split(new DocumentFragment(), codeNode, lineNumber);
  //console.log(newCodeNode);
  // split code
  // split code
}

//  make an elm application for a given row that will handle the display
/*function generateRow(filePath, moduleInfo, splitMode, diffRowDom) {
  const line = getLineNumberStates(diffRowDom, splitMode)
  const codeNode = getCodeNode(diffRowDom, splitMode);
  const oldModuleInfo = moduleInfo.oldModuleInfo;
  const newModuleInfo = moduleInfo.newModuleInfo;

  if(splitMode) { // split mode, we need to generate 2 new rows, one for the old diff and one for the new diff
  generateElmAppForRow(filePath, codeNode.oldCodeNode, line.old.lineState, oldModuleInfo, line.old.lineNumber);
  generateElmAppForRow(filePath, codeNode.newCodeNode, line.new.lineState, newModuleInfo, line.new.lineNumber);
  } else { // unified mode, we only need to generate 1 new row
  if(line.lineState === "ADDED") {
  whichModuleInfo = newModuleInfo;
  } else {
  whichModuleInfo = oldModuleInfo;
  }

  generateElmAppForRow(filePath, codeNode.codeNode, line.lineState, whichModuleInfo, line.lineNumber);
  }
  }*/


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


//  create a popper notification to be used later on
function mkPopper () {
  var arrow = document.createElement("div");
  arrow.setAttribute("id", "arrow");
  arrow.setAttribute("data-popper-arrow", "");

  var tooltipText = document.createElement("div");
  tooltipText.setAttribute("id", "tooltipText");

  var tooltip = document.createElement("pre");
  tooltip.setAttribute("id", "tooltip");
  tooltip.setAttribute("role", "tooltip");
  tooltip.appendChild(tooltipText)
  tooltip.appendChild(arrow)

  return tooltip;
}


// attach a display notification event on all code span that have type annotations
function mkNotificationEvents(tooltip) {
  let popperInstance = null;

  const tooltipOptions = {
    placement: 'top',
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [0, 8],
        },
      },
    ],
  };

  document.querySelectorAll("span[data-specialized-type]").forEach(span => {
    span.addEventListener("mouseover", event => {
      popperInstance = createPopper(span, tooltip, tooltipOptions);
      tooltip.querySelector('#tooltipText').innerHTML = span.dataset.specializedType.replaceAll(" -> ", " ⟶ ");
      tooltip.setAttribute('data-show', '');
    });

    span.addEventListener("mouseleave", event => {
      tooltip.removeAttribute('data-show');
      if (popperInstance) {
        popperInstance.destroy();
        popperInstance = null;
      }
    });
  });
}
