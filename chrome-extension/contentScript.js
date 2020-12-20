chrome.runtime.onMessage.addListener((payload, sender, sendResponse) => main(payload));

function main (payload) {
  console.debug(payload)

  try {
    const haskellDiffDoms = getAllHaskellDiffDom();
    haskellDiffDoms.forEach(diffDom => {
      const splitMode = isSplitMode(diffDom);
      const filePath =  getFilePath(diffDom);
      const moduleInfo = {
        oldModuleInfo: payload.oldPackageInfo[filePath],
        newModuleInfo: payload.newPackageInfo[filePath]
      };
      generateIzuna(diffDom, splitMode, filePath, moduleInfo);
      watchDiffForCodeExpansion(diffDom, splitMode, filePath, moduleInfo);
    });
  } catch (error) {
    console.error("izuna: Fatal error occurred, izuna will stop working on this page")
    console.error("izuna: Try reloading your page")
    console.error("izuna: Please report the error below to https://github.com/matsumonkie/izuna/issues")
    console.error("izuna: " + error)
    return;
  }

  const tooltip = createPopper();
  document.body.appendChild(tooltip);
  mkNotificationEvents(tooltip);
}

function generateIzuna(diffDom, splitMode, filePath, moduleInfo) {
  if(moduleInfo.oldModuleInfo || moduleInfo.newPackageInfo) {
    const diffRowsDom = getDiffRowsDom(filePath, diffDom);
    Array.from(diffRowsDom).forEach (diffRowDom => generateRow(filePath, moduleInfo, splitMode, diffRowDom));
  }
}

/*
 * on github, only the modified part of a file gets shown. If the user wants to see more, he can click on the expand button.
 * We watch the whole diff file and anytime child nodes are being added, we re-run izuna on the whole file
 */
function watchDiffForCodeExpansion (diffDom, splitMode, filePath, moduleInfo) {
  const codeDiffDom = diffDom.querySelector('table.diff-table tbody');
  const config = { attributes: false, childList: true, subtree: false };

  const callback = function(mutationsList, observer) {
    mutationsList.forEach (mutation => {
      if (mutation.type === 'childList') {
        generateIzuna(diffDom, splitMode, filePath, moduleInfo);
      }
    });
  };

  const observer = new MutationObserver(callback);
  observer.observe(codeDiffDom, config);
}


// In your browser page, try to find all the file diffs for Haskell code
function getAllHaskellDiffDom() {
  const haskellDiffDoms = document.querySelectorAll("div.file[data-file-type='.hs']");
  if (haskellDiffDoms.length === 0) {
    console.debug("izuna: Could not find any diff of Haskell code on this page.");
  }
  return haskellDiffDoms;
}


// whether we are diffing in split or unified mode
function isSplitMode(diffDom) {
  return !!diffDom.querySelector("table.file-diff-split");
}


// Given a diff dom, extract its file path
function getFilePath(diffDom) {
  const filePathDom = diffDom.querySelector(".file-header .file-info a[title]")
  if(filePathDom && filePathDom.text) {
    return filePathDom.text
  } else {
    throw `Could not fetch file name!`;
  }
}


// extract each diffing rows for a given file diff
function getDiffRowsDom(filePath, diffDom) {
  // `tr[data-hunk]` are the original rows where as `tr.blob-expanded` are the rows that have been manually requested by the user
  const diffRowsDom = diffDom.querySelector('tbody').querySelectorAll('tr[data-hunk], tr.blob-expanded');
  if (diffRowsDom.length === 0) {
    console.warn(`izuna: Could not find any diff rows for ${filePath}, this will not affect izuna but is probably an error!`);
  }
  return diffRowsDom;
}


//  make an elm application for a given row that will handle the display
function generateRow(filePath, moduleInfo, splitMode, diffRowDom) {
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
}


// for a given row, return both old/new line number and the line state (addition, deletion, unmodified)
function getLineNumberStates(diffRowDom, splitMode) {
  /*
   * in split mode oldLineNumberDom and newLineNumberDom represents
   * respectively the line number from the left and the right, i.e:
   *
   *   oldLineNumberDom        newLineNumberDom
   *   ↓                       ↓
   * | 8 | - someOldCode...  | 8 | + someNewCode...
   *
   *
   * this is similar for the unified mode except that graphically, the number are side to side, i.e:
   *
   *   oldLineNumberDom
   *       newLineNumberDom
   *   ↓   ↓
   * | 8 |   | - someOldCode...
   * |   | 8 | - someNewCode...
   *
   */
  const [ oldLineNumberDom, newLineNumberDom ] = diffRowDom.querySelectorAll('td.blob-num');

  const oldLineNumber = parseInt(oldLineNumberDom.dataset.lineNumber) - 1;
  const newLineNumber = parseInt(newLineNumberDom.dataset.lineNumber) - 1;

  if (splitMode) {
    return {
      old:
      {
        lineNumber: oldLineNumber,
        lineState: getLineState(oldLineNumberDom)
      },
      new:
      {
        lineNumber: newLineNumber,
        lineState: getLineState(newLineNumberDom)
      }
    };
  } else { // unified mode
    const lineState = getLineState(oldLineNumberDom);
    if (lineState == "DELETED") {
      lineNumber = oldLineNumber;
    } else {
      lineNumber = newLineNumber;
    }

    return {
      lineNumber: lineNumber,
      lineState: lineState
    };
  }
}


function getCodeNode(diffRowDom, splitMode) {
  /*
   * in split mode oldCodeNode and newCodeNode represents
   * respectively the code line from the left and the right, i.e:
   *
   *         oldCodeNode             newCodeNode
   *         ↓                       ↓
   * | 8 | - someOldCode...  | 8 | + someNewCode...
   *
   *
   * for the unified mode, there is only one line of code per row, so newCodeNode will be undefined
   *
   *             oldCodeNode
   *             ↓
   * | 8 |   | - someOldCode...
   *
   */
  const [ oldCodeNode, newCodeNode ] = diffRowDom.querySelectorAll('td.blob-code');
  if(splitMode) {
    return {
      oldCodeNode: oldCodeNode,
      newCodeNode: newCodeNode
    };
  } else {
    return { codeNode: oldCodeNode };
  }
}


function getLineState(lineDom) {
  const classList = lineDom.classList
  if(classList.contains("blob-num-addition")) {
    return "ADDED";
  } else if (classList.contains("blob-num-deletion")) {
    return "DELETED";
  } else {
    return "UNMODIFIED";
  }
}


function generateElmAppForRow(filePath, node, lineState, moduleInfo, lineNumber) {
  var lineDom = undefined;
  if( moduleInfo &&
      moduleInfo["fileContent"] &&
      moduleInfo["fileContent"][lineNumber] !== undefined) {
    lineDom = moduleInfo["fileContent"][lineNumber];
  }

  if( lineDom !== undefined && node !== undefined) {
    const flags = {
      state: lineState,
      izunaLine: lineDom,
      githubLine: node.outerHTML
    };
    Elm.Main.init({ flags: flags, node: node });
  } else {
    console.error(`Could not fetch ${lineState} line: ${lineNumber + 1} for file: ${filePath}, defaulting to github content :-(`);
  }
}


//  create a popper notification to be used later on
function createPopper () {
  arrow = document.createElement("div");
  arrow.setAttribute("id", "arrow");
  arrow.setAttribute("data-popper-arrow", "");

  tooltipText = document.createElement("div");
  tooltipText.setAttribute("id", "tooltipText");

  tooltip = document.createElement("pre");
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
      popperInstance = Popper.createPopper(span, tooltip, tooltipOptions);
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
