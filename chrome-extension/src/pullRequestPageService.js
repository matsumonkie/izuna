/*
 * this service extract parts of the dom for
 * a github pull request page dom (e.g: https://github.com/someUser/someRepo/pull/16/files),
 */
export class PullRequestPageService {

  constructor() {
  }

  // On the pull request page, we want to fetch the path of all the files with the given extension (e.g: ".hs" for haskell files)
  getFilesWithExtension(extension) {
    return Array.from(document.querySelectorAll("div.file-header div.file-info a"))
      .map(e => e.textContent)
      .filter(e => e.endsWith(extension));
  }

  // In your browser page, try to find all the file diffs for Haskell code
  getDiffsForFileWithExtension(extension) {
    const diffDoms = document.querySelectorAll(`div.file[data-file-type='${extension}']`);
    if (diffDoms.length === 0) {
      console.debug(`izuna: Could not find any diff for files with extension: "${extension}" code on this page.`);
    }
    return diffDoms;
  }

  // whether we are diffing in split or unified mode
  isSplitMode(diffDom) {
    return !!diffDom.querySelector("table.file-diff-split");
  }

  /*
   * Given a diff dom, extract its file path
   * eg:
   *  input:
   *   +-------------------------------------------------------------------+
   *   |  34 [][][][] myProject/src/main.js                                |
   *   +-------------------------------------------------------------------+
   *   | 1 | + print("coucou");                                            |
   *   | 2 | - print(1 + 1);                                               |
   *   | 3 | - return 0;                                                   |
   *   +-------------------------------------------------------------------+
   *
   * output:
   *   myProject/src/main.js
   */
  getFilePath(diffDom) {
    const filePathDom = diffDom.querySelector(".file-header .file-info a[title]")
    if(filePathDom && filePathDom.text) {
      return filePathDom.text
    } else {
      throw `Could not fetch file name!`;
    }
  }

  // extract each rows of code for a given file diff
  getRows(filePath, diffDom) {
    // `tr[data-hunk]` are the original rows where as `tr.blob-expanded` are the rows that have been manually expanded by the user
    const diffRowsDom = diffDom.querySelector('tbody').querySelectorAll('tr[data-hunk], tr.blob-expanded');
    if (diffRowsDom.length === 0) {
      console.warn(`izuna: Could not find any diff rows for ${filePath}, this will not affect izuna but is probably an error!`);
    }
    return diffRowsDom;
  }

  // for a given row, return both old/new line number and the line state (addition, deletion, unmodified)
  getLineNumberState(diffRowDom, splitMode) {
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
        old: {
          lineNumber: oldLineNumber,
          lineState: this.getLineState(oldLineNumberDom)
        },
        new: {
          lineNumber: newLineNumber,
          lineState: this.getLineState(newLineNumberDom)
        }
      };
    } else { // unified mode
      const lineState = this.getLineState(oldLineNumberDom);
      var lineNumber;
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

  getLineState(lineDom) {
    const classList = lineDom.classList
    if(classList.contains("blob-num-addition")) {
      return "ADDED";
    } else if (classList.contains("blob-num-deletion")) {
      return "DELETED";
    } else {
      return "UNMODIFIED";
    }
  }

  getCodeRow(diffRowDom, splitMode) {
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
    const parentNode = diffRowDom.querySelector('td:not(:empty)');
    const [ oldCodeNode, newCodeNode ] = diffRowDom.querySelectorAll('span.blob-code-inner.blob-code-marker');
    if(splitMode) {
      return {
        parentNode: parentNode,
        oldCodeNode: oldCodeNode,
        newCodeNode: newCodeNode
      };
    } else {
      return {
        parentNode: parentNode,
        codeNode: oldCodeNode
      };
    }
  }
}
