import { NumBlob } from './numBlob.js';

/*
 * this service extract parts of the dom for
 * a github pull request page dom (e.g: https://github.com/someUser/someRepo/pull/16/files),
 */
export class PullRequestPageService {

  constructor() {}

  static codeRowClasses = ['blob-code-inner', 'blob-code-marker']
  static lineRowSelector = 'td.blob-code:not(:empty)';

  // On the pull request page, we want to fetch the path of all the files with the given extension (e.g: ".hs" for haskell files)
  getFilesWithExtension(extension) {
    return Array.from(document.querySelectorAll("div.file-header div.file-info a"))
      .map(e => e.textContent)
      .filter(e => e.endsWith(extension));
  }

  // In your browser page, try to find all the file diffs for the given extension
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

  /*
   * Not all diff rows have the same html structure. This function makes sure every row are "normalized"
   */
  normalizeDiff(diffRowsDom, splitMode) {
    return Array.from(diffRowsDom).map (diffRowDom => {
      var lineRows;
      if (splitMode) {
        lineRows = this.getLineRowForSplitMode(diffRowDom);
      } else {
        lineRows = [ this.getLineRowForUnifiedMode(diffRowDom) ];
      }
      Array.from(lineRows).forEach (lineRow => {
        if(lineRow) {
          var codeRow = this.getCodeRow(lineRow);
          // sometimes, a row doesn't have a span.blob... child. In this case, we need to manually add one as a default container
          if(! codeRow) {
            codeRow = document.createElement("span");
            codeRow.classList.add(...PullRequestPageService.codeRowClasses);
            while(lineRow.firstChild) {
              const child = lineRow.firstChild;
              codeRow.appendChild(lineRow.removeChild(child));
            }
            lineRow.appendChild(codeRow);
          }
        }
      })

      return diffRowDom;
    });
  }

  /*
   * contain the code row and line state (addition, deletion, unmodified)
   * eg:
   *
   *    | 8 | + someCode
   *          ‾‾‾‾‾‾‾‾‾‾
   * caveats: this can be null!
   */
  getLineRowForUnifiedMode(diffRowDom) {
    return diffRowDom.querySelector(PullRequestPageService.lineRowSelector);
  }

  /*
   * contain the code row and line state (addition, deletion, unmodified)
   * eg:
   *
   *    | 8 | unmodifiedCode     | 8 | unmodifiedCode
   *          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾
   * caveat: sometimes one of the column is empty
   * eg:
   *    |   |                    | 8 | unmodifiedCode
   *
   *
   */
  getLineRowForSplitMode(diffRowDom) {
    return diffRowDom.querySelectorAll('td.blob-code');
  }

  /*
   * contain the code row and line state (addition, deletion, unmodified)
   * eg:
   *
   *    | 8 | + someCode
   *            ‾‾‾‾‾‾‾‾
   */
  getCodeRow(lineRow) {
    return lineRow.querySelector(`span.${PullRequestPageService.codeRowClasses.join('.')}`);
  }

  /*
   * for the unified mode, there is only one line of code per row, so newCodeNode will be undefined
   *
   *             oldCodeNode
   *             ↓
   * | 8 |   | - someOldCode...
   */
  getCodeRowForUnifiedMode(diffRowDom) {
    const lineRow = this.getLineRowForUnifiedMode(diffRowDom)
    if(!lineRow) {
      return null
    } else {
      return {
        parentNode: lineRow,
        codeNode: this.getCodeRow(lineRow)
      };
    }
  }

  /*
   * in split mode oldCodeNode and newCodeNode represents
   * respectively the code line from the left and the right, i.e:
   *
   *         leftCodeNode            rightCodeNode
   *         ↓                       ↓
   * | 8 | - someOldCode...  | 8 | + someNewCode...
   *
   */
  getCodeRowsForSplitMode(diffRowDom) {
    const [ leftLineRow, rightLineRow ] = Array.from(this.getLineRowForSplitMode(diffRowDom)).map(lineRow => {
      return {
        parentNode: lineRow,
        codeNode: this.getCodeRow(lineRow)
      }
    });

    return {
      leftLineRow: leftLineRow,
      rightLineRow: rightLineRow
    };
  }
}
