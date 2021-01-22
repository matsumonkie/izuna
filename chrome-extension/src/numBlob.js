class NumBlobClass {

  constructor() {}

  getNumBlobForUnifiedMode(diffRowDom) {
    /*
     * this is similar for the unified mode except that graphically, the number are side to side, i.e:
     *
     *   oldLineNumberDom
     *       newLineNumberDom
     *   ↓   ↓
     * | 8 |   | - someOldCode...
     * |   | 8 | - someNewCode...
     *
     */
    const [ oldLineNumberDom, newLineNumberDom ] = diffRowDom.querySelectorAll(NumBlob.blobNumSelector);

    const oldLineNumber = parseInt(oldLineNumberDom.dataset.lineNumber) - 1;
    const newLineNumber = parseInt(newLineNumberDom.dataset.lineNumber) - 1;
    const lineState = this.getState(oldLineNumberDom);
    var lineNumber;
    if (lineState == NumBlob.DELETED) {
      lineNumber = oldLineNumber;
    } else {
      lineNumber = newLineNumber;
    }

    return {
      lineNumber: lineNumber,
      lineState: lineState
    };
  }

  getNumBlobForSplitMode(diffRowDom) {
    /*
     * in split mode oldLineNumberDom and newLineNumberDom represents
     * respectively the line number from the left and the right, i.e:
     *
     *   leftLine                rightLine
     *   ↓                       ↓
     * | 8 | - someOldCode...  | 8 | + someNewCode...
     */
    const [ leftLine, rightLine ] = Array.from(diffRowDom.querySelectorAll(NumBlob.blobNumSelector)).map(dom => {
      return {
        lineNumber: parseInt(dom.dataset.lineNumber) - 1,
        lineState: this.getState(dom)
      };
    });

    return {
      leftLine: leftLine,
      rightLine: rightLine
    };
  }

  getState(lineDom) {
    const classList = lineDom.classList;
    if(classList.contains('blob-num-addition')) {
      return NumBlob.ADDED;
    } else if (classList.contains('blob-num-deletion')) {
      return NumBlob.DELETED;
    } else {
      return NumBlob.UNMODIFIED;
    }
  }
}

const NumBlob = {
  ADDED: 'ADDED',
  DELETED: 'DELETED',
  UNMODIFIED: 'UNMODIFIED',

  blobNumSelector: 'td.blob-num',
  NumBlobClass: NumBlobClass

};

export { NumBlob };
