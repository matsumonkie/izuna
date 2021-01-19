export class LineState {

  constructor() {}

  static getState(lineDom) {
    const classList = lineDom.classList
    if(classList.contains("blob-num-addition")) {
      return LineState.ADDED;
    } else if (classList.contains("blob-num-deletion")) {
      return LineState.DELETED;
    } else {
      return LineState.UNMODIFIED;
    }
  }

  static ADDED = 'ADDED';
  static DELETED = 'DELETED';
  static UNMODIFIED = 'UNMODIFIED';
}
