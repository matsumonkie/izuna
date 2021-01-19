import { LineState } from './lineState.js';

export class FilesInfo {

  constructor(filesInfo) {
    this.filesInfo = filesInfo;
  }

  findType(filePath, state, col, row) {
    var fileState;
    if(state === LineState.ADDED) {
      fileState = this.filesInfo.newPackageInfo;
    } else {
      fileState = this.filesInfo.oldPackageInfo;
    }

    if(fileState) {
      const fileInfo = fileState[filePath];
      if(fileInfo) {
        const typeRef = this.findTypeRef(fileInfo.typeRefs[row], col, row);
        if(typeRef) {
          const specializedType = fileInfo.types[typeRef.specializedType];
          if(specializedType) {
            const spanLength = typeRef.span.colEnd - 1 - typeRef.span.colStart;
            const centerCol = (typeRef.span.colEnd - 1) - (spanLength / 2);
            return {
              centerCol: centerCol,
              typeName: specializedType.replace(/ -> /g, " ⟶ ")
            }
          }
        }
      }
    }
  }

  findTypeRef(typeRefs, col, row) {
    if(typeRefs) {

      const moreSpecialized = (a, b) => {
        if(!a) {
          return b;
        } else if(!b) {
          return a;
        } else {
          if((a.span.colEnd - a.span.colStart) > (b.span.colEnd - b.span.colStart)) {
            return b;
          } else {
            return a;
          }
        }
      };

      const f = (acc, e) => {
        if(col >= e.span.colStart && col < e.span.colEnd) {
          return moreSpecialized(moreSpecialized (acc, e), e.children.reduce(f, acc));
        } else {
          return acc;
        }
      };

      return typeRefs.reduce(f, null);
    } else {
      return null;
    }
  }
}
