import { Constants } from './constants.js';
import { NumBlob } from './numBlob.js';

export class FilesInfo {

  constructor(filesInfo) {
    this.filesInfo = filesInfo;
  }

  findType(filePath, location, state, col, row) {
    var fileState;
    if(location === Constants.LEFT_LOCATION) {
      fileState = this.filesInfo.oldPackageInfo;
    } else if (location === Constants.RIGHT_LOCATION) {
      fileState = this.filesInfo.newPackageInfo;
    } else { // CENTER
      if(state === NumBlob.DELETED) {
        fileState = this.filesInfo.oldPackageInfo;
      } else {
        fileState = this.filesInfo.newPackageInfo;
      }
    }

    if(fileState) {
      const fileInfo = fileState[filePath];
      if(fileInfo) {
        const typeRef = this.findTypeRef(fileInfo.typeRefs[row], col);
        if(typeRef) {
          const specializedType = fileInfo.types[typeRef.specializedType];
          if(specializedType) {
            const spanLength = typeRef.span.colEnd - 1 - typeRef.span.colStart;
            const centerCol = (typeRef.span.colEnd - 1) - (spanLength / 2);
            return {
              startCol: typeRef.span.colStart,
              endCol: typeRef.span.colEnd - 1,
              centerCol: centerCol,
              typeName: specializedType.replace(/ -> /g, ' ⟶ ')
            };
          }
        }
      }
    }
  }

  findTypeRef(typeRefs, col) {
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
