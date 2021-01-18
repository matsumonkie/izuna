export class FilesInfo {

  constructor(filesInfo) {
    this.filesInfo = filesInfo;
  }

  findType(filePath, state, col, row) {
    var fileState;
    if(state === 'ADDED') {
      fileState = this.filesInfo.newPackageInfo;
    } else {
      fileState = this.filesInfo.oldPackageInfo;
    }

    if(fileState) {
      const fileInfo = fileState[filePath];
      if(fileInfo) {
        const typeRef = this.findTypeRef(fileInfo.typeRefs, col, row);
        if(typeRef) {
          const specializedType = fileInfo.types[typeRef.specializedType];
          if(specializedType) {
            return specializedType.replace(/ -> /g, " ⟶ ");
          }
        }
      }
    }
  }

  findTypeRef(typeRefs, col, row) {
    const typeRefsInRow = typeRefs[row]
    if(typeRefsInRow) {
      return typeRefsInRow.find(e => col >= e.span.colStart && col <= e.span.colEnd)
    } else {
      return null;
    }
  }
}
