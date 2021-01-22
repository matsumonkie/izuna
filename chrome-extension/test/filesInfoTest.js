const equal = require('chai').assert.equal;
import { FilesInfo } from '../src/filesInfo.js';

describe('FilesInfo', function() {
  describe('#find()', function() {

    const info = {
      oldPackageInfo: {
        someFile: {
          typeRefs: {
            42: [
              {
                generalizedType: null,
                specializedType: 0,
                span: {
                  lineStart: 42,
                  lineEnd: 42,
                  colStart: 1,
                  colEnd: 5
                },
                children: [
                  {
                    generalizedType: null,
                    specializedType: 1,
                    span: {
                      lineStart: 42,
                      lineEnd: 42,
                      colStart: 3,
                      colEnd: 4
                    },
                    children: []
                  }
                ]
              }
            ]
          },
          types: {
            0: "Int",
            1: "String"
          }
        }
      }
    };
    const filesInfo = new FilesInfo(info);

    it('returns the typename', function() {
      const { typeName } = filesInfo.findType("someFile", 'DELETED', 2, 42);
      equal(typeName, "Int");
    });

    it('returns null if location doesn\'t contain a type', function() {
      const res = filesInfo.findType("someFile", 'DELETED', 6, 42);
      equal(res, null);
    });

    it('returns child if its location is more specific', function() {
      const { typeName } = filesInfo.findType("someFile", 'DELETED', 3, 42);
      equal(typeName, "String");
    });
  });
});
