/*
 * split every char for a given line of code in a github diff
 * eg:
 *
 *   input:
 *     <span class="whatever" data-code-marker="+">hey</span>
 *
 *   output:
 *     <span class="whatever" data-code-marker="+">
 *       <span>
 *         <span class="izuna-char" data-row="1" data-col="0">h</span>
 *         <span class="izuna-char" data-row="1" data-col="1">e</span>
 *         <span class="izuna-char" data-row="1" data-col="2">y</span>
 *      </span>
 *     </span>
 */
export class Splitter {

  constructor(document = document, Node = Node) {
    this.document = document;
    this.Node = Node;
  }

  static #oddClass = "stest"

  split(docFragment, dom, filePath, state, lineNumber) {
    const foo = (node, charPos) => {
      if(node.nodeType === this.Node.TEXT_NODE) {
        var parent = this.document.createElement("span");
        const characterNodes = Array.from(node.textContent).map((character) => {
          var newChar = this.document.createElement("span");
          newChar.setAttribute('class', 'izuna-char');
          newChar.setAttribute('data-file-path', filePath);
          newChar.setAttribute('data-state', state);
          newChar.setAttribute('data-row', lineNumber);
          newChar.setAttribute('data-col', charPos);
          newChar.textContent = character;
          charPos = charPos + 0.5;

          var newChar2 = this.document.createElement("span");
          newChar2.setAttribute('class', 'izuna-fake-char');
          newChar2.setAttribute('data-file-path', filePath);
          newChar2.setAttribute('data-state', state);
          newChar2.setAttribute('data-row', lineNumber);
          newChar2.setAttribute('data-col', charPos);
          charPos = charPos + 0.5;

          return [newChar, newChar2];
        });
        characterNodes.forEach((characterNodes) => {
          const [charNode1, charNode2] = characterNodes;
          parent.appendChild(charNode1);
          parent.appendChild(charNode2);
        });

        return [parent, charPos];
      } else {
        Array.from(node.childNodes).forEach((child) => {
          const [newChild, newCharPos] = foo(child, charPos)
          charPos = newCharPos;
          child.replaceWith(newChild)
        });

        return [node, charPos];
      }
    }

    var [newNode, _] = foo(dom.cloneNode(true), 0);
    docFragment.appendChild(newNode);

    return docFragment;
  }

  // this is for debug purpose only
  show(splitted, margin = "") {
    if(splitted.nodeType === this.Node.TEXT_NODE) {
      console.log(margin + '[' + splitted.textContent + ']');
    } else {
      console.log(margin + splitted.cloneNode().outerHTML);
    }
    Array.from(splitted.childNodes).forEach((child) => this.show(child, margin + "  "));
  }
}
