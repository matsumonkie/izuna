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
 *         <span class="izuna-char"      data-row="1" data-col="0"  >h</span>
 *         <span class="izuna-fake-char" data-row="1" data-col="0.5"></span>
 *         <span class="izuna-char"      data-row="1" data-col="1"  >e</span>
 *         <span class="izuna-fake-char" data-row="1" data-col="1.5"></span>
 *         <span class="izuna-char"      data-row="1" data-col="2"  >y</span>
 *      </span>
 *     </span>
 *
 * Note that the span with the "izuna-fake-char" class are only here so we can correctly positionate the notification.
 * When your text element is even, we can still set its center with this trick
 * e.g:
 *       center is easy to find
 *                  ↓
 *      when odd: "hey"
 *
 * center is not easy to find, we need to add an element between 'h' and 'o' that will behave as its center
 * that's the purpose of izuna-fake-char span
 *                  ↓
 *      when even: "ho"
 *
 */
export class Splitter {

  constructor(document = document, Node = Node) {
    this.document = document;
    this.Node = Node;
  }

  static REAL_SPAN = 'izuna-char';
  static FAKE_SPAN = 'izuna-fake-char';

  createSpan(spanType, filePath, location, lineState, lineNumber, columnNumber, textContent) {
    var newChar = this.document.createElement("span");
    newChar.setAttribute('class', spanType);
    newChar.setAttribute('data-file-path', filePath);
    newChar.setAttribute('data-location', location);
    newChar.setAttribute('data-state', lineState);
    newChar.setAttribute('data-row', lineNumber);
    newChar.setAttribute('data-col', columnNumber);
    if(textContent) {
      newChar.textContent = textContent;
    }
    return newChar;
  }

  split(docFragment, dom, filePath, location, lineState, lineNumber) {
    const buildSpans = (node, charPos) => {
      if(node.nodeType === this.Node.TEXT_NODE) {
        var parent = this.document.createElement("span");
        const characterNodes = Array.from(node.textContent).map((character) => {
          const realSpan = this.createSpan(Splitter.REAL_SPAN, filePath, location, lineState, lineNumber, charPos, character);
          charPos = charPos + 0.5;
          const fakeSpan = this.createSpan(Splitter.FAKE_SPAN, filePath, location, lineState, lineNumber, charPos, null);
          charPos = charPos + 0.5;
          return [realSpan, fakeSpan];
        });

        characterNodes.forEach((characterNodes) => {
          const [realSpan, fakeSpan] = characterNodes;
          parent.appendChild(realSpan);
          parent.appendChild(fakeSpan);
        });

        return [parent, charPos];
      } else {
        Array.from(node.childNodes).forEach((child) => {
          const [newChild, newCharPos] = buildSpans(child, charPos)
          charPos = newCharPos;
          child.replaceWith(newChild)
        });

        return [node, charPos];
      }
    }
    var [newNode, _] = buildSpans(dom.cloneNode(true), 0);
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
