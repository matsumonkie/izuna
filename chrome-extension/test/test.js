const equal = require('chai').assert.equal;
import { Splitter } from '../src/splitter.js';
require('jsdom-global')();

describe('Splitter', function() {
  describe('#split()', function() {
    it('foo', function() {
      var dom = document.createElement("span");
      dom.setAttribute('class', 'whatever');
      dom.setAttribute('data-code-marker', '+');
      dom.textContent = 'hey';
      const splitter = new Splitter(document, Node);
      const splitted = splitter.split(new DocumentFragment(), dom, 'someFilePath', 'ADDED', 1)

      /*
      splitter.show(dom)
      console.log("\n###\n");
      splitter.show(splitted)
      */

      equal(true, true);
    });

    /*it('foo', function() {
      const dom = new JSDOM(`<span class="blob-code-inner blob-code-marker" data-code-marker="+"><span class="pl-en x x-first x-last">hey</span> <span class="pl-k">::</span> <span class="pl-en"><span class="pl-c1">String</span></span></span>`).window.document.querySelector("span");

      const splitter = new Splitter(new JSDOM().window.document, new JSDOM().window.Node);
      const splitted = splitter.split(JSDOM.fragment(), dom, 1)
      /*
      splitter.show(dom, "")
      console.log("\n###\n");
      splitter.show(splitted, "")
      * /

      equal(true, true);
    });*/
  });
});
